'Made available under the MIT license, see LICENSE for details

'Set this to -1 to enable compile in console functionality, 0 to exclude it (only available on Linux)
$LET CONSOLE = -1

DEFLNG A-Z
DIM SHARED CursorX, CursorY, Blinkrate, CursorActive, CursorShape, CONSOLE

CHDIR _STARTDIR$

$IF CONSOLE AND LINUX THEN
    $CONSOLE
    $SCREENHIDE
    $IF 32BIT THEN
        DECLARE LIBRARY "i386-linux-gnu/ncurses"
        END DECLARE
    $ELSE
        DECLARE LIBRARY "x86_64-linux-gnu/ncurses"
        END DECLARE
    $END IF
    DECLARE LIBRARY "../programs_qb/headers/stdiox"
        SUB initx
        SUB printx (s$)
        SUB printstringx (BYVAL x&, BYVAL y&, s$)
        FUNCTION acs& (BYVAL cp437byte&)
        SUB displayx
        SUB locatex (BYVAL row&, BYVAL col&)
        SUB colorx (BYVAL fg&, BYVAL bg&)
        FUNCTION maxcol&
        FUNCTION maxrow&
        FUNCTION screenx& (BYVAL row&, BYVAL col&)
        FUNCTION csrlinx&
        FUNCTION posx&
        FUNCTION inkeyx (extended&)
        SUB showcursor
        SUB hidecursor
        SUB clsx ()
        SUB finishx
    END DECLARE
    _DEST _CONSOLE
$END IF

'Parse command line
IF _COMMANDCOUNT > 0 THEN
    $IF CONSOLE AND LINUX THEN
        IF _COMMANDCOUNT > 2 THEN PRINT COMMAND$(0); ": [--console] [<file>]": SYSTEM
    $ELSE
        IF _COMMANDCOUNT > 1 THEN PRINT COMMAND$(0); ": [<file>]": SYSTEM
    $END IF

    FOR i = 1 TO _COMMANDCOUNT
        IF COMMAND$(i) = "--console" OR COMMAND$(i) = "-c" THEN
            CONSOLE = -1
        ELSE
            IF inputfile$ <> "" THEN PRINT "Bad command line": SYSTEM
            inputfile$ = COMMAND$(i)
        END IF
    NEXT i

END IF

IF CONSOLE THEN
    $IF CONSOLE AND LINUX THEN
        initx
    $END IF
ELSE
    _SCREENSHOW
    _DEST 0
END IF

lines = 22
DIM contents AS _MEM
DIM newmem AS _MEM 'Used for temporary allocations (for resizing)
CursorShape = 1
Blinkrate = 500
CursorActive = -1

GOSUB redraw
GOTO initial_load

DO
    DO
        k$ = inkeyy$
        _LIMIT 30
    LOOP WHILE k$ = ""
    SELECT CASE k$
        CASE "0" TO "9", "A" TO "F", "a" TO "f"
            changed = -1
            IF editmode = -1 THEN
                GOSUB textinsert
            ELSE
                IF cursornybble = 0 THEN
                    _MEMPUT contents, contents.OFFSET + cursoroff, VAL("&H" + k$) * &H10 + (_MEMGET(contents, contents.OFFSET + cursoroff, _UNSIGNED _BYTE) AND &HF) AS _UNSIGNED _BYTE
                    cursornybble = 1
                ELSE
                    _MEMPUT contents, contents.OFFSET + cursoroff, VAL("&H" + k$) + (_MEMGET(contents, contents.OFFSET + cursoroff, _UNSIGNED _BYTE) AND &HF0) AS _UNSIGNED _BYTE
                    cursornybble = 0
                    IF cursoroff < contents.SIZE - 1 THEN
                        cursoroff = cursoroff + 1
                        IF cursoroff MOD 16 = 0 THEN
                            IF scrline = lines THEN foffset = foffset + 16 ELSE scrline = scrline + 1
                        END IF
                    END IF
                END IF
                GOSUB redraw
            END IF
        CASE CHR$(32) TO CHR$(126)
            IF editmode = -1 THEN GOSUB textinsert: changed = -1
        CASE CHR$(0) + CHR$(73) 'PgUp
            cursornybble = 0
            IF foffset < 16 * lines THEN
                foffset = 0
                cursoroff = 0
                scrline = 1
            ELSE
                foffset = foffset - 16 * lines
                cursoroff = cursoroff - 16 * lines
            END IF
            GOSUB redraw
        CASE CHR$(0) + CHR$(81) 'PgDn
            IF foffset + 16 * lines < contents.SIZE - 1 AND foffset + 16 * lines * 2 > contents.SIZE - 1 THEN
                foffset = foffset + 16 * lines
                IF cursoroff + 16 * lines > contents.SIZE - 1 THEN cursoroff = o2l~&&(contents.SIZE) - 1: scrline = (cursoroff - foffset) \ 16 + 1 ELSE cursoroff = cursoroff + 16 * lines
                cursornybble = 0
            ELSEIF foffset + 16 * lines * 2 < contents.SIZE - 1 THEN
                foffset = foffset + 16 * lines
                cursoroff = cursoroff + 16 * lines
                cursornybble = 0
            END IF
            GOSUB redraw
        CASE CHR$(0) + CHR$(77) 'right
            IF cursoroff < contents.SIZE - 1 THEN
                cursornybble = 0
                cursoroff = cursoroff + 1
                IF cursoroff MOD 16 = 0 THEN
                    IF scrline = lines THEN foffset = foffset + 16 ELSE scrline = scrline + 1
                END IF
                GOSUB redraw
            END IF
        CASE CHR$(0) + CHR$(72) 'up
            IF cursoroff >= 16 THEN
                cursornybble = 0
                cursoroff = cursoroff - 16
                IF scrline = 1 THEN foffset = foffset - 16 ELSE scrline = scrline - 1
                GOSUB redraw
            END IF
        CASE CHR$(0) + CHR$(75) 'left
            IF cursoroff >= 1 THEN
                cursornybble = 0
                IF cursoroff MOD 16 = 0 THEN
                    IF scrline = 1 THEN foffset = foffset - 16 ELSE scrline = scrline - 1
                END IF
                cursoroff = cursoroff - 1
                GOSUB redraw
            END IF
        CASE CHR$(0) + CHR$(80) 'down
            IF cursoroff + 16 < contents.SIZE THEN
                cursornybble = 0
                cursoroff = cursoroff + 16
                IF scrline = lines THEN foffset = foffset + 16 ELSE scrline = scrline + 1
                GOSUB redraw
            END IF
        CASE CHR$(0) + CHR$(83) 'delete
            IF cursoroff <= contents.SIZE - 1 THEN
                count = 1
                GOSUB deletebytes
                changed = -1
                IF cursoroff >= contents.SIZE AND cursoroff <> 0 THEN cursoroff = o2l~&&(contents.SIZE) - 1
                GOSUB redraw
            END IF
        CASE CHR$(0) + CHR$(59) 'F1 Calculator
        CASE CHR$(0) + CHR$(60) 'F2 Search
            'IF editmode = 0 THEN
            '    hexneedle$ = prompt_input$("FIND HEX [" + recenthexsearch$ + "]: ")
            '    IF needle$ = CHR$(27) THEN needle$ = recenthexsearch$ ELSE recenthexsearch$ = needle$
            '    for
        CASE CHR$(0) + CHR$(61) 'F3 Search&Replace
        CASE CHR$(0) + CHR$(62) 'F4 Insert
            count = VAL(prompt_input$("INSERT Num bytes (&H/&O/&B for non-decimal): "))
            newmem = _MEMNEW(contents.SIZE + count)
            IF newmem.SIZE <> contents.SIZE + count THEN
                clear_bottom
                locatey 25, 1
                printy "Allocation failed, data unchanged. Press any key."
                displayy
                DO
                    _LIMIT 20
                    k$ = inkeyy$
                LOOP WHILE k$ = ""
            ELSE
                _MEMCOPY contents, contents.OFFSET, cursoroff TO newmem, newmem.OFFSET
                _MEMFILL newmem, newmem.OFFSET + cursoroff, count, 0 AS _BYTE
                _MEMCOPY contents, contents.OFFSET + cursoroff, contents.SIZE - cursoroff TO newmem, newmem.OFFSET + cursoroff + count
                _MEMFREE contents
                contents = newmem
                changed = -1
            END IF
            GOSUB redraw
        CASE CHR$(0) + CHR$(63) 'F5 Append
            count = VAL(prompt_input$("APPEND Num bytes (&H/&O/&B for non-decimal): "))
            newmem = _MEMNEW(contents.SIZE + count)
            IF newmem.SIZE <> contents.SIZE + count THEN
                clear_bottom
                locatey 25, 1
                printy "Allocation failed, data unchanged. Press any key."
                displayy
                DO
                    _LIMIT 20
                    k$ = inkeyy$
                LOOP WHILE k$ = ""
            ELSE
                _MEMCOPY contents, contents.OFFSET, contents.SIZE TO newmem, newmem.OFFSET
                _MEMFILL newmem, newmem.OFFSET + contents.SIZE, count, 0 AS _BYTE
                _MEMFREE contents
                contents = newmem
                changed = -1
            END IF
            GOSUB redraw
        CASE CHR$(0) + CHR$(64) 'F6 Delete
            count = VAL(prompt_input$("DELETE Num bytes (&H/&O/&B for non-decimal): "))
            IF count > contents.SIZE THEN
                clear_bottom
                locatey 25, 1
                printy "Count bigger than file, data unchanged. Press any key."
                displayy
                DO
                    _LIMIT 20
                    k$ = inkeyy$
                LOOP WHILE k$ = ""
            ELSE
                GOSUB deletebytes
                changed = -1
            END IF
            IF cursoroff >= contents.SIZE AND cursoroff <> 0 THEN cursoroff = o2l~&&(contents.SIZE) - 1
            GOSUB redraw
        CASE CHR$(0) + CHR$(65) 'F7 Goto
            p = VAL(prompt_input$("GOTO Offset (&H/&O/&B for non-decimal): "))
            IF p > contents.SIZE - 1 THEN p = o2l~&&(contents.SIZE) - 1
            cursoroff = p
            foffset = (cursoroff \ 16) * 16
            scrline = 1
            GOSUB redraw
        CASE CHR$(0) + CHR$(66) 'F8 Write
            IF changed THEN
                write_file inputfile$, contents
                changed = 0
                GOSUB redraw
            END IF
        CASE CHR$(0) + CHR$(67) 'F9 Open
            initial_load:
            IF changed THEN GOSUB promptsave
            changed = 0
            IF inputfile$ = "" THEN inputfile$ = prompt_input$("File name: ")
            IF _MEMEXISTS(contents) THEN _MEMFREE contents
            read_file inputfile$, contents
            changed = 0 'becomes 0 when saved and -1 when changed
            foffset = 0
            cursoroff = 0
            cursornybble = 0 'becomes 1 when on the second nybble
            editmode = 0 '0 = hex, -1 = text
            scrline = 1
            GOSUB redraw
        CASE CHR$(0) + CHR$(68) 'F10
        CASE CHR$(0) + CHR$(133) 'F11
        CASE CHR$(0) + CHR$(134) 'F12


        CASE CHR$(27) 'escape
            IF changed THEN GOSUB promptsave
            $IF CONSOLE AND LINUX THEN
                IF CONSOLE THEN finishx
            $END IF
            SYSTEM
        CASE CHR$(9) 'tab
            cursornybble = 0
            editmode = NOT editmode
            GOSUB redraw
    END SELECT
LOOP

deletebytes:
newmem = _MEMNEW(contents.SIZE - count)
_MEMCOPY contents, contents.OFFSET, cursoroff TO newmem, newmem.OFFSET
_MEMCOPY contents, contents.OFFSET + cursoroff + count, contents.SIZE - cursoroff - count TO newmem, newmem.OFFSET + cursoroff
_MEMFREE contents
contents = newmem
RETURN

promptsave:
clear_bottom
locatey 25, 1
printy "Current file changed. Save (y/n)? "
displayy
DO
    k$ = UCASE$(INKEY$)
LOOP WHILE k$ <> "Y" AND k$ <> "N"
IF k$ = "Y" THEN write_file inputfile$, contents
GOSUB redraw
RETURN

textinsert:
_MEMPUT contents, contents.OFFSET + cursoroff, ASC(k$) AS _UNSIGNED _BYTE
IF cursoroff < contents.SIZE - 1 THEN
    cursoroff = cursoroff + 1
    IF cursoroff MOD 16 = 0 THEN
        IF scrline = lines THEN foffset = foffset + 16 ELSE scrline = scrline + 1
    END IF
    GOSUB redraw
END IF
GOSUB redraw
RETURN

redraw:
clsy
printy "[" + hexpad$(cursoroff, 8) + "/" + hexpad$(o2l~&&(contents.SIZE), 8) + "]"
IF changed THEN printy "!"
printy " " + inputfile$
locatey csrliny + 1, 1
FOR lstart = foffset TO foffset + 16 * lines - 1 STEP 16
    IF lstart + 16 > contents.SIZE THEN count = o2l~&&(contents.SIZE) - lstart ELSE count = 16
    IF count > 0 THEN
        bytes$ = SPACE$(count)
        _MEMGET contents, contents.OFFSET + lstart, bytes$
        printy hexpad$(lstart, 8) + "|"
        FOR byte = 1 TO count
            IF byte = 9 THEN printy "  "
            locatey csrliny, posy + 1
            IF lstart + byte - 1 = cursoroff AND editmode = 0 THEN
                CursorY = csrliny
                CursorX = posy + cursornybble
            END IF
            printy hexpadb$(ASC(bytes$, byte), 2)
        NEXT byte
        locatey csrliny, 60
        printy " | "
        FOR byte = 1 TO count
            b = ASC(bytes$, byte)
            IF lstart + byte - 1 = cursoroff AND editmode = -1 THEN CursorY = csrliny: CursorX = posy
            IF b >= 32 AND b <= 126 THEN printy CHR$(b) ELSE printy "." 'CHR$(249)
        NEXT byte
        locatey csrliny + 1, 1
    ELSE
        printy hexpad$(lstart, 8) + "|"
        locatey csrliny + 1, 1
    END IF
NEXT lstart
locatey 25, 1
printy "F4=INSERT  F5=APPEND  F6=DELETE  F7=GOTO  F8=SAVE  F9=OPEN"
$IF CONSOLE AND LINUX THEN
    IF CONSOLE THEN locatex CursorY, CursorX
$END IF
displayy
RETURN

SUB clear_bottom
locatey 25, 1
printy SPACE$(79)
displayy
END SUB

FUNCTION hexpad$ (n, p)
n$ = HEX$(n)
hexpad$ = SPACE$(p - LEN(n$)) + n$
END FUNCTION

FUNCTION hexpadb$ (n, p)
n$ = HEX$(n)
IF LEN(n$) MOD 2 THEN n$ = "0" + n$
hexpadb$ = SPACE$(p - LEN(n$)) + n$
END FUNCTION

SUB read_file (f$, m AS _MEM)
OPEN f$ FOR BINARY AS #1
l = LOF(1)
temp$ = SPACE$(l)
GET #1, 1, temp$
CLOSE #1
m = _MEMNEW(l)
_MEMPUT m, m.OFFSET, temp$
END SUB

SUB write_file (f$, m AS _MEM)
IF _FILEEXISTS(f$) THEN KILL f$
OPEN f$ FOR BINARY AS #1
temp$ = SPACE$(o2l~&&(m.SIZE))
_MEMGET m, m.OFFSET, temp$
PUT #1, , temp$
END SUB

$IF CONSOLE AND LINUX THEN
    SUB printy (s$)
    IF CONSOLE THEN printx s$ + CHR$(0) ELSE PRINT s$;
    END SUB

    SUB locatey (r, c)
    IF CONSOLE THEN locatex r, c ELSE LOCATE r, c
    END SUB

    FUNCTION csrliny
    IF CONSOLE THEN csrliny = csrlinx ELSE csrliny = CSRLIN
    END FUNCTION

    FUNCTION posy
    IF CONSOLE THEN posy = posx ELSE posy = POS(0)
    END FUNCTION

    SUB colory (f, b)
    IF CONSOLE THEN colorx f, b ELSE COLOR f, b
    END SUB


    SUB clsy
    IF CONSOLE THEN clsx ELSE CLS
    END SUB

    SUB displayy
    IF CONSOLE THEN displayx ELSE _DISPLAY
    END SUB

    FUNCTION inkeyy$
    IF CONSOLE THEN
        b1 = inkeyx(extended)
        s$ = CHR$(b1)
        IF extended <> 0 THEN s$ = s$ + CHR$(extended)
    ELSE
        s$ = INKEY$
    END IF
    inkeyy$ = s$
    END FUNCTION
$ELSE
    SUB printy (s$)
    PRINT s$;
    END SUB

    SUB locatey (r, c)
    LOCATE r, c
    END SUB

    FUNCTION csrliny
    csrliny = CSRLIN
    END FUNCTION

    FUNCTION posy
    posy = POS(0)
    END FUNCTION

    SUB colory (f, b)
    COLOR f, b
    END SUB


    SUB clsy
    CLS
    END SUB

    SUB displayy
    _DISPLAY
    END SUB

    FUNCTION inkeyy$
    inkeyy$ = INKEY$
    END FUNCTION
$END IF


SUB _GL
STATIC blinker!, showing
IF TIMER - blinker! >= Blinkrate / 1000 THEN showing = NOT showing: blinker! = TIMER
IF showing AND CursorActive THEN
    cy = CursorY
    cx = CursorX
    fh = _FONTHEIGHT
    fw = _FONTWIDTH

    'For SCREEN 0:
    h = _HEIGHT
    w = _WIDTH

    'For all other screen modes:
    'h = _HEIGHT / fh
    'w = _WIDTH / fw

    _glMatrixMode _GL_PROJECTION
    _glLoadIdentity
    _glOrtho 0, w * fw, 0, h * fh, 0, -1
    x = (cx - 1) * fw
    SELECT CASE CursorShape
        CASE 0
            'Left vertical bar
            _glBegin _GL_LINES
            _glVertex2i x + 1, (h - cy) * fh
            _glVertex2i x + 1, (h - cy + 1) * fh
        CASE 1
            'Underline
            _glBegin _GL_LINES
            _glVertex2i x, (h - cy) * fh
            _glVertex2i x + fw, (h - cy) * fh
        CASE 2
            'Box
            _glBegin _GL_QUADS
            _glVertex2i x, (h - cy) * fh
            _glVertex2i x, (h - cy + 1) * fh
            _glVertex2i x + fw, (h - cy + 1) * fh
            _glVertex2i x + fw, (h - cy) * fh
    END SELECT
    _glEnd
END IF
END SUB

'Only works on little-endian machines (and big-endian ones where len(x~&&) = len(x%&))
FUNCTION o2l~&& (o~%&)
STATIC m AS _MEM
IF _MEMEXISTS(m) = 0 THEN m = _MEMNEW(LEN(dummy~&&))
_MEMPUT m, m.OFFSET, o~%&
o2l~&& = _MEMGET(m, m.OFFSET, _UNSIGNED _INTEGER64)
END FUNCTION

'This routine based on one thanks to Steve!
FUNCTION prompt_input$ (prompt$)
clear_bottom
x = LEN(prompt$) + 1
DO
    _LIMIT 30
    l = 79 - x
    p = 1 - l + LEN(temp$)
    IF p < 1 - l THEN p = 1 - l
    locatey 25, 1
    printy prompt$ + t$ + " "
    displayy
    k$ = inkeyy$
    SELECT CASE k$
        CASE CHR$(8): temp$ = LEFT$(temp$, LEN(temp$) - 1)
        CASE CHR$(13): prompt_input$ = temp$: EXIT FUNCTION
        CASE CHR$(32) TO CHR$(126): temp$ = temp$ + k$
    END SELECT
    t$ = MID$(temp$, p, l)
LOOP
END FUNCTION

