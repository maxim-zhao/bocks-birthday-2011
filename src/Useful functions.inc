

.define VDPAddress      $bf ; w
.define VDPData         $be ; w
.define VDPStatus       $bf ; r
.define PSG             $7f ; w
.define FMAddress       $f0 ; w
.define FMData          $f1 ; w
.define FMDetect        $f2 ; r/w
.define MemoryControl   $3e ; w
.define IOControl       $3f ; w
.define IOPort1         $dc ; r/w
.define IOPort2         $dd ; r/w
; Other:
.define SRAMPaging      $fffc ; r/w
.define SRAMPagingOn    $08
.define SRAMPagingOff   $80
.define Frame2Paging    $ffff ; r/w
.define TileMapAddress     $3800 | $4000 ; ORed with $4000 for setting VRAM address
.define SpriteTableAddress $3f00 | $4000
.define PaletteAddress     $0000 | $c000
; VDP registers
.enum $80
VDPReg_0                 db ; Misc
VDPReg_1                 db ; Misc
VDPRegTileMapAddress     db
VDPReg_3                 db ; Unused
VDPReg_4                 db ; Unused
VDPRegSpriteTableAddress db
VDPRegSpriteTileSet      db
VDPRegBorderColour       db
VDPRegHScroll            db
VDPRegVScroll            db
VDPRegLineInt            db
.ende
.define P1U %00000001
.define P1D %00000010
.define P1L %00000100
.define P1R %00001000
.define P11 %00010000
.define P12 %00100000
.define P2U %01000000
.define P2D %10000000
.define P2L %00000001
.define P2R %00000010
.define P21 %00000100
.define P22 %00001000
.define ResetButton %00010000
; colours
.enum $00
cl000 db
cl100 db
cl200 db
cl300 db
cl010 db
cl110 db
cl210 db
cl310 db
cl020 db
cl120 db
cl220 db
cl320 db
cl030 db
cl130 db
cl230 db
cl330 db
cl001 db
cl101 db
cl201 db
cl301 db
cl011 db
cl111 db
cl211 db
cl311 db
cl021 db
cl121 db
cl221 db
cl321 db
cl031 db
cl131 db
cl231 db
cl331 db
cl002 db
cl102 db
cl202 db
cl302 db
cl012 db
cl112 db
cl212 db
cl312 db
cl022 db
cl122 db
cl222 db
cl322 db
cl032 db
cl132 db
cl232 db
cl332 db
cl003 db
cl103 db
cl203 db
cl303 db
cl013 db
cl113 db
cl213 db
cl313 db
cl023 db
cl123 db
cl223 db
cl323 db
cl033 db
cl133 db
cl233 db
cl333 db
.ende


;==============================================================
; Set up VDP registers (default values)
;==============================================================
; Call DefaultInitialiseVDP to set up VDP to default values.
; Also defines NameTableAddress, SpriteTableAddress and SpriteSet
; which can be used elsewhere.
; To change the values used, copy and paste the modified data
; and code into the main source. Data is commented to help.
;==============================================================
.section "Initialise VDP to defaults" free
DefaultInitialiseVDP:
    push hl
    push bc
        ld hl,_Data
        ld b,_End-_Data
        ld c,$bf
        otir
    pop bc
    pop hl
    ret

.define SpriteSet           1       ; 0 for sprites to use tiles 0-255, 1 for 256+

_Data:
    .db %00100110,VDPReg_0
    ;    |||||||`- Disable sync
    ;    ||||||`-- Enable extra height modes
    ;    |||||`--- SMS mode instead of SG
    ;    ||||`---- Shift sprites left 8 pixels
    ;    |||`----- Enable line interrupts
    ;    ||`------ Blank leftmost column for scrolling
    ;    |`------- Fix top 2 rows during horizontal scrolling
    ;    `-------- Fix right 8 columns during vertical scrolling
    .db %10000100,VDPReg_1
    ;     |||| |`- Zoomed sprites -> 16x16 pixels
    ;     |||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
    ;     |||`---- 30 row/240 line mode
    ;     ||`----- 28 row/224 line mode
    ;     |`------ Enable VBlank interrupts
    ;     `------- Enable display
    .db (TileMapAddress>>10)   |%11110001,VDPRegTileMapAddress
    .db (SpriteTableAddress>>7)|%10000001,VDPRegSpriteTableAddress
    .db (SpriteSet<<2)         |%11111011,VDPRegSpriteTileSet
    .db $1|$f0,VDPRegBorderColour
    ;    `-------- Border palette colour (sprite palette)
    .db $00,VDPRegHScroll
    ;    ``------- Horizontal scroll
    .db $00,VDPRegVScroll
    ;    ``------- Vertical scroll
    .db $ff,VDPRegLineInt
    ;    ``------- Line interrupt spacing ($ff to disable)
_End:
.ends

;==============================================================
; Clear VRAM
;==============================================================
; Sets all of VRAM to zero
;==============================================================
.section "Clear VRAM" free
ClearVRAM:
    push af
    push hl
        ld hl,$0000
        call VRAMToHL
        ; Output 16KB of zeroes
        ld hl, $4000    ; Counter for 16KB of VRAM
        ld a,$00        ; Value to write
      -:out ($be),a ; Output to VRAM address, which is auto-incremented after each write
        dec h
        jp nz,-
        dec l
        jp nz,-
    pop hl
    pop af
    ret
.ends


;==============================================================
; VRAM to HL
;==============================================================
; Sets VRAM write address to hl
;==============================================================
.section "VRAM to HL"
VRAMToHL:
    push af
        ld a,l
        out ($bf),a
        ld a,h
        or $40
        out ($bf),a
    pop af
    ret
.ends

;==============================================================
; Load palette
;==============================================================
; Parameters:
; hl = location
; b  = number of values to write
; c  = palette index to start at (<32)
;==============================================================
.section "Load palette" free
LoadPalette:
	push af
	push bc
	push hl
	    ld a,c
	    out ($bf),a     ; Palette index
	    ld a,$c0
	    out ($bf),a     ; Palette write identifier
	    ld c,$be
	    otir            ; Output b bytes starting at hl to port c
	pop hl
	pop bc
	pop af
    ret
.ends

;==============================================================
; Tile loader
;==============================================================
; Parameters:
; hl = tile number to start at
; ix = location of tile data
; bc = No. of tiles to load
; d  = bits per pixel
;==============================================================
.section "Tile loader" FREE
LoadTiles:
    push af
    push bc
    push de
    push hl
    push ix
        ; Tell VDP where I want to write (hl<<5)
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,hl
        ld a,l
        out ($bf),a
        ld a,h
        or $40
        out ($bf),a

        ; I need to output bc*8 bytes so I need to modify bc (<<3)
        sla c
        rl b
        sla c
        rl b
        sla c
        rl b

        ; Write data
        _Loop:
            ; Restore counter
            ld e,d

            _DataLoop:
                ; Write byte
                ld a,(ix+0)
                out ($be),a
                dec e
                inc ix
                jp nz,_DataLoop

            ; Restore counter
            ld e,d
            _BlankLoop:
                ; Write blank data to fill up the rest of the tile definition
                inc e
                ld a,e
                cp 5
                jp z,_NoMoreBlanks

                ld a,0
                out ($be),a
                jp _BlankLoop

            _NoMoreBlanks:

            dec bc
            ld a,b
            or c
            jp nz,_Loop

    pop ix
    pop hl
    pop de
    pop bc
    pop af
    ret
.ends

;==============================================================
; V Counter reader
; Waits for 2 consecutive identical values (to avoid garbage)
; Returns in a *and* b
;==============================================================
.section "Get VCount" free
GetVCount:
    in a,($7e)  ; get VCount
  -:ld b,a      ; store it
    in a,($7e)  ; and again
    cp b        ; Is it the same?
    jp nz,-     ; If not, repeat
    ret         ; If so, return it in a (and b)
.ends

.section "Wait for VBlank without interrupts" free
WaitForVBlankNoInt:
    push bc
    push af
      -:call GetVCount
        cp 192
        jp nz,-
    pop af
    pop bc
    ret
.ends












;==============================================================
; Write ASCII text pointed to by hl to VRAM
; Stops when it finds a null byte, skips control characters,
; understands \n
; Pass name table address in iy, it will be modified
;==============================================================
.section "Write ASCII" FREE
VRAMToIY:
    push hl
    push iy
    pop hl
    call VRAMToHL
    pop hl
    ret

WriteASCII:
    push af
    push bc
    push hl
        call VRAMToIY
    	_WriteTilesLoop:
		    ld a,(hl)	; Value to write
		    cp $00		; compare a with $00, set z flag if they match
		    jp z,_WriteTilesLoopEnd	; if so, it's the string end so stop writing it
		    cp 10		; Check for LF
		    jp z,_NewLine
		    sub $20
            jp c,_SkipControlChar
		    out ($BE),a	; Output to VRAM address, which is auto-incremented after each write
		    ld a,%00000000
		    push hl
		    pop hl  ; delay
		    out ($BE),a
            _SkipControlChar:
		    inc hl
		    jp _WriteTilesLoop
	    _NewLine:
		    ; Go to the next line, ie. next multiple of 32=$20
            push hl
                push iy
                pop hl
                ld bc,64
                add hl,bc
                push hl
                pop iy
                call VRAMToIY
            pop hl
            _NoNewLine:
		    inc hl
		    jp _WriteTilesLoop
	    _WriteTilesLoopEnd:
    pop hl
    pop bc
    pop af
    ret
.ends

;==============================================================
; Number writer
; Writes hex byte in a to the screen
;==============================================================
.section "Write a to screen" FREE
WriteNumber:    ; writes hex byte in a to VRAM
    push af
    push bc
        ld b,a      ; back up a
        ; Strip to digits:
        ; Digit 1
        srl a
        srl a
        srl a
        srl a
        call WriteDigit
        ; Digit 2
        ld a,b
        and $f
        call WriteDigit
    pop bc
    pop af
    ret
WriteNumberEx:    ; writes the hex byte in a in a position unique to its value
    push bc
    push hl
        ld b,$00
        ld c,a
        ld hl,NameTableAddress
        add hl,bc
        add hl,bc
        add hl,bc
        add hl,bc
        call WriteNumber
    pop hl
    pop bc
    ret

WriteDigit:     ; writes the digit in a
    cp $0a      ; compare it to A - if it's less then it's 0-9
    jp c,IsNum
        add a,$07   ; if it's >9 then make it point at A-F
    IsNum:
    add a,$10

    out ($BE),a ; Output to VRAM address, which is auto-incremented after each write
    ld a,%00000000
    out ($BE),a
    ret

WriteSpace:
    push af
        ld a,0
        out ($be),a
        out ($be),a
    pop af
    ret
.ends

.section "PAL/NTSC detection" FREE
;==============================================================
; Speed detector
; Returns a=1 for PAL, 0 for NTSC
; MUST have standard screen size (not stretched)
;==============================================================
IsPAL:
    push bc
        call _GetNumber
        ld bc,$02d2
        sbc hl,bc       ; halfway between lowest and highest values found :P
    pop bc
    jp c,_IsNTSC
    ld a,1
    ret
    _IsNTSC:
    ld a,0
    ret

_GetNumber: ; returns counter in hl
    _WaitForLine1:
        call GetVCount
        cp $01
        jp nz,_WaitForLine1

    ; Line 1, let's start counting while checking for line 0
    ld hl,$0000
    _WaitForLine0:
        inc hl
        call GetVCount
        cp $00
        jp nz,_WaitForLine0
    ret
.ends
/*
.section "Turn off screen" FREE
TurnOffScreen:
    push af
        ld a,%10000000  ; 28 line mode
        out ($bf),a
        ld a,$81
        out ($bf),a
    pop af
    ret
.ends
*/
;==============================================================
; Sprite disabler
; Sets all sprites' Y-position to 208, thereby stopping display
; of any more sprites than I've used
;==============================================================
.section "No sprites" FREE
NoSprites:
    push af
    push bc
    push hl
        ld bc,64    ; how many sprites
        ld hl,SpriteTableAddress
        call VRAMToHL
      -:ld a,208+16 ; for 28-line mode
        out ($be),a
        dec bc
        ld a,b
        or c
        jp nz,-
    pop hl
    pop bc
    pop af
    ret
.ends

