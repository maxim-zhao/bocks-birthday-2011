; TODO:
; - save RAM for high score
; - clouds
; - stolen music/SFX? Would be nice to get some speed-adjustable music

;==============================================================
; WLA-DX banking setup
;==============================================================
.memorymap
defaultslot 0
slotsize $7ff0 ; ROM
slot 0 $0000
slotsize $0010 ; header
slot 1 $7ff0
slotsize $4000 ; ROM
slot 2 $8000
slotsize $2000 ; RAM
slot 3 $c000
.endme

.rombankmap
bankstotal 2
banksize $7ff0
banks 1
banksize $0010
banks 1
.endro

.org 0
.enum $c000 export
Port3EVal                              db
Paused                                 db
PSGDecoderBuffer                       dsb 34 ; must stay at $c002! see later
VBlankRoutine                          dw
CurrentlyPressedButtons                db
JustPressedButtons                     db

PaletteFadeControl                     db       ; bit 7 1 = fade in, 0 = fade out
                                                ;       lower bits = counter, should go 9->0
                                                ;       Must be followed by:
PaletteSize                            db       ; counter for number of entries in TargetPalette
PaletteFadeFrameCounter                db       ; Palette fade frame counter
ActualPalette                          dsb 32   ; 32 bytes Actual palette (when fading)
TargetPalette                          dsb 32   ; 32 bytes Target palette
CurrentScroll                          dw       ; 8.8 fixed point
ScrollSpeedLow                         db       ; low byte
ScrollSpeed                            dw       ; 8.16 fixed point
LastScrollHighBits                     db       ; comparison for knowing when to paint
Acceleration                           dw       ; only the low part
CurrentPlatformLengthCounter           db       ; number of tiles remaining to be drawn in the current platform
CurrentPlatformHeight                  db       ; height in tiles of the current platform
NextPlatformHeight                     db       ; height in tiles of the next platform - so I can draw transitions properly
PlatformHeightBuffer                   dsb 32   ; platform heights of the last 32 drawn - for ground collision detection. Mustn't wrap low byte
DrawingTilesBuffer                     dsb 24*2*2 ; 2 columns
DrawingTilesPointer                    dw         ; pointer into it
DrawTilesFlag                          db       ; non-zero -> draw buffer at next vblank
DrawTilesVRAMStart                     dw       ; VRAM address to start at
GroundType                             db       ; while drawing, this tells us what sort of ground we're on
RandomNumberGeneratorWord              dw       ; RNG scratch
DistanceMeter                          dsb 7    ; 7 digits
HighScore                              dsb 7
SpriteTableMirror                      dsb 64+128
CurrentMessagePointer                  dw       ; message drawing
SpriteDistanceCounter                  db       ; sprite animation counter
PixelsScrolledThisFrame                db       ; for use with the above
SpriteAnimationCounter                 db       ; index into tables
CurrentY                               db       ; current sprite Y
ExpectedY                              db       ; sprite Y we're expecting for the current ground
IsJumping                              db       ; 0 = on ground 1 = jumping up 2 = falling down
JumpingIndex                           db       ; lookup into JumpingTable for jumping/falling
.ende

.asciitable
map ' ' to '~' = 0
.enda

.define STRING_TERMINATOR $ff

.define ERROR_RST $df ; rst $18
.macro ERROR
  .db ERROR_RST
.endm
.emptyfill ERROR_RST

.macro CallHL
  rst $10
.endm

.macro ldDEXY args X,Y
  ld de,TileMapAddress + (Y * 32 + X) * 2
.endm

.macro ldHLTileIndex args INDEX
  ld hl,$4000 + INDEX * 32
.endm

.define FastOtirBlockSize 256
.macro FastOtir args count
.ifgr count FastOtirBlockSize
  call outiblock
  FastOtir count-FastOtirBlockSize
.else
  call outiblock+(FastOtirBlockSize-count)*2
.endif
.endm

.macro SetVDPAddress args addr
  ld a,<addr ; WLA DX syntax: low byte of addr
  out (VDPAddress),a
  ld a,>addr ; WLA DX syntax: high byte of addr
  out (VDPAddress),a
.endm

.macro TEXT
.dw (\1 + \2 * 32) * 2 | TileMapAddress
.asc \3
.db STRING_TERMINATOR
.endm

.bank 0 slot 0

.include "Useful functions.inc"
.include "Phantasy Star decompressors.inc"
.include "Phantasy Star Gaiden decompressor.inc"
.sdsctag 1.00,"Bock's Birthday 2011 II: The Forgotten Subtitle",Comments,"Maxim"

.org 0
; standard startup
.section "Startup" force
di
im 1
ld sp, $dff0
jp main
.ends

.org $10 ; rst $10 = CallHL
.section "rst $10" force
CallHL:
  jp (hl)
DoNothing:
  ret ; could save a whole byte by reusing another innocent ret
.ends

.org $18 ; rst $18 = error
.section "$18 error stuff" force
  jp ErrorHandler
.ends

; space for more..?

.org $38
.section "Interrupt handler" force
  ex af,af'
  exx
    in a,(VDPStatus) ; satisfy interrupt
    ld hl,(VBlankRoutine)
    CallHL
  exx
  ex af,af'
  ei
  reti
.ends

.org $66
.section "Pause handler" force
NMI:
  ; just toggle a flag
  push af
    ld a,(Paused)
    xor 1
    ld (Paused),a
  pop af
  retn
.ends

.section "main" free
main:
  call TurnOffScreen
  ; clear RAM
  ld hl,$c001
  ld de,$c002
  ld bc,$dff0-$c002
  ld (hl),0
  ldir

/*  ; init paging - some emulators don't map all 48KB by default
  ld a,0
  ld hl,$fffd
  ld b,3
-:ld (hl),a
  inc a
  inc hl
  djnz -
*/
  ; blacken the palette
  SetVDPAddress PaletteAddress
  ld hl,$c001 ; good as anywhere
  ld c,VDPData
  FastOtir 32

  jp TitleScreen
.ends

.section "Title screen" free
TitleScreen:
  call TurnOffScreen
  di
  call DefaultInitialiseVDP
  call ClearVRAM
  call NoSprites

  ld ix,GameGraphics
  ld hl,$4000
  call PSG_decompress
  ld ix,GameSprites
  ld hl,$4000+256*32
  call PSG_decompress

  ld hl,GamePalette
  ld de,TargetPalette
  ld bc,16
  ldir
  ld hl,SpritePalette
  ld de,TargetPalette+16
  ld bc,16
  ldir

  ld de,TileMapAddress|$4000
  call SetVRAMAddressToDE
  ld hl,TitleScreenTiles
  ld c,VDPData
  FastOtir 32*24*2
  
  ; draw on high-score and last-score
  ldDEXY 16,14
  call SetVRAMAddressToDE
  ld hl,DistanceMeter
  ld b,7
-:ld a,(hl)
  call RenderNibble
  inc hl
  djnz -
  ldDEXY 16,15
  call SetVRAMAddressToDE
  ld hl,HighScore
  ld b,7
-:ld a,(hl)
  call RenderNibble
  inc hl
  djnz -

  call Initialise

  ld hl,DoNothing
  ld (VBlankRoutine),hl
  ei

  call GameVBlank ; to get other stuff in VRAM

  call TurnOnScreen

  call FadeIn

  ; I'm just using the same routine for both as it suffices...
  ld hl,GameVBlank
  ld (VBlankRoutine),hl

-:halt
  ld a,(JustPressedButtons)
  and P11|P12
  jp nz,Game

  ; randomise
  call GetRandomNumber

  jr -

.ends

.section "Title VBlank" free
TitleVBlank:
  ; VRAM
  ; Non-VRAM
  call GetInputs
;  call FadePaletteInRAM
  ret
.ends

.section "Init" free
Initialise:
  ; Set scroll speed
  ld hl,$0100 ; 1 pixel per frame = 60px/s
  ld (ScrollSpeed),hl
  xor a
  ld (ScrollSpeedLow),a
  ld hl,0
  ld (CurrentScroll),hl
  ld hl,$0010 ; 1/256 pixels per frame per frame
  ld (Acceleration),hl

  ; Initialise game engine stuff
  ld a,40
  ld (CurrentPlatformLengthCounter),a
  ld a,20
  ld (CurrentPlatformHeight),a
  ld bc,31
  ld hl,PlatformHeightBuffer
  ld de,PlatformHeightBuffer+1
  ld (hl),a
  ldir
  ld hl,TileMapAddress|$4000 + 1*2
  ld (DrawTilesVRAMStart),hl
  ld a,7 ~ $ff
  ld (LastScrollHighBits),a
  ld a,4
  ld (GroundType),a
  ld hl,Message
  ld (CurrentMessagePointer),hl
  ; zero the distance meter
  ld hl,DistanceMeter
  ld de,DistanceMeter+1
  ld bc,6
  ld (hl),0
  ldir

  ; set up the sprites
  ld hl,SpriteTableYInit
  ld de,SpriteTableMirror
  ld bc,64
  ldir
  ld hl,SpriteTableXNInit
  ld bc,128
  ldir

  ld a,128
  ld (CurrentY),a
  
  ; disable any tile buffer copying
  xor a
  ld (DrawTilesFlag),a

  ret
.ends

.section Game
Game:
  ; Main game non-vblank loop
----:
  halt

  ; Add acceleration onto the current speed
  ld de,(Acceleration)
  ld hl,(ScrollSpeed-1)
  add hl,de
  ld (ScrollSpeed-1),hl
  ld a,(ScrollSpeed+1)
  adc a,0
  ld (ScrollSpeed+1),a
  cp 8 ; 8px per frame = maximum
  jr nz,+
  ; limit it - truncate to exactly 8
  ld hl,0
  ld (ScrollSpeed-1),hl
+:

ScrollScreen:
  ; Scroll the screen
  ld de,(ScrollSpeed)   ; truncate to 8.8
  ld hl,(CurrentScroll) ; subtract from 8.8
  xor a                 ; reset carry
  sbc hl,de
  ; calculate the pixels moved for later
  ; pixels = (CurrentScroll) high - h
  ld a,(CurrentScroll+1)
  sub h
  ld (PixelsScrolledThisFrame),a
  ld (CurrentScroll),hl

CheckDraw:
  ; consider drawing something if the scroll has underflowed 8px
  ; h = pixels scrolled
  ; we want to check if it's passed a multiple of 8 since last time we checked
  ld a,h
  and 7 ~ $ff
  ld b,a
  ld a,(LastScrollHighBits)
  cp b
  jr z,+
  ld a,b
  ld (LastScrollHighBits),a
  call NextColumn
  call AddToDistance
+:

GetExpectedY:
  ; get min(+4 to +5)
  ld ix,PlatformHeightBuffer
  ld a,(ix+4)
  ld b,(ix+5)
;  ld c,(ix+6)
  cp b
  jr c,+
  ld a,b
;+:cp c
;  jr c,+
;  ld a,c
+:; that's in units of tiles from the top
  ; multiply by 8
  sla a
  sla a
  sla a
  ; subtract 32
  sub 32
  ; done
  ld (ExpectedY),a

CheckMotion:
  ld a,(IsJumping)
  or a
  jr z,_OnGround
  dec a
  jr z,_JumpingUp
_FallingDown:
  ; decrement the index
  ld a,(JumpingIndex)
  or a
  jr z,+
  dec a ; decrement if not zero
+:
_FallingDownImpl:
  ld (JumpingIndex),a
  ; look up delta
  ld l,a
  ld h,>JumpLookup
  ld b,(hl)
  ; add to Y
  ld a,(CurrentY)
  add a,b
  ; Is that past the ground?
  ld b,a
  ld a,(ExpectedY)
  cp b
  ; result: z = exactly hit ground: transition to walking, use a or b
  ;         nc = above ground: use b
  ;         c = below ground:
  ;               if too far, die
  ;               else use a and transition to walking
  jr z,_fallingOnGround
  jr nc,_fallingAboveGround
_fallingBelowGround:
  ; how far?
  sub b
  cp -4
  jp c,Dead
  add a,b ; get it back
  ld b,a ; in the right place

_fallingOnGround:
  ; transition to walking
  xor a
  ld (IsJumping),a
  ld a,b
  ; fall through
_fallingAboveGround:
  ; use b
  ld a,b
  ; fall through
  ld (CurrentY),a
  call AnimateSprite
  jr _CheckMotionEnd

_JumpingUp:
  ; Is the button still held?
  ld a,(CurrentlyPressedButtons)
  and P11|P12
  jr nz,+
  ; no: time to start falling
_StartToFall:
  ld a,2
  ld (IsJumping),a
  ld a,JumpLookupMax
  jr _FallingDownImpl
+:; yes: increment index
  ld a,(JumpingIndex)
  inc a
  ; is it the end?
  cp 30 ;JumpLookupMax+1
  jr z,_StartToFall ; yes: time to start falling
+:; no: use it
_JumpingUpImpl:
  ld (JumpingIndex),a
  ; look up delta
  ld l,a
  ld h,>JumpLookup
  ld b,(hl)
  ; subtract from y
  ld a,(CurrentY)
  sub b
  ; Did we go past 0?
  jr nc,+
  ; then we hit our head...
  xor a
  ld (CurrentY),a
  jr _StartToFall
+:ld (CurrentY),a
  call AnimateSprite
  jr _CheckMotionEnd

_OnGround:
  ld a,(ExpectedY)
  ; are we above or below it?
  ld b,a
  ld a,(CurrentY)
  sub b
  jr z,+ ; should be 0
  jp nc,Dead
  jr _StartToFall

+:
  ; Do we want to start a jump?
  ld a,(JustPressedButtons)
  and P11|P12 ; either button to jump
  jr z,+
  ld a,1 ; 1 = jumping up
  ld (IsJumping),a
  xor a ; a = 0 = jump lookup index
  jr _JumpingUpImpl
+:; check distance walked
  ; One frame every 9 pixels travelled matches the original game
  ld a,(PixelsScrolledThisFrame)
  ld b,a
  ld a,(SpriteDistanceCounter)
  sub b
  jr nc,+
  ; next sprite please
  push af
    call AnimateSprite
  pop af
  ; subtract from counter
  add a,9
+:ld (SpriteDistanceCounter),a
  ; jr _CheckMotionEnd ; fall through

_CheckMotionEnd:

CheckForDead:
  ; check we're above ground
  ld a,(CurrentY)
  ld b,a
  ld a,(ExpectedY)
  ; leniency
  add a,2
  sub b
  ; z = on ground
  ; c = below
  ; nc = above
  jp c,Dead

  ; Main game non-vblank loop end
  jp ----

NextColumn:
  ; Do we need to fill the buffer or just switch to the second column?
  ld a,(GroundType)
  or a
  jr z,_DrawLeft    ; 0 Draw from left buffer
  dec a
  jr z,_DrawRight   ; 1 Draw from right buffer
  dec a
  jp z,_Transition1 ; 2 Fill buffer with transition gfs and draw left
  dec a
  jp z,_Transition2 ; 3 Draw from right buffer

_StartFlat:         ; 4 Fill with flat ground and draw left
  ; Fill with all sky
  call _blankBuffer

  ; Seek to the right place to draw the ground in
  ld a,(CurrentPlatformHeight)
  sla a
  ld ix,DrawingTilesBuffer
  add a,ixl
  ld ixl,a
  ; now fill it in
  ld (ix+0),T_grassTop12
  ld (ix+2),T_grassTop3
  ld (ix+24*2),T_grassTop12
  ld (ix+24*2+2),T_grassTop4
  ; Now fill in the ground
  ; how many? (24-a-1)/2
  ld a,(CurrentPlatformHeight)
  add a,-23
  neg
  sra a
  jr z,+
  ld b,a
-:; draw 4 tiles
  inc ix
  inc ix
  inc ix
  inc ix
  ld (ix+0),T_ground1
  ld (ix+2),T_ground2
  ld (ix+24*2+0),T_ground2
  ld (ix+24*2+2),T_ground1
  djnz -
+:; done - fall through

_DrawLeft:
  ld hl,DrawingTilesBuffer
  ld (DrawingTilesPointer),hl
  ld a,1 ; draw right next
  ld (GroundType),a

  call DrawMessage

  jp _GroundDone

_DrawRight:
  ld hl,DrawingTilesBuffer+24*2
  ld (DrawingTilesPointer),hl

  ; Decrement the ground counter
  ; Time for a new height?
  ; decrement counter
  ld hl,CurrentPlatformLengthCounter
  dec (hl)
  jr nz,++
NewHeight:
  ; time for a new height and length
  ; height even number between $08 and $16, i.e. 8 levels
  call GetRandomNumber
  and $7
  sla a
  add a,8
  ; but is it too far?
  ld b,a
  ld a,(CurrentPlatformHeight)
  sub b
  jr c,+
  sub 8 ; maximum
  jr nc,NewHeight ; loop until it is acceptable
+:; save it
  ld a,b
  ld (NextPlatformHeight),a
  ; length 4-11 blocks
  call GetRandomNumber
  and $7
  add a,4
  ld (CurrentPlatformLengthCounter),a
  ld a,2 ; Transition 1
  ld (GroundType),a
  jp _GroundDone

++:
  ld a,0 ; draw left next
  ld (GroundType),a

  jp _GroundDone

_Transition1:
  ld hl,DrawingTilesBuffer
  ld (DrawingTilesPointer),hl

  ; Fill with all sky
  call _blankBuffer

  ; Is it a transition up or down?
  ld a,(NextPlatformHeight)
  ld b,a
  ld a,(CurrentPlatformHeight)
  cp b
  jp z,_StartFlat ; same - nothing to do!
  ; OK, so was it bigger or smaller?
  jp c,_TransitionDown

_TransitionUp:
  ; Seek to the right place to draw the ground in
  ld a,(NextPlatformHeight)
  sla a
  ld ix,DrawingTilesBuffer
  add a,ixl
  ld ixl,a
+:; now fill it in
  ld (ix+0),T_grassTL1
  ld (ix+24*2),T_grassTL2
  ld (ix+24*2+2),T_grassTL4

  ; next the edge
  ld a,(NextPlatformHeight)
  ld b,a
  ld a,(CurrentPlatformHeight)
  sub b ; a = no. of tiles
  srl a
  dec a
  jr z,+
  ld b,a
-:ld (ix+24*2+4),T_groundLR1
  ld (ix+24*2+7),$02 ; hflip
  ld (ix+24*2+6),T_groundLR2
  inc ix
  inc ix
  inc ix
  inc ix
  djnz -
+:
  ; next the end of the previous level
  ld a,(CurrentPlatformHeight)
  sla a
  ld ix,DrawingTilesBuffer
  add a,ixl
  ld ixl,a
  ; now fill it in
  ld (ix+0),T_grassTR1
  ld (ix+2),T_groundBR3
  ld (ix+24*2),T_groundBR2
  ld (ix+24*2+2),T_groundBR4

  ; finally, the ground
  ; Now fill in the ground
  ; how many? (24-a-1)/2
  ld a,(CurrentPlatformHeight)
  add a,-23
  neg
  sra a
  jr z,+
  ld b,a
-:; draw 4 tiles
  inc ix
  inc ix
  inc ix
  inc ix
  ld (ix+0),T_ground1
  ld (ix+2),T_ground2
  ld (ix+24*2+0),T_ground2
  ld (ix+24*2+2),T_ground1
  djnz -
+:; done

  ; copy it over now...
  ld a,(NextPlatformHeight)
  ld (CurrentPlatformHeight),a

  ld a,3 ; transition2
  ld (GroundType),a

  call DrawMessage

  jp _GroundDone

_TransitionDown:
  ; Seek to the right place to draw the ground in
  ld a,(CurrentPlatformHeight)
  sla a
  ld ix,DrawingTilesBuffer
  add a,ixl
  ld ixl,a
+:; now fill it in
  ld (ix+0),T_grassTR1
  ld (ix+2),T_grassTR3
  ld (ix+24*2),T_grassTR2
  ld (ix+24*2+2),T_groundR24

  ; next the edge
  ld a,(CurrentPlatformHeight)
  ld b,a
  ld a,(NextPlatformHeight)
  sub b ; a = no. of tiles
  srl a
  dec a
  jr z,+
  ld b,a
-:ld (ix+4),T_groundLR2
  ld (ix+6),T_groundLR1
  ld (ix+7),2 ; hflip
  ld (ix+24*2+4),T_groundR24
  ld (ix+24*2+6),T_groundR24
  inc ix
  inc ix
  inc ix
  inc ix
  djnz -
+:
  ; next the end of the next level
  ld a,(NextPlatformHeight)
  sla a
  ld ix,DrawingTilesBuffer
  add a,ixl
  ld ixl,a
  ; now fill it in
  ld (ix+0),T_groundBL1
  ld (ix+2),T_ground2
  ld (ix+24*2),T_grassTL2
  ld (ix+24*2+2),T_groundBL4

  ; finally, the ground
  ; Now fill in the ground
  ; how many? (24-a-1)/2
  ld a,(NextPlatformHeight)
  add a,-23
  neg
  sra a
  jr z,+
  ld b,a
-:; draw 4 tiles
  inc ix
  inc ix
  inc ix
  inc ix
  ld (ix+0),T_ground1
  ld (ix+2),T_ground2
  ld (ix+24*2+0),T_ground2
  ld (ix+24*2+2),T_ground1
  djnz -
+:; done

  ; copy it over now...
  ld a,(NextPlatformHeight)
  ld (CurrentPlatformHeight),a

  ld a,3 ; transition2
  ld (GroundType),a

  call DrawMessage

  jr _GroundDone

_Transition2:
  ld hl,DrawingTilesBuffer+24*2
  ld (DrawingTilesPointer),hl

  ld a,4 ; startFlat
  ld (GroundType),a
  ; jr _GroundDone ; fall through

_GroundDone:
  ; Put height in the height buffer
  ; First shift the contents left
  ld hl,PlatformHeightBuffer+1
  ld de,PlatformHeightBuffer+0
  ld bc,31
  ldir
  ; Then put it in on the right
  ld a,(CurrentPlatformHeight)
  ld (PlatformHeightBuffer+31),a

  ; Set the flag to copy to VRAM next frame
  ld a,1
  ld (DrawTilesFlag),a
  ret

_blankBuffer:
  ld bc,24*2*2-1
  ld hl,DrawingTilesBuffer
  ld (hl),0
  ld de,DrawingTilesBuffer+1
  ldir
  ret

DrawMessage:
  ; draw text at the top, if it's a buffer fill
  ld hl,(CurrentMessagePointer)
  ld a,(hl)
  or a
  jr z,+
  inc hl
  ld (CurrentMessagePointer),hl
  ; convert to tile index
  sub 32
  ; but if it was a control character, make it a space
  jr nc,+
  xor a
+:; write to buffer
  ld (DrawingTilesBuffer),a
  ld a,(hl)
  or a
  jr z,+
  inc hl
  ld (CurrentMessagePointer),hl
  ; convert to tile index
  sub 32
  ; but if it was a control character, make it a space
  jr nc,+
  xor a
+:; write to buffer
  ld (DrawingTilesBuffer+24*2),a
  ret
+:ld hl,Message
  ld (CurrentMessagePointer),hl
  ret

DrawTileBuffer:
  ld de,(DrawingTilesPointer)
  ld b,24 ; tiles
  ; set VRAM address
  ld hl,(DrawTilesVRAMStart)
-:ld a,l
  out (VDPAddress),a
  ld a,h
  out (VDPAddress),a
  ; Write a byte pair
  ld a,(de)
  out (VDPData),a
  inc de
  ld a,(de)
  out (VDPData),a
  inc de
  ; Increment VRAM address
  push de
    ld de,32*2
    add hl,de
  pop de
  ; Loop
  djnz -
  ; Move write pointer on
  ld hl,(DrawTilesVRAMStart)
  inc hl
  inc hl
  ; Wrap?
  ld a,l
  cp 64
  jr nz,+
  ld l,0
+:ld (DrawTilesVRAMStart),hl
  ; Reset the flag
  xor a
  ld (DrawTilesFlag),a
  ret

AddToDistance:
  ld hl,DistanceMeter+6
  ld b,7
-:ld a,(hl) ; get digit
  inc a     ; increment
  ld (hl),a ; save
  cp 10     ; but if it's 10...
  jr nz,+
  xor a     ; make it a 0
  ld (hl),a
  dec hl    ; move left one place
  djnz -    ; and repeat up to 7 digits
+:; now put it in the sprite table
  ld hl,DistanceMeter
  ld de,SpriteTableMirror+64+1
  ld b,7
-:ld a,(hl)
  add a,S_Font
  ld (de),a
  inc hl
  inc de
  inc de
  djnz -
  ret

GameVBlank:
  ; VRAM
  ld a,(DrawTilesFlag)
  or a
  call nz,DrawTileBuffer
  ; HScroll write
  ld a,(CurrentScroll+1) ; high byte
  out (VDPAddress),a
  ld a,VDPRegHScroll
  out (VDPAddress),a
  ; Sprite table
  SetVDPAddress SpriteTableAddress|$4000
  ld c,VDPData
  ld hl,SpriteTableMirror
  FastOtir 64
  SetVDPAddress (SpriteTableAddress+128)|$4000
  FastOtir 128
  ; Non-VRAM
  call GetInputs
  ret
.ends

.section "Sprite animation" free
SpriteAnimationTable:
.dw SpriteRun1,SpriteRun2,SpriteRun1,SpriteRun3
AnimateSprite:
  ; are we jumping?
  ld a,(IsJumping)
  or a
  jr z,_running
  ; Then we want the jumping tiles
  ld hl,SpriteJump
  jr _WriteToSAT

_running:
  ld a,(SpriteAnimationCounter)
  inc a
  cp 4
  jr nz,+
  xor a
+:ld (SpriteAnimationCounter),a
  ; look up the sprite changes we need
  ld hl,SpriteAnimationTable
  sla a
  ld e,a
  ld d,0
  add hl,de
  ld e,(hl)
  inc hl
  ld d,(hl)
  ex de,hl ; hl = data
_WriteToSAT:
  ; head ys
  ld a,(hl)
  ld b,a
  ld a,(CurrentY)
  add a,b
  ld ix,SpriteTableMirror+8
  ld (ix+0),a
  ld (ix+1),a
  add a,8
  ld (ix+2),a
  ld (ix+3),a
  inc hl
  ; body ys
  ld a,(CurrentY)
  add a,16
  ld (ix+4),a
  ld (ix+5),a
  add a,8
  ld (ix+6),a
  ld (ix+7),a
  ; put tile indexes in
  ld de,SpriteTableMirror+64+8*2 ; point at first tile X/index
  ld bc,8*2
  ldir
  ret
.ends

.section "Palette fades" free
; Call once per VBlank
PaletteToCRAM:
  SetVDPAddress PaletteAddress
  ld hl,ActualPalette
  ld c,VDPData
  FastOtir 32
  ret

; Call to initiate a fade in from black
FadeIn:
  ld hl,ActualPalette
  ld de,ActualPalette+1
  ld bc,31
  ld (hl),$00
  ldir               ; Fill ActualPalette with black

  ld hl,$2089        ; Set PaletteFadeControl to fade in ($89) the whole palette ($20)
  jr +

FadeOutBackground:
  ld hl,$1009        ; Set PaletteFadeControl to fade out ($89) the BG palette ($10)
  jr +

; Call to initiate a fade out to black
FadeOut:
  ld hl,$2009        ; Fade out, 32 colours
+:ld (PaletteFadeControl),hl

  ; select a suitable vblank routine
  ld hl,PaletteFadeVBlank
  ld (VBlankRoutine),hl

  ; wait for it to finish
-:
  ld a,(PaletteFadeControl)
  and $7f
  halt ; halt here because else we end one frame early
  jr nz,-

  ret


; VBlank routine for during palette fades
PaletteFadeVBlank:
  ; VRAM
  call PaletteToCRAM
  ; Non-VRAM
  call FadePaletteInRAM
  ret

; Call once per frame to do palette fading
; Stolen from Phantasy Star
; Main function body only runs every 4 calls (using PaletteFadeFrameCounter as a counter)
; Checks PaletteFadeControl - bit 7 = fade in, rest = counter
; PaletteSize tells it how many palette entries to fade
; TargetPalette and ActualPalette are referred to
FadePaletteInRAM:
    ld hl,PaletteFadeFrameCounter ; Decrement PaletteFadeFrameCounter
    dec (hl)
    ret p              ; return if >=0
    ld (hl),$03        ; otherwise set to 3 and continue (so only do this part every 4 calls)
    ld hl,PaletteFadeControl ; Check PaletteFadeControl
    ld a,(hl)
    bit 7,a            ; if bit 7 is set
    jp nz,_FadeIn      ; then fade in
    or a               ; If PaletteFadeControl==0
    ret z              ; then return
    dec (hl)           ; Otherwise, decrement PaletteFadeControl
    inc hl
    ld b,(hl)          ; PaletteSize
    ld hl,ActualPalette
  -:call _FadeOut      ; process PaletteSize bytes from ActualPalette
    inc hl
    djnz -
    ret

_FadeOut:
    ld a,(hl)
    or a
    ret z              ; zero = black = no fade to do
    and %00000011      ; check red
    jr z,+
    dec (hl)           ; If non-zero, decrement
    ret
  +:ld a,(hl)
    and %00001100      ; check green
    jr z,+
    ld a,(hl)
    sub $04            ; If non-zero, decrement
    ld (hl),a
    ret
  +:ld a,(hl)
    and $30            ; check blue
    ret z
    sub $10            ; If non-zero, decrement
    ld (hl),a
    ret

_FadeIn:
    cp $80             ; Is only bit 7 set?
    jr nz,+            ; If not, handle that
    ld (hl),$00        ; Otherwise, zero it (PaletteFadeControl)
    ret
  +:dec (hl)           ; Decrement it (PaletteFadeControl)
    inc hl
    ld b,(hl)          ; PaletteSize
    ld hl,TargetPalette
    ld de,ActualPalette
  -:call _FadePaletteEntry ; compare PaletteSize bytes from ActualPalette
    inc hl
    inc de
    djnz -
    ret

_FadePaletteEntry:
    ld a,(de)          ; If (de)==(hl) then leave it
    cp (hl)
    ret z
    add a,%00010000    ; increment blue
    cp (hl)
    jr z,+
    jr nc,++           ; if it's too far then try green
  +:ld (de),a          ; else save that
    ret
 ++:ld a,(de)
    add a,%00000100    ; increment green
    cp (hl)
    jr z,+
    jr nc,++           ; if it's too far then try red
  +:ld (de),a          ; else save that
    ret
 ++:ex de,hl
    inc (hl)           ; increment red
    ex de,hl
    ret
.ends

.section "Screen control" free
TurnOffScreen:
  ld a,%10100100
    ;    |||| |`- Zoomed sprites -> 16x16 pixels
    ;    |||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
    ;    |||`---- 30 row/240 line mode
    ;    ||`----- 28 row/224 line mode
    ;    |`------ Enable VBlank interrupts
    ;    `------- Enable display

  jr +
TurnOnScreen:
  ; turn on screen
  ld a,%11100100
    ;    |||| |`- Zoomed sprites -> 16x16 pixels
    ;    |||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
    ;    |||`---- 30 row/240 line mode
    ;    ||`----- 28 row/224 line mode
    ;    |`------ Enable VBlank interrupts
    ;    `------- Enable display
+:out (VDPStatus),a
  ld a,VDPReg_1
  out (VDPStatus),a
  ret
.ends

.section "OUTI block" free
outiblock:
.rept FastOtirBlockSize
  outi
.endr
  ret
.ends

.section "Comments" free
Comments:
.db "Written by Maxim for Omar's birthday in 2011. "
.db "Delivered only slightly late :)",10
.db "http://www.smspower.org/maxim", 0
.ends

.section "Get Inputs" free
GetInputs:
  in a,(IOPort1)
  cpl
  and P1U|P1D|P1L|P1R|P11|P12
  ld b,a             ; b = all buttons pressed
  ld hl,CurrentlyPressedButtons
  xor (hl)           ; xor with what was pressed already
  ld (hl),b
  and b              ; a = all buttons pressed since last time
  ld (JustPressedButtons),a
  in a,(IOPort2)
  cpl
  and ResetButton
  jp nz,0 ; always reset
  ret
.ends

.section "Random number generator" free
GetRandomNumber:
  ; Uses a 16-bit RAM variable called RandomNumberGeneratorWord
  ; Returns an 8-bit pseudo-random number in a
  ; trashes HL
  ld hl,(RandomNumberGeneratorWord)
  ld a,h         ; get high byte
  rrca           ; rotate right by 2
  rrca
  xor h          ; xor with original
  rrca           ; rotate right by 1
  xor l          ; xor with low byte
  rrca           ; rotate right by 4
  rrca
  rrca
  rrca
  xor l          ; xor again
  rra            ; rotate right by 1 through carry
  adc hl,hl      ; add RandomNumberGeneratorWord to itself
  jr nz,+
  ld hl,$733c    ; if last xor resulted in zero then re-seed random number generator
+:ld a,r         ; r = refresh register = semi-random number
  xor l          ; xor with l which is fairly random
  ld (RandomNumberGeneratorWord),hl
  ret                ; return random number in a
.ends

.section "Error handler" free
ErrorHandler:
  di
  ; push everything in reverse display order
  ex af,af'
  exx
  ; shadow regs
  push hl
  push de
  push bc
  push af
  ex af,af'
  exx
  ; real regs
  push iy
  push ix
  push hl
  push de
  push bc
  push af
  ; set up the screen
  call TurnOffScreen
  ld hl,_VDPRegs
  ld b,_VDPRegsEnd-_VDPRegs
  ld c,$bf
  otir
  call ClearVRAM
  ; load in the font
  ld ix,Font
  ld hl,$4000
  call PSG_decompress
  ; and the palette
  SetVDPAddress PaletteAddress
  ld hl,ErrorHandlerPalette
  ld c,VDPData
  FastOtir 32

  ld hl,ErrorTitle
  call WriteTextWithLocation
  ld hl,ErrorRegs
  call WriteTextWithLocation
  ld hl,ErrorStack
  call WriteTextWithLocation

  ; pop stuff off and display it
  ; shadow regs
  ldDEXY 0,4
  call SetVRAMAddressToDE
  ld b,4 ; number of reg pairs
-:pop hl
  call RenderHL
  djnz -

  ; real regs
  ldDEXY 0,3
  call SetVRAMAddressToDE
  ld b,6 ; number of reg pairs
-:pop hl
  call RenderHL
  djnz -

  ; return pc is at the top of the stack
  pop hl
  dec hl
  call RenderHL

  ; sp
  ld hl,0
  add hl,sp
  call RenderHL

  ; stack
  ld b,(23-6)*(32/4) ; maximum displayable
  ldDEXY 0,7
  call SetVRAMAddressToDE
-:ld a,h
  cp $df
  jr nz,+
  ld a,l
  cp $f0
  jr z,_endofstack
+:; hl is a valid stack address
  push hl
    ; get the value
    ld a,(hl)
    inc hl
    ld h,(hl)
    ld l,a
    ; render it
    call RenderHL
  pop hl
  inc hl
  inc hl
  djnz -
_endofstack:

  call TurnOnScreen

-:jr -

RenderHL:
  ; render the high byte
  ld a,h
  call RenderHexDigit
  ld a,l
  ; fall through

RenderHexDigit:
  ; high nibble
  ld c,a
  srl a
  srl a
  srl a
  srl a
  call RenderNibble
  ld a,c
  and $f
  ; fall through

RenderNibble:
  ; is it less than 10?
  cp 10
  jr c,+
  add a,$21-10 ; 10-15: how to map that with a=0
  jr ++
+:add a,$10 ; 0-9: how to map that with 0=26
++:
  out (VDPData),a
  xor a
  out (VDPData),a
  ret

ErrorTitle:
 TEXT 7,0 "Oh noes! An error!"
ErrorRegs:;01234567890123456789012345678901
 TEXT 0,2 "A F B C D E H L IX  IY  PC  SP"
ErrorStack:
 TEXT 0,6 "Stack:"

_VDPRegs:
    .db %00000100,VDPReg_0
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
_VDPRegsEnd:

.ends

.section "Text output" free
; take data at (hl)
; first 2 bytes are tilemap address
; rest is tile numbers
; terminated with STRING_TERMINATOR
; outputs to name table interspersed with $00s to select the low tiles
WriteTextWithLocation:
  ld e,(hl)
  inc hl
  ld d,(hl)
  inc hl
  ; fall through
WriteText:
  call SetVRAMAddressToDE
-:ld a,(hl)
  cp STRING_TERMINATOR
  ret z
  out (VDPData),a
  xor a
  out (VDPData),a
  inc hl
  jr -
.ends

.section "Data" free
Font:
GameGraphics:
.incbin "game.psgcompr"
TitleScreenTiles:
.incbin "titlescreen.bin" skip 32*2*31

.enum $00 ; game tiles
T_font           dsb 91
T_clouds         dsb 109
T_grassTL1       db
T_grassTL2       db
T_grassTop12     db
T_grassTR1       db
T_grassTR2       db
T_grassTL4       db
T_grassTop3      db
T_grassTop4      db
T_grassTR3       db
T_groundR24      db
T_groundLR1      db
T_ground1        db
T_ground2        db
T_groundLR2      db
T_groundBR2      db
T_groundBL1      db
T_groundBR3      db
T_groundBR4      db
T_groundBL4      db
.ende

GameSprites:
.incbin "sprites.psgcompr"

.enum $00 ; sprite tiles
S_Blank              db
S_Sprite             dsb 32
S_Font               dsb 10
S_m                  db
.ende

TitleScreenPalette:
ErrorHandlerPalette:
GamePalette:
.incbin "game-palette.bin"
SpritePalette:
.incbin "sprite-palette.bin"
.ends

.section "Message"
Message:
.incbin "message.txt"
.db 0
.ends

.section "Clouds tilemaps"
Cloud1:
.db 8,4
.db $00 $5b $5c $5d $5e $5f $60 $61
.db $62 $63 $64 $64 $64 $64 $65 $66
.db $67 $68 $64 $64 $64 $69 $6a $6b
.db $6c $6d $6e $6f $70 $71 $72 $73
Cloud2:
.db 6,5
.db $74 $75 $76 $77 $78 $79
.db $7a $7b $7c $7d $7e $00
.db $7f $65 $64 $64 $80 $81
.db $82 $83 $84 $85 $86 $87
.db $88 $89 $8a $8b $8c $8d
Cloud3:
.db 15,7
.db $00 $00 $00 $8e $8f $90 $91 $92 $93 $94 $95 $96 $00 $00 $00
.db $97 $98 $99 $9a $64 $64 $64 $64 $64 $65 $9b $9c $9d $9e $00
.db $9f $64 $64 $64 $64 $64 $64 $64 $64 $64 $64 $64 $69 $a0 $a1
.db $a2 $64 $64 $64 $64 $64 $64 $64 $64 $64 $64 $64 $64 $a3 $a4
.db $a5 $a6 $a7 $a8 $65 $64 $64 $64 $64 $64 $64 $a9 $aa $ab $ac
.db $ad $ae $af $b0 $b1 $b2 $b3 $b4 $b5 $b6 $b7 $b8 $b9 $ba $bb
.db $00 $bc $bd $be $bf $c0 $c1 $c2 $c3 $c4 $c5 $c6 $c7 $00 $00
Clouds:
.dw Cloud1,Cloud2,Cloud3
.ends

.section "Sprite table init data"
SpriteTableYInit:
.db 180, 180, 180, 180, 180, 180, 180, 180 ; meter
.define _Y 128 ; start position y
.db _Y,_Y,_Y+8,_Y+8,_Y+16,_Y+16,_Y+24,_Y+24 ; fox
.db 208 ; terminator
.dsb 64 0
SpriteTableXNInit:
.db 12,S_Font 20,S_Font 28,S_Font 36,S_Font 44,S_Font 52,S_Font 60,S_Font 68,S_m ; meter
.define _X 48 ; position x
.db _X,  1, _X+8,  2 ; fox
.db _X,  7, _X+8,  8
.db _X, 13, _X+8, 14
.db _X, 23, _X+8, 24
.dsb 128 0

; Main sprite indices
SpriteRun1:
.db 1
.db _X,  3, _X+8,  4
.db _X,  9, _X+8, 10
.db _X, 15, _X+8, 16
.db _X, 25, _X+8, 26
SpriteRun2:
.db 0
.db _X,  3, _X+8,  4
.db _X,  9, _X+8, 10
.db _X, 17, _X+8, 18
.db _X, 27, _X+8, 28
SpriteRun3:
.db 0
.db _X,  3, _X+8,  4
.db _X,  9, _X+8, 10
.db _X, 19, _X+8, 20
.db _X, 29, _X+8, 30
SpriteJump:
.db 0
.db _X,  5, _X+8,  6
.db _X, 11, _X+8, 12
.db _X, 21, _X+8, 22
.db _X, 31, _X+8, 32
.ends

.section "Jump lookup" align 256 free
JumpLookup: ; 64px in 30f
.db 4
.db 4
.db 4
.db 4
.db 4
.db 3
.db 3
.db 4
.db 3
.db 3
.db 2
.db 3
.db 2
.db 3
.db 2
.db 2
.db 2
.db 2
.db 1
.db 2
.db 1
.db 1
.db 1
.db 1
.db 1
.db 1
.db 0
.db 1
.db 0
.db 0
.define JumpLookupMax 29 ;CADDR-JumpLookup-1
.ends

.section "Death screen" free
Dead:
  ; set the grey sprite palette
  ld hl,DeathPalette
  ld de,ActualPalette+16
  ld bc,16
  ldir

  call FadeOutBackground

  ld hl,DeathVBlank
  ld (VBlankRoutine),hl

  ld a,2
  ld (IsJumping),a

-:halt
  ; animate the fox down
  ld a,(CurrentY)
  cp 208
  jr z,+
  inc a
  ld (CurrentY),a
  call AnimateSprite
  jr -

+:call FadeOut
  ; is it the new high-score?
  ; compare left to right
  ; higher means new high score
  ; lower means lower
  ; equal means next digit
  ; first page it in
  ld hl,HighScore
  ld de,DistanceMeter
  ld b,7

-:ld a,(de)
  dec b
  cp (hl)
  inc hl
  inc de
  jr z,-
  jr nc,_NewHighScore
  ; not a new high score
_scoreCheckEnd:

  jp TitleScreen

_NewHighScore:
  ; copy it
  ld hl,DistanceMeter
  ld de,HighScore
  ld b,7
  ldir
  jr _scoreCheckEnd


ROMMarker:
.db "BOCK'S BIRTHDAY "
.db "2011  BACKUP RAM"
.db "PROGRAMMED BY   "
.db "           MAXIM"

DeathPalette:
.define A 0
.define B $15
.define C $2a
.define D $3f
.db $24 A B C D B C B B C C
.undef A
.undef B
.undef C
.undef D

DeathVBlank:
  ; VRAM stuff
  ; Sprite table
  SetVDPAddress SpriteTableAddress|$4000
  ld c,VDPData
  ld hl,SpriteTableMirror
  FastOtir 64
  SetVDPAddress (SpriteTableAddress+128)|$4000
  FastOtir 128
  ; Non-VRAM stuff
  ret
.ends
