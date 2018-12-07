set BMP2TILE="..\..\Delphi\BMP to tile\bmp2tile.exe"
%BMP2TILE% "gfx.png"                 -savetiles "game.psgcompr"    -savepalette "game-palette.bin" -savetilemap "titlescreen.bin" -exit
%BMP2TILE% "sprites.png"             -savetiles "sprites.psgcompr" -savepalette "sprite-palette.bin" -exit
