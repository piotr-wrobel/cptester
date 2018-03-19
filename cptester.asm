 processor 6502						;Procesor 6510, 6502, 8500 - C64
 org $1000
 
PLOT	= $fff0						; Funkcja PLOT z KERNAL (ustawienie kursora)
CHROUT	= $ffd2						; Funkcja CHROUT z KERNAL (wypisanie znaku na aktualnej pozycji kursora)
stradrr = $FB						; Miejsce na stronie zerowej, na wskaźnik do wypisywanego stringa
stop 	= $91						; Adres na mapie C64 - można z niego odczytać status klawisza RUN/STOP #$7F
port2 	= $dc00						; Adres Control Port 2
port1 	= $dc01						; Adres Control Port 1
ekran 	= $400						; Adres początku ekranu tekstowego

wiersz 	= 40							; Długość wiersza ekranu
j1p 	= ekran+(wiersz*6)+13		; Wyznaczenie środka wizualizacji stanu joysticka na porcie 1
j2p 	= ekran+(wiersz*6)+25		; Wyznaczenie środka wizualizacji stanu joysticka na porcie 2

pionowy		= 66						; Kod znaku lini pionowej
poziomy		= 67						; Kod znaku lini poziomej
pionowy_l	= 115					; Kod znaku -|
pionowy_p	= 107					; Kod znaku |-
poziomy_g	= 113					; Kod znaku _|_
poziomy_d	= 114					; Kod znaku odwrotnego do powyższego
fire_on		= 81						; Kod znaku pełnego kółeczka
fire_off	= 87						; Kod znaku pustego kółeczka

key_home	= 19
key_right	= 29
key_down	= 17


  jsr cls
  ldx #2			; Ustawienie wiersza
  ldy #9			; Ustawienie kolumny
  lda #powitanie&255		; Pod adres stradrr wrzucany wskaznik do napisu
  sta stradrr
  lda #powitanie/256
  sta stradrr+1
  jsr putmsg_xy
  ldx #22			; Ustawienie wiersza
  ldy #0			; Ustawienie kolumny
  lda #wyjscie&255		; Pod adres stradrr wrzucany wskaznik do napisu
  sta stradrr
  lda #wyjscie/256
  sta stradrr+1
  jsr putmsg_xy  
loop:
  lda port2
  tax
  and #$01
  bne e1
  lda #poziomy_g
  jmp e2
e1:
  lda #poziomy
e2:
  sta j2p-wiersz 
  txa 
  and #$02
  bne e3
  lda #poziomy_d
  jmp e4
e3:
  lda #poziomy
e4:
  sta j2p+wiersz 
  txa 
  and #$04
  bne e5
  lda #pionowy_l
  jmp e6
e5:
  lda #pionowy
e6:
  sta j2p-1 
  txa 
  and #$08
  bne e7
  lda #pionowy_p
  jmp e8
e7:
  lda #pionowy
e8:
  sta j2p+1 
  txa 
  and #$10
  bne e9
  lda #fire_on
  jmp e10
e9:
  lda #fire_off
e10:
  sta j2p 

  lda port1
  tax
  and #$01
  bne e11
  lda #poziomy_g
  jmp e12
e11:
  lda #poziomy
e12:
  sta j1p-wiersz 
  txa 
  and #$02
  bne e13
  lda #poziomy_d
  jmp e14
e13:
  lda #poziomy
e14:
  sta j1p+wiersz 
  txa 
  and #$04
  bne e15
  lda #pionowy_l
  jmp e16
e15:
  lda #pionowy
e16:
  sta j1p-1 
  txa 
  and #$08
  bne e17
  lda #pionowy_p
  jmp e18
e17:
  lda #pionowy
e18:
  sta j1p+1 
  txa 
  and #$10
  bne e19
  lda #fire_on
  jmp e20
e19:
  lda #fire_off
e20:
  sta j1p
  lda stop
  cmp #$7f
  beq koniec 
  jmp loop
koniec:
  rts
 
 ; ********** Funkcje dodatkowe *******************
 
putmsg_xy .SUBROUTINE		; Wypisanie stringa na ekran od zdefiniowanej X,Y pozycji kursora
  clc 
  jsr PLOT
putmsg .SUBROUTINE 			; Wypisanie stringa na ekran od aktualnej pozycji kursora
  ldy #$00
.loop: 
  lda (stradrr),y
  bne .wypisz
  rts
.wypisz:
  jsr CHROUT
  iny
  jmp .loop
 
cls .SUBROUTINE				; Procedurka czyszczenia ekranu
  ldx #$00
  lda #$20
.loop:  
  sta ekran,x
  sta ekran+$100,x
  sta ekran+$200,x
  sta ekran+$300,x
  dex
  bne .loop
  rts

powitanie 	.DC "*** CP TESTER V.3 ***",0				; Nazwa programu
wyjscie		.DC "PRESS STOP KEY TO EXIT...",0
;dane 		.DS 8,255
