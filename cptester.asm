 processor 6502						;Procesor 6510, 6502, 8500 - C64
 org $1000
 
PLOT	= $fff0						; Funkcja PLOT z KERNAL (ustawienie kursora)
CHROUT	= $ffd2						; Funkcja CHROUT z KERNAL (wypisanie znaku na aktualnej pozycji kursora)
stradrr = $FB						; Miejsce na stronie zerowej, na wskaźnik do wypisywanego stringa
stop 	= $91						; Adres na mapie C64 - można z niego odczytać status klawisza RUN/STOP #$7F
stop_pressed	= $7f
port2 	= $dc00						; Adres Control Port 2
port1 	= $dc01						; Adres Control Port 1
ekran 	= $400						; Adres początku ekranu tekstowego
cls_code = 147						; Kod czyszczenia ekranu

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

up		= $01
down	= $02
left	= $04
right	= $08
fire	= $10


  ;jsr cls
  lda #cls_code				; Czyszczenie ekranu inline
  jsr CHROUT
  ldx #2					; Ustawienie wiersza
  ldy #9					; Ustawienie kolumny
  lda #powitanie&255		; Pod adres stradrr wrzucany wskaznik do napisu
  sta stradrr
  lda #powitanie/256
  sta [stradrr + 1]
  jsr putmsg_xy
  ldx #22					; Ustawienie wiersza
  ldy #0					; Ustawienie kolumny
  lda #wyjscie&255			; Pod adres stradrr wrzucany wskaznik do napisu
  sta stradrr
  lda #wyjscie/256
  sta [stradrr + 1]
  jsr putmsg_xy  
loop:
  ldx port2
  ldy #poziomy_g
  txa
  and #up
  beq e1
  ldy #poziomy
e1:
  sty [j2p - wiersz] 
  ldy #poziomy_d
  txa 
  and #down
  beq e2
  ldy #poziomy
e2:
  sty [j2p + wiersz] 
  ldy #pionowy_l
  txa 
  and #left
  beq e3
  ldy #pionowy
e3:
  sty [j2p - 1] 
  ldy #pionowy_p
  txa 
  and #right
  beq e4
  ldy #pionowy
e4:
  sty [j2p + 1] 
  ldy #fire_on
  txa 
  and #fire
  beq e5
  ldy #fire_off
e5:
  sty j2p 

  ldx port1
  ldy #poziomy_g
  txa
  and #up
  beq e6
  ldy #poziomy
e6:
  sty [j1p - wiersz] 
  ldy #poziomy_d
  txa 
  and #down
  beq e7
  ldy #poziomy
e7:
  sty [j1p + wiersz] 
  ldy #pionowy_l
  txa 
  and #left
  beq e8
  ldy #pionowy
e8:
  sty [j1p - 1] 
  ldy #pionowy_p
  txa 
  and #right
  beq e9
  ldy #pionowy
e9:
  sty [j1p + 1] 
  ldy #fire_on
  txa 
  and #fire
  beq e10
  ldy #fire_off
e10:
  sty j1p
  lda stop
  cmp #stop_pressed
  beq koniec 
  jmp loop
koniec:
  ;jsr cls
  lda #cls_code		; Czyszczenie ekranu inline
  jmp CHROUT
  ;rts
 
 ; ********** Funkcje dodatkowe *******************
 
putmsg_xy .SUBROUTINE		; Wypisanie stringa na ekran od zdefiniowanej X,Y pozycji kursora
  clc 
  jsr PLOT
putmsg .SUBROUTINE 			; Wypisanie stringa na ekran od aktualnej pozycji kursora
  ldy #$00
.loop: 
  lda (stradrr),y
  beq .koniec
  jsr CHROUT
  iny
  bne .loop
.koniec
  rts
 
; cls .SUBROUTINE				; Procedurka czyszczenia ekranu
;   ldx #$00
;   lda #$20
; .loop:  
;   sta ekran,x
;   sta ekran+$100,x
;   sta ekran+$200,x
;   sta ekran+$300,x
;   dex
;   bne .loop
;   rts
  
;cls .SUBROUTINE				; Procedurka czyszczenia ekranu w KERNAL
;  lda #cls_code
;  jsr CHROUT
;  rts  

powitanie 	.DC "*** CP TESTER V.3 ***",0				; Nazwa programu
wyjscie		.DC "PRESS STOP KEY TO EXIT...",0
;dane 		.DS 8,255
