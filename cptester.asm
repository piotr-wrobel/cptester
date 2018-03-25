 processor 6502						;Procesor 6510, 6502, 8500 - C64
 org $1000
 
PLOT	= $fff0						; Funkcja PLOT z KERNAL (ustawienie kursora)
CHROUT	= $ffd2						; Funkcja CHROUT z KERNAL (wypisanie znaku na aktualnej pozycji kursora)
zero_tmp = $FB						; Miejsce na stronie zerowej, na wskaźnik do wypisywanego stringa
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
  lda #powitanie&255		; Pod adres zero_tmp wrzucany wskaznik do napisu
  sta zero_tmp
  lda #powitanie/256
  sta [zero_tmp + 1]
  jsr putmsg_xy
  ldx #22					; Ustawienie wiersza
  ldy #0					; Ustawienie kolumny
  lda #wyjscie&255			; Pod adres zero_tmp wrzucany wskaznik do napisu
  sta zero_tmp
  lda #wyjscie/256
  sta [zero_tmp + 1]
  jsr putmsg_xy  
loop:  						; Pętla główna
  ldy #20					; Ilość przebiegów pętli sprawdzającej *2 / 10*2
  lda port1					
  sta zero_tmp+2			; Zachowujemy zawartosc portu, do badania kolejnych bitow w zero_tmp+2
zapisz:  
  cpy #10					; Jeżeli jesteśmy w połowie pętli, to zachowujemy zawartosc drugiego portu, pierwszy jest przeanalizowany
  bne omin
  lda port2
  sta zero_tmp+2
omin:
  ldx stany,Y				; Juz tutaj ładujemy do X odwzorowanie stanu aktywnego pola wynikajacego z indeksu Y
  lda #01					; Testujemy najmlodszy bit, pozniej przesuniemy w prawo
  and zero_tmp+2
  beq ustawiony
pusty:
  ldx stany+1,Y				; Jednak stan nie jest aktywny, nadpisujemy odpowiednim stanem do odwzorowania na ekranie
ustawiony:
  ror zero_tmp+2
  lda pozycje,Y
  sta zero_tmp
  lda pozycje+1,Y
  sta zero_tmp+1
  txa
  ldx #$00
  sta (zero_tmp,X)
  dey
  dey
  bne zapisz 
  
  lda stop
  cmp #stop_pressed
  beq koniec 
  jmp loop
koniec:
  lda #cls_code		; Czyszczenie ekranu inline
  jmp CHROUT

  
  ; ********** Funkcje dodatkowe *******************
putmsg_xy .SUBROUTINE		; Wypisanie stringa na ekran od zdefiniowanej X,Y pozycji kursora
  clc 
  jsr PLOT
putmsg .SUBROUTINE 			; Wypisanie stringa na ekran od aktualnej pozycji kursora
  ldy #$00
.loop: 
  lda (zero_tmp),y
  beq .koniec
  jsr CHROUT
  iny
  bne .loop
.koniec
  rts

pozycje		.DC 0,0,[[j2p]&255],[[j2p]/256],[[j2p + 1]&255],[[j2p + 1]/256],[[j2p - 1]&255],[[j2p - 1]/256],[[j2p + wiersz]&255],[[j2p + wiersz]/256],[[j2p - wiersz]&255],[[j2p - wiersz]/256]
			.DC     [[j1p]&255],[[j1p]/256],[[j1p + 1]&255],[[j1p + 1]/256],[[j1p - 1]&255],[[j1p - 1]/256],[[j1p + wiersz]&255],[[j1p + wiersz]/256],[[j1p - wiersz]&255],[[j1p - wiersz]/256]
stany		.DC 0,0,fire_on,fire_off,pionowy_p,pionowy,pionowy_l,pionowy,poziomy_d,poziomy,poziomy_g,poziomy
			.DC     fire_on,fire_off,pionowy_p,pionowy,pionowy_l,pionowy,poziomy_d,poziomy,poziomy_g,poziomy
powitanie 	.DC "*** CP TESTER V.3 ***",0				; Nazwa programu
wyjscie		.DC "PRESS STOP KEY TO EXIT...",0