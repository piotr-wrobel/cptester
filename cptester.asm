 processor 6502				;Procesor 6510, 6502, 8500 - C64
 org $1000
 
PLOT	= $fff0				; Funkcja PLOT z KERNAL (ustawienie kursora)
CHROUT	= $ffd2				; Funkcja CHROUT z KERNAL (wypisanie znaku na aktualnej pozycji kursora)
zero_tmp = $FB				; Miejsce na stronie zerowej, na wskaźnik do wypisywanego stringa
stop 	= $91				; Adres na mapie C64 - można z niego odczytać status klawisza RUN/STOP #$7F
stop_pressed	= $7f
port2 	= $dc00				; Adres Control Port 2
port1 	= $dc01				; Adres Control Port 1
ekran 	= $400				; Adres początku ekranu tekstowego
cls_code = 147				; Kod czyszczenia ekranu

wwierszu 	= 40							; Długość wiersza ekranu
wiersz 		= 8
j1p 	= ekran+(wwierszu*wiersz)+13		; Wyznaczenie środka wizualizacji stanu joysticka na porcie 1
j2p 	= ekran+(wwierszu*wiersz)+25		; Wyznaczenie środka wizualizacji stanu joysticka na porcie 2

pionowy		= 66			; Kod znaku lini pionowej
poziomy		= 67			; Kod znaku lini poziomej
pionowy_l	= 115			; Kod znaku -|
pionowy_p	= 107			; Kod znaku |-
poziomy_g	= 113			; Kod znaku _|_
poziomy_d	= 114			; Kod znaku odwrotnego do powyższego
fire_on		= 81			; Kod znaku pełnego kółeczka
fire_off	= 87			; Kod znaku pustego kółeczka

up		= $01
down	= $02
left	= $04
right	= $08
fire	= $10

  lda #cls_code				; Czyszczenie ekranu inline
  jsr CHROUT
  ldx #2					; Ustawienie wiersza
  ldy #9					; Ustawienie kolumny
  lda #<powitanie			; Pod adres zero_tmp wrzucany wskaznik do napisu
  sta zero_tmp
  lda #>powitanie
  sta [zero_tmp + 1]
  jsr putmsg_xy
  ldx #5					; Ustawienie wiersza
  ldy #10					; Ustawienie kolumny
  lda #<opis				; Pod adres zero_tmp wrzucany wskaznik do napisu
  sta zero_tmp
  lda #>opis
  sta [zero_tmp + 1]
  jsr putmsg_xy  
  ldx #22					; Ustawienie wiersza
  ldy #0					; Ustawienie kolumny
  lda #<wyjscie				; Pod adres zero_tmp wrzucany wskaznik do napisu
  sta zero_tmp
  lda #>wyjscie
  sta [zero_tmp + 1]
  jsr putmsg_xy
loop:  						; Pętla główna
  ldy #9					; Ilość przebiegów pętli sprawdzającej *2 / 10*2
  lda port1					
  sta zero_tmp+2			; Zachowujemy zawartosc portu, do badania kolejnych bitow w zero_tmp+2
zapisz:  
  cpy #4
  bne omin
  lda port2					; Jeżeli jesteśmy w połowie pętli, to zachowujemy zawartosc drugiego portu, pierwszy jest przeanalizowany
  sta zero_tmp+2
omin:
  ldx stany_on,y			; Juz tutaj ładujemy do X odwzorowanie stanu aktywnego pola wynikajacego z indeksu Y
  lda #01					; Testujemy najmlodszy bit, pozniej przesuniemy w prawo
  and zero_tmp+2
  beq ustawiony
pusty:
  ldx stany_off,y			; Jednak stan nie jest aktywny, nadpisujemy odpowiednim stanem do odwzorowania na ekranie
ustawiony:
  lsr zero_tmp+2			; Przesuwamy w pracy, by następnym razem sprawdzić kolejny bit
  lda pozycje,y				; Ustawiamy na stronie zerowej adres na ekranie, gdzie wyświetlamy odwzorowanie
  sta zero_tmp
  lda #$05
  sta zero_tmp+1
  txa						; Do akumlatora przesuwamy z X ustawiony wcześniej symbol odwzorowania stanu
  ldx #$00					; Ładujemy 0 do X, żeby wykorzystać zapis indeksowany (bez przesunięcia) typu indirect
  sta (zero_tmp,x)			; Na ekran przygotowany symbol
  dey
  bpl zapisz 				; Jeżeli indeks nieujemny to zapętlamy
  
  lda stop
  cmp #stop_pressed
  beq koniec 
  jmp loop
koniec:
  lda #cls_code				; Czyszczenie ekranu inline
  jmp CHROUT				; Skok do funkcji KERNALA, ta zakończy się RTS i wyjście do BASICA

  
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

; ********* Dane w pamięci ***************

pozycje		.DC #<j2p,#<[j2p + 1],#<[j2p - 1],#<[j2p + wwierszu],#<[j2p - wwierszu]
			.DC #<[j1p],#<[j1p + 1],#<[j1p - 1],#<[j1p + wwierszu],#<[j1p - wwierszu]
stany_on	.DC fire_on,pionowy_p,pionowy_l,poziomy_d,poziomy_g
			.DC     fire_on,pionowy_p,pionowy_l,poziomy_d,poziomy_g
stany_off	.DC fire_off,pionowy,pionowy,poziomy,poziomy
			.DC     fire_off,pionowy,pionowy,poziomy,poziomy			
powitanie 	.DC "*** CP TESTER V.4 ***",0				;Napisy na ekranie
opis		.DC "PORT #1     PORT #2",0
wyjscie		.DC "PRESS STOP KEY TO EXIT...",0
