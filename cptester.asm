 processor 6502				;Procesor 6510, 6502, 8500 - C64
 org $0801
 
PLOT	= $fff0				; Funkcja PLOT z KERNAL (ustawienie kursora)
CHROUT	= $ffd2				; Funkcja CHROUT z KERNAL (wypisanie znaku na aktualnej pozycji kursora)
zero_tmp = $FB				; Miejsce na stronie zerowej, na wskaźnik do wypisywanego stringa
kolor_ramki = [zero_tmp + 3]
stop 	= $91				; Adres na mapie C64 - można z niego odczytać status klawisza RUN/STOP #$7F
stop_pressed	= $7f
port2 	= $dc00				; Adres Control Port 2
port1 	= $dc01				; Adres Control Port 1
ekran 	= $400				; Adres początku ekranu tekstowego
colory_ekran	= $d800		; Adres początku mapy kolorów ekranu
cls_code = 147				; Kod czyszczenia ekranu
kolor_aktywny = 4			; Fioletowy :)

wwierszu 	= 40							; Długość wiersza ekranu
wiersz 		= 8
j1p 	= ekran+(wwierszu*wiersz)+13		; Wyznaczenie środka wizualizacji stanu joysticka na porcie 1
j2p 	= ekran+(wwierszu*wiersz)+25		; Wyznaczenie środka wizualizacji stanu joysticka na porcie 2

hbyte  = >j1p
hbyte_c = >(colory_ekran+(wwierszu*wiersz)+13)

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

; ******* Poczatek w BASIC ********

  .WORD basend,asmstart 	; Adres następnej linii w basicu, nr lini BASIC taki jak adres procedury (dla żartu)
  .DC #$9e,(asmstart/1000)%10+$30,(asmstart/100)%10+$30,(asmstart/10)%10+$30,asmstart%10+$30 ; komenda SYS, czterocyfrowy adres procedury w ASCII
  .DC #$00 					; koniec lini w BASIC
basend:
  .WORD #$0000 				; koniec programu w BASIC


; ********* Dane w pamięci ***************

pozycje					.DC #<j2p,#<[j2p + 1],#<[j2p - 1],#<[j2p + wwierszu],#<[j2p - wwierszu]
						.DC #<[j1p],#<[j1p + 1],#<[j1p - 1],#<[j1p + wwierszu],#<[j1p - wwierszu]
stany_on				.DC fire_on,pionowy_p,pionowy_l,poziomy_d,poziomy_g
						.DC     fire_on,pionowy_p,pionowy_l,poziomy_d,poziomy_g
stany_off				.DC fire_off,pionowy,pionowy,poziomy,poziomy
						.DC     fire_off,pionowy,pionowy,poziomy,poziomy
powitanie 				.DC "*** CPTESTER V5.0 ***",0				;Napisy na ekranie
opis					.DC "PORT #1     PORT #2",0
wyjscie					.DC "PRESS STOP KEY TO EXIT...",0
kolor_ramki_nowy_tmp	.DC #$00
kolor_ramki_nowy		.DC #$00
znak_c					.DC #$00


; *********** Kod programu *************  

irg_1:	
  lda #<irg_2
  sta $0314
  lda #>irg_2
  sta $0315					;Re-direct next interrupt to irg_2 service routine
  lda #250
  sta $d012					;Next interrupt to occur at raster line no. 0
  lda kolor_ramki_nowy
  sta $d020
  asl $d019					;"Acknowledge" the interrupt by clearing the VIC's interrupt flag.
  jmp $ea31					;Jump to the beginning KERNAL's standard interrupt service routine.



irg_2:	
  lda #<irg_1
  sta $0314
  lda #>irg_1
  sta $0315					;Re-direct next interrupt back to irg_1
  lda #50
  sta $d012					;Next interrupt to occur at raster line no. 210
  lda kolor_ramki
  sta $d020
  asl $d019					;"Acknowledge" the interrupt by clearing the VIC's interrupt flag.
  jmp $ea81					;Jump to the final part of KERNAL's standard interrupt service routine.




asmstart:
  lda #%01111111
  sta $dc0d					;"Switch off" interrupts signals from CIA-1
  and $d011
  sta $d011					;Clear most significant bit in VIC's raster register
  lda #51
  sta $d012					;Set the raster line number where interrupt should occur
  lda #<irg_1
  sta $0314
  lda #>irg_1
  sta $0315					;Set the interrupt vector to point to interrupt service routine below
  lda $d020
  sta kolor_ramki
  lda #%00000001
  sta $d01a					;Enable raster interrupt signals from VIC
  
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
  lda kolor_ramki
  sta kolor_ramki_nowy_tmp
  ldy #9					; Ilość przebiegów pętli sprawdzającej *2 / 10*2
  lda port1					
  sta [zero_tmp + 2]		; Zachowujemy zawartosc portu, do badania kolejnych bitow w zero_tmp+2
zapisz:  
  cpy #4
  bne omin
  lda port2					; Jeżeli jesteśmy w połowie pętli, to zachowujemy zawartosc drugiego portu, pierwszy jest przeanalizowany
  sta [zero_tmp + 2]
omin:
  ldx stany_off,y			; Juz tutaj ładujemy do X odwzorowanie stanu nieaktywnego pola wynikajacego z indeksu Y
  lda colory_ekran+999		; Kolor ostatniego znaku, to kolor znaków przed uruchomieniem programu, tego koloru nie modyfikujemy
  sta znak_c   
  lda #01					; Testujemy najmlodszy bit, pozniej przesuniemy w prawo
  and [zero_tmp + 2]
  bne pusty
ustawiony:
  ldx stany_on,y			; Ładujemy do X odwzorowanie stanu aktywnego pola wynikajacego z indeksu Y
  lda $d021
  sta kolor_ramki_nowy_tmp
  lda #kolor_aktywny
  sta znak_c  
pusty:
  lsr [zero_tmp + 2]			; Przesuwamy w prawo, by następnym razem sprawdzić kolejny bit
  lda pozycje,y				; Ustawiamy na stronie zerowej adres na ekranie, gdzie wyświetlamy odwzorowanie
  sta zero_tmp
  lda #hbyte
  sta [zero_tmp + 1]
  txa						; Do akumlatora przesuwamy z X ustawiony wcześniej symbol odwzorowania stanu
  ldx #$00					; Ładujemy 0 do X, żeby wykorzystać zapis indeksowany (bez przesunięcia) typu indirect
  sta (zero_tmp,x)			; Na ekran przygotowany symbol
  lda #hbyte_c					; Zmieniamy tylko starszy bajt adresu, żeby wskazywał na mapę kolorów
  sta [zero_tmp + 1]
  lda znak_c
  sta (zero_tmp,x)
  dey
  bpl zapisz 				; Jeżeli indeks nieujemny to zapętlamy
  lda stop
  cmp #stop_pressed
  beq koniec 
  lda kolor_ramki_nowy_tmp
  sta kolor_ramki_nowy
  jmp loop

koniec:
  lda #%00000000
  sta $d01a	;Disable raster interrupt signals from VIC
  lda #%11111111
  sta $dc0d	;"Switch on" interrupts signals from CIA-1  
  lda #$31
  sta $0314
  lda #$ea
  sta $0315
  lda kolor_ramki
  sta $d020
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

