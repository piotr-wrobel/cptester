 processor 6502				; Procesor 6510, 6502, 8500 - C64
 org $0801
 
PLOT	= $fff0				; Funkcja PLOT z KERNAL (ustawienie kursora)
CHROUT	= $ffd2				; Funkcja CHROUT z KERNAL (wypisanie znaku na aktualnej pozycji kursora)
zero_tmp = $FB				; Miejsce na stronie zerowej, na wskaźnik do wypisywanego stringa
stop 	= $91				; Adres na mapie C64 - można z niego odczytać status klawisza RUN/STOP #$7F
stop_pressed	= $7f		; Maska nacisnietego klawisza STOP
port2 	= $dc00				; Adres Control Port 2
port1 	= $dc01				; Adres Control Port 1
cls_code = 147				; Kod czyszczenia ekranu

SPRITES_MEMORY 	= $3200
SPR_J_C							= [SPRITES_MEMORY/64]
SPR_J_U							= [SPR_J_C + 1]
SPR_J_UR						= [SPR_J_C + 2]
SPR_J_R							= [SPR_J_C + 3]
SPR_J_DR						= [SPR_J_C + 4]
SPR_J_D							= [SPR_J_C + 5]
SPR_J_DL						= [SPR_J_C + 6]
SPR_J_L							= [SPR_J_C + 7]
SPR_J_UL						= [SPR_J_C + 8]
SPR_J_FIRE						= [SPR_J_C + 9]
SPR_J_ERR						= [SPR_J_C + 10]

MULTICOLOR_ON		= $d01c
MULTICOLOR_R1 		= $d025
MULTICOLOR_R2 		= $d026

SPRITE_VISIBLE_R 	= $d015

SPRITE_X_EXPAND		= $d01d
SPRITE_Y_EXPAND		= $d017

SPRITES_MCOLOR1		= $03
SPRITES_MCOLOR2		= $05
SPRITE_COLOR1		= $0e
SPRITE_COLOR2		= $07

SPRITE0_COLOR_R 	= $d027
SPRITE0_POINTER 	= $07f8

SPRITE0_X_REG 		= $d000
SPRITE0_Y_REG 		= $d001

SPRITE_0			= $01
SPRITE_1			= $02
SPRITE_2			= $04
SPRITE_3			= $08
SPRITE_4			= $10
SPRITE_5			= $20
SPRITE_6			= $40
SPRITE_7			= $80
SPRITE_ALL			= $ff

JOY_0_X_POS			= 120
JOY_0_Y_POS			= 110
JOY_1_X_POS			= 216
JOY_1_Y_POS			= 110

; ******* Poczatek w BASIC ********

  .WORD basend,asmstart 																														; Adres następnej linii w basicu, nr lini BASIC taki jak adres procedury (dla żartu)
  .DC #$9e,(asmstart/1000)%10+$30,(asmstart/100)%10+$30,(asmstart/10)%10+$30,asmstart%10+$30 		; komenda SYS, czterocyfrowy adres procedury w ASCII
  .DC #$00																																				; koniec lini w BASIC
basend:
  .WORD #$0000 																																		; koniec programu w BASIC

; ********* Dane w pamięci ***************

joy_c_data
	.DC $07,$ff,$d0,$1f,$c3,$f4,$3f,$c3
	.DC $fc,$43,$ff,$c1,$c3,$ff,$c3,$cf
	.DC $ff,$f3,$ff,$eb,$ff,$ff,$96,$ff
	.DC $fe,$55,$bf,$ce,$69,$b3,$ce,$69
	.DC $b3,$ce,$69,$b3,$fe,$55,$bf,$ff
	.DC $96,$ff,$ff,$eb,$ff,$cf,$ff,$f3
	.DC $c3,$ff,$c3,$43,$ff,$c1,$3f,$c3
	.DC $fc,$1f,$c3,$f4,$07,$ff,$d0,$8e

joy_u_data
	.DC $07,$ff,$d0,$1f,$c3,$f4,$3f,$c3
	.DC $fc,$43,$ff,$c1,$c3,$d7,$c3,$cf
	.DC $69,$f3,$ff,$69,$ff,$ff,$69,$ff
	.DC $fe,$69,$bf,$ce,$69,$b3,$ce,$69
	.DC $b3,$ce,$69,$b3,$fe,$55,$bf,$ff
	.DC $96,$ff,$ff,$eb,$ff,$cf,$ff,$f3
	.DC $c3,$ff,$c3,$43,$ff,$c1,$3f,$c3
	.DC $fc,$1f,$c3,$f4,$07,$ff,$d0,$8e

joy_ur_data
	.DC $07,$ff,$d0,$1f,$c3,$f4,$3f,$c3
	.DC $fc,$43,$ff,$c1,$c3,$ff,$c3,$cf
	.DC $ff,$f3,$ff,$e9,$5f,$ff,$96,$9f
	.DC $fe,$5a,$9f,$ce,$6a,$93,$ce,$6a
	.DC $73,$ce,$69,$73,$fe,$55,$bf,$ff
	.DC $96,$ff,$ff,$eb,$ff,$cf,$ff,$f3
	.DC $c3,$ff,$c3,$43,$ff,$c1,$3f,$c3
	.DC $fc,$1f,$c3,$f4,$07,$ff,$d0,$8e

joy_r_data
	.DC $07,$ff,$d0,$1f,$c3,$f4,$3f,$c3
	.DC $fc,$43,$ff,$c1,$c3,$ff,$c3,$cf
	.DC $ff,$f3,$ff,$eb,$ff,$ff,$96,$ff
	.DC $fe,$55,$5f,$ce,$6a,$93,$ce,$6a
	.DC $93,$ce,$6a,$93,$fe,$55,$5f,$ff
	.DC $96,$ff,$ff,$eb,$ff,$cf,$ff,$f3
	.DC $c3,$ff,$c3,$43,$ff,$c1,$3f,$c3
	.DC $fc,$1f,$c3,$f4,$07,$ff,$d0,$8e

joy_dr_data
	.DC $07,$ff,$d0,$1f,$c3,$f4,$3f,$c3
	.DC $fc,$43,$ff,$c1,$c3,$ff,$c3,$cf
	.DC $ff,$f3,$ff,$eb,$ff,$ff,$96,$ff
	.DC $fe,$55,$bf,$ce,$69,$73,$ce,$6a
	.DC $73,$ce,$6a,$93,$fe,$5a,$9f,$ff
	.DC $96,$9f,$ff,$e9,$5f,$cf,$ff,$f3
	.DC $c3,$ff,$c3,$43,$ff,$c1,$3f,$c3
	.DC $fc,$1f,$c3,$f4,$07,$ff,$d0,$8e

joy_d_data
	.DC $07,$ff,$d0,$1f,$c3,$f4,$3f,$c3
	.DC $fc,$43,$ff,$c1,$c3,$ff,$c3,$cf
	.DC $ff,$f3,$ff,$eb,$ff,$ff,$96,$ff
	.DC $fe,$55,$bf,$ce,$69,$b3,$ce,$69
	.DC $b3,$ce,$69,$b3,$fe,$69,$bf,$ff
	.DC $69,$ff,$ff,$69,$ff,$cf,$69,$f3
	.DC $c3,$d7,$c3,$43,$ff,$c1,$3f,$c3
	.DC $fc,$1f,$c3,$f4,$07,$ff,$d0,$8e

joy_dl_data
	.DC $07,$ff,$d0,$1f,$c3,$f4,$3f,$c3
	.DC $fc,$43,$ff,$c1,$c3,$ff,$c3,$cf
	.DC $ff,$f3,$ff,$eb,$ff,$ff,$96,$ff
	.DC $fe,$55,$bf,$cd,$69,$b3,$cd,$a9
	.DC $b3,$c6,$a9,$b3,$f6,$a5,$bf,$f6
	.DC $96,$ff,$f5,$6b,$ff,$cf,$ff,$f3
	.DC $c3,$ff,$c3,$43,$ff,$c1,$3f,$c3
	.DC $fc,$1f,$c3,$f4,$07,$ff,$d0,$8e

joy_l_data
	.DC $07,$ff,$d0,$1f,$c3,$f4,$3f,$c3
	.DC $fc,$43,$ff,$c1,$c3,$ff,$c3,$cf
	.DC $ff,$f3,$ff,$eb,$ff,$ff,$96,$ff
	.DC $f5,$55,$bf,$c6,$a9,$b3,$c6,$a9
	.DC $b3,$c6,$a9,$b3,$f5,$55,$bf,$ff
	.DC $96,$ff,$ff,$eb,$ff,$cf,$ff,$f3
	.DC $c3,$ff,$c3,$43,$ff,$c1,$3f,$c3
	.DC $fc,$1f,$c3,$f4,$07,$ff,$d0,$8e

joy_ul_data
	.DC $07,$ff,$d0,$1f,$c3,$f4,$3f,$c3
	.DC $fc,$43,$ff,$c1,$c3,$ff,$c3,$cf
	.DC $ff,$f3,$f5,$6b,$ff,$f6,$96,$ff
	.DC $f6,$a5,$bf,$c6,$a9,$b3,$cd,$a9
	.DC $b3,$cd,$69,$b3,$fe,$55,$bf,$ff
	.DC $96,$ff,$ff,$eb,$ff,$cf,$ff,$f3
	.DC $c3,$ff,$c3,$43,$ff,$c1,$3f,$c3
	.DC $fc,$1f,$c3,$f4,$07,$ff,$d0,$8e

joy_fire_data
	.DC $00,$00,$00,$00,$28,$00,$00,$28
	.DC $00,$28,$00,$28,$28,$00,$28,$20
	.DC $00,$08,$00,$00,$00,$00,$00,$00
	.DC $00,$00,$00,$20,$00,$08,$20,$00
	.DC $08,$20,$00,$08,$00,$00,$00,$00
	.DC $00,$00,$00,$00,$00,$20,$00,$08
	.DC $28,$00,$28,$28,$00,$28,$00,$28
	.DC $00,$00,$28,$00,$00,$00,$00,$8a

joy_error_data:
	.DC $07,$ff,$d0,$1f,$ff,$f4,$3f,$ff
	.DC $fc,$7f,$ff,$fd,$ff,$ff,$ff,$ff
	.DC $ff,$ff,$c3,$03,$03,$cf,$33,$33
	.DC $cf,$33,$33,$cf,$03,$03,$c3,$0f
	.DC $0f,$cf,$33,$33,$cf,$33,$33,$cf
	.DC $33,$33,$c3,$33,$33,$ff,$ff,$ff
	.DC $ff,$ff,$ff,$7f,$ff,$fd,$3f,$ff
	.DC $fc,$1f,$ff,$f4,$07,$ff,$d0,$8e
mapa_joy:													; Mapa banków z danymi do sprites
	.DC SPR_J_C, SPR_J_U, SPR_J_D, SPR_J_ERR, SPR_J_L, SPR_J_UL, SPR_J_DL, SPR_J_ERR, SPR_J_R, SPR_J_UR, SPR_J_DR, SPR_J_ERR, SPR_J_ERR, SPR_J_ERR, SPR_J_ERR, SPR_J_ERR
powitanie:
	.DC "*** CPTESTER V6.0 ***",0				; Napisy na ekranie
opis
	.DC "PORT #1     PORT #2",0
wyjscie:
	.DC "PRESS STOP KEY TO EXIT...     ",0
xshift:
	.DC $08
kolor_tla:
	.DC $00
schowek:
	.WORD #0000

; *********** Kod programu *************  
asmstart:  
	lda $d020
	sta kolor_tla
	lda #cls_code				; Czyszczenie ekranu inline
	jsr CHROUT
	ldx #2						; Ustawienie wiersza
	ldy #9						; Ustawienie kolumny
	lda #<powitanie				; Pod adres zero_tmp wrzucany wskaznik do napisu
	sta zero_tmp
	lda #>powitanie
	sta [zero_tmp + 1]
	jsr putmsg_xy
	ldx #5						; Ustawienie wiersza
	ldy #10						; Ustawienie kolumny
	lda #<opis					; Pod adres zero_tmp wrzucany wskaznik do napisu
	sta zero_tmp
	lda #>opis
	sta [zero_tmp + 1]
	jsr putmsg_xy  
	ldx #17						; Ustawienie wiersza
	ldy #8							; Ustawienie kolumny
	lda #<wyjscie				; Pod adres zero_tmp wrzucany wskaznik do napisu
	sta zero_tmp
	lda #>wyjscie
	sta [zero_tmp + 1]
	jsr putmsg_xy
	;ldx #22						; Ustawienie wiersza
	;ldy #8						; Ustawienie kolumny
	;lda #<wyjscie				; Pod adres zero_tmp wrzucany wskaznik do napisu
	;sta zero_tmp
	;lda #>wyjscie
	;sta [zero_tmp + 1]
	;jsr putmsg_xy

; Skopiowanie Sprites do właściwej lokalizacji w pamięci

	ldx #$00				; 256 bajtów do przepisania ! :)
loop_s0:					; petla przepisujaca pod właściwy adres
	lda joy_c_data,x
	sta SPRITES_MEMORY,x
	inx
	bne loop_s0

	ldx #$00				; 256 bajtów do przepisania ! :)
loop_s1:					; petla przepisujaca pod właściwy adres
	lda joy_dr_data,x
	sta [SPRITES_MEMORY+[4*64]],x
	inx
	bne loop_s1

	ldx #$00				; 192 bajty do przepisania ! :)
loop_s2:					; petla przepisujaca pod właściwy adres
	lda joy_ul_data,x
	sta [SPRITES_MEMORY+[8*64]],x
	inx
	cpx #$c0
	bne loop_s2

; Ustawienie wyświetlania Sprites

	lda #SPRITES_MCOLOR1				; sprite multicolor 1
	sta MULTICOLOR_R1
	lda #SPRITES_MCOLOR2				; sprite multicolor 2
	sta MULTICOLOR_R2
	lda #[SPRITE_0 + SPRITE_2 ]			; te sprite widoczne na poczatek
	sta SPRITE_VISIBLE_R

	lda #SPRITE_COLOR1
	sta SPRITE0_COLOR_R					; główny kolor sprite 0
	sta [SPRITE0_COLOR_R+2]				; główny kolor sprite 2

	lda #SPRITE_COLOR2
	sta [SPRITE0_COLOR_R+1]				; główny kolor sprite 1
	sta [SPRITE0_COLOR_R+3]				; główny kolor sprite 3

	lda #SPR_J_C
	sta SPRITE0_POINTER
	sta [SPRITE0_POINTER+2]

	lda #SPR_J_FIRE
	sta [SPRITE0_POINTER+1]
	sta [SPRITE0_POINTER+3]

	lda #JOY_0_X_POS
	sta SPRITE0_X_REG
	sta [SPRITE0_X_REG+2]

	lda #JOY_0_Y_POS
	sta SPRITE0_Y_REG
	sta [SPRITE0_Y_REG+2]

	lda #JOY_1_X_POS
	sta [SPRITE0_X_REG+4]
	sta [SPRITE0_X_REG+6]

	lda #JOY_1_Y_POS
	sta [SPRITE0_Y_REG+4]
	sta [SPRITE0_Y_REG+6]

	lda #[SPRITE_0 + SPRITE_1 + SPRITE_2 + SPRITE_3]	; Dla wszystkich używanych sprites włączamy tryb multicolor
	sta MULTICOLOR_ON

	lda #<mapa_joy						; Młodszy bajt adresu mapy  banków sprite
	sta zero_tmp							; Zapisujemy na strone zerową
	lda #>mapa_joy						; Starszy bajt adresu mapy banków sprite
	sta [zero_tmp +1]						; Zapisujemy na strone zerowa
; Ustawienie przerwania
	sei
	lda #%01111111
	sta $dc0d			;"Switch off" interrupts signals from CIA-1
	sta $dd0d			;"Switch off" interrupts signals from CIA-2
	and $d011
	sta $d011			;Clear most significant bit in VIC's raster register
	lda #20
	sta $d012			;Set the raster line number where interrupt should occur
	lda #<przerwanie_1	; Pod adres zero_tmp wrzucany wskaznik do napisu
	sta $0314
	lda #>przerwanie_1
	sta $0315
	lda $d01a
	ora #%00000001
	sta $d01a			;Enable raster interrupt signals from VIC
	lda $dc0d			;Skasowanie ewentualnych przerwań które się pojawiły w międzyczasie od CIA-1
	lda $dd0d			;Skasowanie ewentualnych przerwań które się pojawiły w międzyczasie od CIA-2
	cli
loop:  											; Pętla główna
; Port 1
	lda port1									; Do akumlatora wartosc portu 1
	eor #$1f
	tax											; Robimy sobie kopie w rej X
	and #$0f									; Zostawiamy najmlodsze 4 bity (4 kierunki JOYA)
	tay											; Wrzucamy to do rej Y, przyda sie do zaindeksowania adresu banku sprite
	lda (zero_tmp),y						; Mamy poprawny bank dla sprite nr 0
	sta SPRITE0_POINTER			; No i go wyswietlamy ! :)
	lda SPRITE_VISIBLE_R			; Do rej A wyswietlane sprite'y
	and #[SPRITE_0 + SPRITE_2 + SPRITE_3]					; Zostawimy wszystkie oprocz sprite 1 (fire dla port 1)
	tay											; Co  sobie zapamietamy w rej Y
	txa											; Do A zapamiętany w X port_1 już zanegowany
	and #$10									; Czy jest aktywny fire ?
	beq aktualizuj_j1
	tya											; Jest aktywny fire - włączamy SPRITE_1
	ora #[SPRITE_1]
	bne gotowy_j1
aktualizuj_j1:
	tya
gotowy_j1:
	sta SPRITE_VISIBLE_R
;Port 2
	lda port2									; Do akumlatora wartosc portu 1
	eor #$1f
	tax											; Robimy sobie kopie w rej X
	and #$0f									; Zostawiamy najmlodsze 4 bity (4 kierunki JOYA)
	tay											; Wrzucamy to do rej Y, przyda sie do zaindeksowania adresu banku sprite
	lda (zero_tmp),y						; Mamy poprawny bank dla sprite nr 0
	sta [SPRITE0_POINTER+2]		; No i go wyswietlamy ! :)
	lda SPRITE_VISIBLE_R			; Do rej A wyswietlane sprite'y
	and #[SPRITE_0 + SPRITE_1+ SPRITE_2]					;Zostawimy wszystkie oprocz sprite 3 (fire dla port 2)
	tay											; Co  sobie zapamietamy w rej Y
	txa											; Do A zapamiętany w X port_1 już zanegowany
	and #$10									; Czy jest aktywny fire ?
	beq aktualizuj_j2
	tya											; Jest aktywny fire - włączamy SPRITE_3
	ora #[SPRITE_3]
	bne gotowy_j2
aktualizuj_j2:
	tya
gotowy_j2:
	sta SPRITE_VISIBLE_R

	lda stop
	cmp #stop_pressed
	beq koniec
	jmp loop

koniec:
	sei
	lda #$31
	sta $0314
	lda #$ea
	sta $0315
	lda #%10000001
	sta $dc0d			;"Switch ON" interrupts signals from CIA-1
	lda $d01a
	and #%11111110
	sta $d01a			;Enable raster interrupt signals from VIC
	cli
	lda #$00
	sta SPRITE_VISIBLE_R
	lda #$07
	sta $d016
	lda kolor_tla
	sta $d020
	lda #cls_code					; Czyszczenie ekranu inline
	jmp CHROUT						; Skok do funkcji KERNALA, ta zakończy się RTS i wyjście do BASICA
  
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

putchar_xy .SUBROUTINE
	pha
	clc
	jsr PLOT
putchar .SUBROUTINE 			; Wypisanie znaku na ekran od aktualnej pozycji kursora
	pla
	jsr CHROUT
	rts

scroll_left .SUBROUTINE			;Przesunięcie wiersza o jeden znak w lewo, po prawej stronie wstawiony pusty znak
	pha
	ldy #$01
.loop:
	lda (zero_tmp),y			;W zero_tmp mamy adres poczatku wiersza na ekranie ktory scrollujey
	dey
	sta (zero_tmp),y
	iny
	iny
	tya
	cmp #40
	bne .loop					;W tej petli mamy przesuniecie o znak w lewo całego wiersza, po prawej stronie dodamy nowy znak
	lda #<wyjscie				; Pod adres zero_tmp wrzucany wskaznik do napisu
	sta zero_tmp
	lda #>wyjscie
	sta [zero_tmp + 1]			; Gotowe

	ldy schowek					;Ładujemy ze schowka indeks napisu (na poczatku 0)
	lda (zero_tmp),y			;Do akumulatora kolejny znak napisu wyjscia
	sta [schowek +1]			;Chowamy go na chwile
	iny
	lda (zero_tmp),y			;Do akumulatora kolejny znak napisu wyjscia
	bne .dalej2
	ldy #0						;Koniec napisu, startujemy od poczatku
.dalej2:
	sty schowek					;Zapamietujemy indeks napisu, dla kolejnego wstawiania po prawej
	pla
	tax
	;ldx #16					; Ustawienie wiersza
	ldy #38						; Ustawienie kolumny
	lda [schowek +1]
	jsr putchar_xy
	rts

przerwanie_1:
	;lda $d011
	;and #%11011111
	;sta $d011				;Enable TXT mode
	inc $d020
	dec xshift
	lda xshift
	and #7					; Tu sprawdzenie, czy wrzucamy nowy znak, jeśli tak, to wywołanie procedury przesunięcia o jeden znak i dopisania nowego znaku
	cmp #7
	bne .dalej
	lda zero_tmp
	pha
	lda [zero_tmp+1]
	pha
	lda #<[$400+(40*22)]	;Do zero_tmp adres początku wiersza który scrollujemy
	sta zero_tmp
	lda #>[$400+(40*22)]
	sta [zero_tmp+1]
	lda #22					; Nr wiersza jeszcze do akumulatora
	jsr scroll_left
	pla
	sta [zero_tmp+1]
	pla
	sta zero_tmp
	lda #7
.dalej:
	sta $d016
	lda #255
	sta $d012				;Set the raster line number where interrupt should occur
	lda #<przerwanie_2
	sta $0314
	lda #>przerwanie_2
	sta $0315
	asl $d019
	jmp $ea81

przerwanie_2:
	;lda $d011
	;and #%11011111
	;sta $d011				;Enable TXT mode
	dec $d020
	lda #7
	sta $d016
	lda #20
	sta $d012				;Set the raster line number where interrupt should occur
	lda #<przerwanie_1
	sta $0314
	lda #>przerwanie_1
	sta $0315
	asl $d019
	jmp $ea31

