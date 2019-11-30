///---------------------------------------///
///         Constants & Variables         ///
///---------------------------------------///

.const bitmap 		= $6000
.const bitmapScr	= $4900 //1000
.const bitmapCol	= $4ce8 //1000
.const screen 		= $4400	//1000
.const sprites		= $5100
.const sprtPointer  = screen+$03f8
.const charset		= $4000
.const charsetm		= $5800
.const datablocks	= $9200
.const music 		= $1000
.const musicPlay 	= music+3
.const color 		= $d800

.const rasterPos1	= $ff
.const rasterPos2	= 216
.const rasterPos3	= $fa

.const brdColor 	= $d020
.const bgColor 		= $d021

.const Start 		= $080d

.label firstrow = screen + (22 * 40)
.label secondrow = screen + (23 * 40)

///---------------------------------------///
///              Code Start               ///
///---------------------------------------///

*= charset "Character Set"
.import binary "resources/ferofont.bin"

* = music "Music"
.import binary "resources/1980.dat",2

*= bitmap "Bitmap"
.import binary "resources/dithered.map"

*= bitmapCol "BitmapColors"
.import binary "resources/dithered.col"

*= bitmapScr "Bitmap Screen"
.import binary "resources/dithered.scr"

*= sprites "Sprites"
.import binary "resources/spritechar.dat"

*= charsetm "CharSetMixed"
.import binary "resources/map.bin"


* = $0801 "Basic"
	BasicUpstart(Start)

* = Start "Code"
 
		lda $dd00
        and #$FC	 ///	11111100
        ora #2       ///	00000010 - Bank1: $4000-$7fff
        sta $dd00

		lda #%00010110	//Bitmap: $6000, Screen: $4400
		sta $d018

//#import "type.asm"
/* 
jsr loading
jsr loading
jsr loading
jsr loading
jsr loading
 */
		lda #$0e
		sta brdColor
		lda #$06
		sta bgColor

jsr clearscreen
		jsr music	/// Initialize music @ $1000

#import "bigr.asm"
		


///-----------------------------------------------------------///
///           		   Memory Management      	    		  ///
///-----------------------------------------------------------///


		//				 Chrmem: $5800 */
		


///-----------------------------------------------------------///
///           			Prepare Raster IRQs          		  ///
///-----------------------------------------------------------///

	sei			/// disable interrupts
/* 
	lda #$7f		/// turn off the cia interrupts
	sta $dc0d
	sta $dd0d

	lda $d01a		/// enable raster irq
	ora #$01
	sta $d01a

	lda $d011		/// clear high bit of raster line
	and #$7f
	sta $d011 */

	lda $d011			/// Bitmap Mode
	ora #$20
	sta $d011

	lda #%00110101		/// no basic or kernal
	sta $01

	lda #rasterPos1
	sta $d012

	lda #<irq01
	sta $fffe
	lda #>irq01
	sta $ffff

	lda #$0
	sta $0fff	
	sta $3fff	/// Clear ghost byte
	sta $7fff

	cli			/// re-enable interrupts

		
		jsr image

		lda #$00
		sta brdColor
		sta bgColor

		jsr initSprites
		jsr stableSprites


mainLoop:	
	
    jsr toggletext
    jsr counttext

	jmp mainLoop


irq01:
    	sta irq01a
		stx irq01x
		sty irq01y




		lda #%00111000		//25 Rows
		sta $d011
		lda #%11011000		//40 Columns // Multicolor Mode
		sta $d016

		lsr $d019		/// ack RASTER IRQ


		jsr colorCycle
		jsr spritemovey

		lda #<irq02		/// prepare irq vector
		sta $fffe
		lda #>irq02
		sta $ffff

		lda #rasterPos2	/// set next irq raster position
		sta $d012



    /* 	lda #$05
		sta $d020 */


		irq01a_p: lda #$00
		.label irq01a = irq01a_p+1

		irq01x_p: ldx #$00
		.label irq01x = irq01x_p+1

		irq01y_p: ldy #$00
		.label irq01y = irq01y_p+1

		rti



irq02:

    	sta irq02a
		stx irq02x
		sty irq02y      


		lda #%00010000 // Screen 0400 // Bitmap 2000 // Chars 1800
		sta $d018

		lda #%11001000		//40 Columns // Multicolor Mode
		sta $d016

		lda #%00011000 // 25 Rows
		sta $d011 

		lsr $d019		/// ack RASTER IRQ
		lda #<irq03		/// prepare irq vector
		sta $fffe
		lda #>irq03
		sta $ffff

		lda #rasterPos3	/// set next irq raster position
		sta $d012

		/* lda #$02
		sta $d020 */

       jsr scrollpixel
		jsr musicPlay

		irq02a_p: lda #$00

		.label irq02a = irq02a_p+1

		irq02x_p: ldx #$00
		.label irq02x = irq02x_p+1

		irq02y_p: ldy #$00
		.label irq02y = irq02y_p+1

		rti



irq03:

    	sta irq03a
		stx irq03x
		sty irq03y
	

		ldx #$04
	l1: dex
		bne l1

		lda #%00010011 // Text Mode // 24 Rows
		sta $d011

		ldx #$0e
	l2: dex
		bne l2

		lda #%11011000 // 40 Columns // Multicolor
		sta $d016

	 	lda #%00011011 // Text Mode // 25 Rows
		sta $d011
	
        
		lsr $d019		/// ack RASTER IRQ
		lda #<irq01		/// prepare irq vector
		sta $fffe
		lda #>irq01
		sta $ffff

        

		lda #rasterPos1	/// set next irq raster position
		sta $d012

		lda #%00011110
		sta $d018		 /// Bitmap #6000 // Charmem #7800 //

           


        jsr scrollchr
	


		irq03a_p: lda #$00
		.label irq03a = irq03a_p+1

		irq03x_p: ldx #$00
		.label irq03x = irq03x_p+1

		irq03y_p: ldy #$00
		.label irq03y = irq03y_p+1

		rti


initSprites:

	
	.for (var i = 0; i <= 7; i++)  // don't forget to change!!!!
	{	
	lda #324+i
	sta sprtPointer+i
	lda #$06
	sta $d027+i
	}

	lda #$0e
	sta $d025
	lda #$06
	sta $d026

	lda #%11111111
	sta $d01c

    lda #$80
	sta $d000
	adc #$16
    sta $d002
	adc #$15
    sta $d004
    adc #$15
    sta $d006
    adc #$17
    sta $d008

	lda #%00000000
	sta $d01d
	sta $d017



    
rts



image:

	lda #$00
    ldx #$00

	
	copy:   lda bitmapScr ,x
    		sta screen ,x
    		lda bitmapScr+$100,x
    		sta screen +$100,x
    		lda bitmapScr+$200,x
    		sta screen +$200,x
			lda bitmapScr+$250,x
    		sta screen +$250,x


    		lda bitmapCol ,x
    		sta color ,x
    		lda bitmapCol+$100,x
    		sta color +$100,x
    		lda bitmapCol+$200,x
    		sta color +$200,x
			lda bitmapCol+$250,x
    		sta color +$250,x
    		dex
    		bne copy
    		rts



colorCycle:
	lda colorTable2
	sta colorTable2+40
	ldx #$00
cl:
	lda colorTable2+1,x
	sta colorTable2,x
	sta $db98,x   /// color + 23*40
	sta $db70,x   /// color + 22*40
	lda #02
	sta $dbc0,x   /// color + 23*40
	sta $db48,x   /// color + 22*40
	inx
	cpx #40
	bne cl
	rts

stableSprites:
			lda #26
			sta $d00b
			sta $d00d
			sta $d00f

			lda #62
			sta $d00a
			lda #$ce
			sta $d00c

			lda #226
			sta $d00e

			lda #11
			sta $d02d
			sta $d02e
spritemovey:
/* 
			lda framecountery
			cmp #255
			bne movey

			lda #0
			sta framecountery */
	movey:	ldx framecountery
			.for(var i=0; i<=8; i+=2) {
			lda sinustable,x
			adc #247
			sta $d001+i
			inx
			}
	        inc framecountery
			lda colortable,x
	       	sta $d020
			sta $d027
			sta $d028
			sta $d029
			sta $d02a
			sta $d02b

/* 	lda sinustable
	sta sinustable+60

	ldx #$00
    clo:    
	lda sinustable+1,x
	sta sinustable,x

	inx
	cpx #60
	bne clo */
	rts


toggletext:

    lda textno
    cmp #1
    beq raat
    .for (var i = 0; i <= 4; i++)  // don't forget to change!!!!
	{	
	lda #332+i
	sta sprtPointer+i
	}

    lda #128
	sta $d000

	adc #27
    sta $d002

	adc #25
    sta $d004

    adc #22
    sta $d006

    adc #23
    sta $d008

/* 	lda #%0011111		//sprite x/y stretch
	sta $d01d
	sta $d017 */

    inc textno
    rts

raat:
    .for (var i = 0; i <= 4; i++)  // don't forget to change!!!!
	{	
	lda #324+i
	sta sprtPointer+i
	}

    lda #$80
	sta $d000
	adc #$16
    sta $d002
	adc #$15
    sta $d004
    adc #$15
    sta $d006
    adc #$17
    sta $d008

    dec textno
    rts


    
    
    
    


scrollpixel:
	lda stored016
	sec
	sbc #%00000001
	and #%00000111
    sta stored016
    ora #%00001000  
    sta $d016
	rts

scrollchr:
	lda stored016
	beq scrollchars
	rts

scrollchars:
	ldx #0
scrloop:
	lda firstrow+1,x
	sta firstrow,x
	lda secondrow+1,x
	sta secondrow,x
	inx
	cpx #40
	bne scrloop
getchr: 
	lda text
	cmp #$ff
	bne ct

	lda #<text
	sta getchr+1
	lda #>text
	sta getchr+2
	jmp getchr

ct:	sta firstrow+39
	ora #$40
	sta secondrow+39

	inc getchr+1
	bne skip
	inc getchr+2

skip:

	rts

counttext:  
        lda textcount
        ldy #$ff
        ldx #0
delaym: dex
        bne delaym
        dey
        bne delaym
        inc textcount
        cmp #4
        bne counttext
        lda #0
        sta textcount
        rts

sound:

lda #$ff
sta $d400+24 //volume max
lda #54
sta $d400+1 //hi byte freq voice 1
lda #$05 //instant attack, a little decay
sta $d400+5
lda #%10000000 // noise, gate close
sta $d400+4 //voice 1 control register
lda #%10000001 //noise, gate open
sta $d400+4

rts

loading:
		ldy #0
		ldx #0
loadloop:
		dex
		bne loadloop
		sty brdColor
		dey
		bne loadloop
		rts
		

clearscreen:  ldx #0
        lda #$20
clearloop:   sta $4400,x
        sta $4500,x
        sta $4600,x
        inx
        bne clearloop
		rts

*= datablocks "Datablocks"

	sinustable:
		.byte 4,3,2,1,1,1,1,1,2,2,3,4,5,6,7,7
		.byte 7,7,7,6,5,4,3,2,1,1,1,1,1,1,2,3
		.byte 4,5,6,7,7,7,7,7,6,6,5,3,2,2,1,1
		.byte 1,1,1,2,3,4,5,6,7,7,7,7,7,7,6,5
		.byte 4,3,2,1,1,1,1,1,2,3,4,5,6,6,7,7
		.byte 7,7,7,6,5,4,3,2,1,1,1,1,1,2,2,3
		.byte 4,5,6,7,7,7,7,7,6,5,4,3,2,1,1,1
		.byte 1,1,1,2,3,4,5,6,7,7,7,7,7,6,6,5
		.byte 3,2,2,1,1,1,1,1,2,3,4,5,6,7,7,7
		.byte 7,7,7,6,5,4,3,2,1,1,1,1,1,2,3,4
		.byte 5,6,6,7,7,7,7,7,6,5,4,3,2,1,1,1
		.byte 1,1,2,2,3,4,5,6,7,7,7,7,7,6,5,4
		.byte 3,2,1,1,1,1,1,1,2,3,4,5,6,7,7,7
		.byte 7,7,6,6,5,3,2,2,1,1,1,1,1,2,3,4
		.byte 5,6,7,7,7,7,7,7,6,5,4,3,2,1,1,1
		.byte 1,1,2,3,4,5,6,6,7,7,7,7,7,6,5,4

	colortable:

		.byte 1,1,1,1,1,1,1,1,1
		.byte 7,7,15,15,12,12,11,11,11,1,1,2,2,2,9,9
		.byte 8,8,10,10,7,7,13,13,15,15
		.byte 1,1,1,1,1,1,1
		.byte 7,7,15,15,12,12
		.byte 11,11,11,1,1,6,6,6,4,4,14,14,3,3,15,15

		.byte 5,5,5,5,5,5,5,5,5
		.byte 7,7,15,15,12,12,11,11,11,1,1,2,2,2,9,9
		.byte 8,8,10,10,7,7,13,13,15,15
		.byte 5,5,5,5,5,5,5
		.byte 7,7,15,15,12,12
		.byte 11,11,11,1,1,6,6,6,4,4,14,14,3,3,15,15

		.byte 3,3,3,3,3,3,3,3,3
		.byte 7,7,15,15,12,12,11,11,11,1,1,2,2,2,9,9
		.byte 8,8,10,10,7,7,13,13,15,15
		.byte 3,3,3,3,3,3,3
		.byte 7,7,15,15,12,12
		.byte 11,11,11,1,1,6,6,6,4,4,14,14,3,3,15,15

		.byte 7,7,7,7,7,7,7,7,7
		.byte 7,7,15,15,12,12,11,11,11,1,1,2,2,2,9,9
		.byte 8,8,10,10,7,7,13,13,15,15
		.byte 7,7,7,7,7,7,7
		.byte 7,7,15,15,12,12
		.byte 11,11,11,1,1,6,6,6,4,4,14,14,3,3,15,15

	colorTable2:

		.byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		.byte $0b,$0b,$0c,$0c,$0c,$0c,$0c,$0f
		.byte $0f,$03,$03,$07,$01,$01,$01,$01
		.byte $01,$07,$03,$03,$0c,$0c,$0b,$0b
		.byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b

	framecounterx:
		.byte 0

	framecounterx2:
		.byte 0

	framecountery:
		.byte 0

	spriteoffsettable:
		.byte 0,20,40,60,80

	spritecounter:
		.byte 0
        
stored016:
	.byte 0	

textno:	
	.byte 0

textcount:	
	.byte 0

*=$c000

text:
	.text "     $% retrojen presents $%     raat#06: 'the four horsemen of the apocalypse'     "
	.text "28/29 december 2019 - retrojen headquarters / karakoy / istanbul / turkey     "
	.text "the last meeting of 2019. organization: $% alcofribas $%     "
	.text "alcofribas on the keys... prepare yourself for quick seminars, demo talks, "
	.text "8 bit computers and consoles, surprize game contests, basic, assembler, c "
	.text "and even fpga workshops!     last, but not least... batman demo, vespertino "
	.text "teaser, pinball dreams, more batman demo. as you know, there is no such "
	.text "thing as an ugly girl, just not enough batman demo!      we are always seeking for fresh "
	.text "stuff. don't forget to bring your code, pixelart, demo, intro, $%robe$%, retro "
	.text "gear, joysticks, dinner, drinks and bed with you!      thanks for watching and " 
	.text "see you at the meeting! "
	.text "so... what about raat? what is raat???      people are shocked to discover that "
	.text "this kind of incredibly mysterious question has a non-mysterious answer. "
	.text "as you may know, mystery is a property of questions, not answers.     "
	.text "raat is an abbreviation of ''retrojen akil adamlar toplantisi''' and means "
	.text "''retrojen wise men meeting''. yes, it's that simple.      but... oh my dear! you "
	.text "come across a new, mysterious question now. what could it be?      yes, you're "
	.text "right! what's retrojen? does this ring any bells for anyone? the scenerman "
	.text "always rings twice :) :) :)     retrojen is a retro computer focused "
	.text "group making annual meetings, forum & hardcopy fanzin. and now... the youtube "
	.text "channel is on the way! stay tuned for details!"
	.text "by the way, it called me awake to organise raat, to give thanks to our "
	.text "predecessor 7dx parties for their presence. without them we wouldn't have "
	.text "raat here today.     last, but not least :) thanks to our precious 3d & pixel-art "
	.text "guru f3r0 for this nice invitro, which is also his first scene release.      "
	.text "are you still watching?      what are you waiting for???      $% the four horsemen are "
	.text "waiting for you at retrojen hq $%     take your backpack and come to visit us!"

	.fill 40,$20
	.byte $ff


taxt1:
.text "LOAD"    ////f = $06 - screen koda cevirdi.
taxt2:
.text " "
taxt3:
.text @"\"SINUSTABLES.BIN\""
taxt4:
.text " "
taxt5:
.text ":P"


c64taxt:
///.text "                                            **** commodore 64 basic v2 ****                                              64k ram system  38911 basic bytes free                                         ready."  ////f = $06 - screen koda cevirdi.
.text "                                            **** RAAT #06 : 28.12.2019 ****                                              RAATHQ - MAKEREVI, ISTANBUL / BEYOGLU                                          READY."  ////F = $06 - SCREEN KODA CEVIRDI.

ydelay: 
.byte 0
ypos: 
.byte $17 //always init with $17 for scroller


message:
.text "Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN"
.text "Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN"
.text "Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN"
.text "Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN"
.text "Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN"
.text "Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN"
.text "Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN"
.text "Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN"
.text "Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN"
.byte $ff


creditstext:
.text @"AdaminBiri\$20AGY\$20Akermen\$20Alcofribas\$20Alpyre\$20AmonR\$20Arcane\$20Ari\$20Astron\$20Attilan\$20Axon\$20Bager\$20Beast\$20Blockmind\$20Caisson\$20Cengizermis\$20codewarrior\$20Coze\$20Curt\$20Datura\$20doMiNO\$20Drey\$20eins\$20Endo\$20ExtMode\$20F3R0\$20fullgrim\$20function\$20Gaddar\$20Geos\$20gibraltar\$20Hades\$20Hydrogen\$20Impetigo\$20i_r_on\$20İlkerG\$20ilky\$20Illcare_Barrelers\$20joker\$20Madcat\$20matahari\$20Memrah\$20MineCrafter6860\$20modelist\$20Nightlord\$20Norvax\$20overkill\$20ozkano\$20Peacer\$20Perpetual\$20Ragnor\$20Ref\$20Retromaster\$20Savagery\$20Shax\$20Skate\$20spritus\$20SSG\$20Senol\$20TTalayman\$20Vigo\$20Wisdom\$20Witchdoktor\$20Wizardofwar\$20Wizofwor\$20Wolfiem\$20YavuzG\$20Zer0"
//.text @"\$23AdaminBiri\$20\$23AGY\$20\$23Akermen\$20\$23Alcofribas\$20\$23Alpyre\$20\$23AmonR\$20\$23Arcane\$20\$23Ari\$20\$23Astron\$20\$23Attilan\$20\$23Axon\$20\$23Bager\$20\$23Beast\$20\$23Blockmind\$20\$23Caisson\$20\$23Cengizermis\$20\$23codewarrior\$20\$23Coze\$20\$23Curt\$20\$23Datura\$20\$23doMiNO\$20\$23Drey\$20\$23eins\$20\$23Endo\$20\$23ExtMode\$20\$23F3R0\$20\$23fullgrim\$20\$23function\$20\$23Gaddar\$20\$23Geos\$20\$23gibraltar\$20\$23Hades\$20\$23Hydrogen\$20\$23Impetigo\$20\$23i_r_on\$20\$23lkerG\$20\$23ilky\$20\$23Illcare_Barrelers\$20\$23joker\$20\$23Madcat\$20\$23matahari\$20\$23Memrah\$20\$23MineCrafter6860\$20\$23modelist\$20\$23Nightlord\$20\$23Norvax\$20\$23overkill\$20\$23ozkano\$20\$23Peacer\$20\$23Perpetual\$20\$23Ragnor\$20\$23Ref\$20\$23Retromaster\$20\$23Savagery\$20\$23Shax\$20\$23Skate\$20\$23spritus\$20\$23SSG\$20\$23Senol\$20\$23TTalayman\$20\$23Vigo\$20\$23Wisdom\$20\$23Witchdoktor\$20\$23Wizardofwar\$20\$23Wizofwor\$20\$23Wolfiem\$20\$23YavuzG\$20\$23Zer0"



colortable3:
        .byte 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9
        .byte 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
        .byte 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
        .byte 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
        .byte 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
        .byte 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
        .byte 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
        .byte 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9



/* 
.text "AdaminBiri       "
.text "AGY              "
.text "Akermen          "
.text "Alcofribas       "
.text "Alpyre           "
.text "AmonR            "
.text "Arcane           "
.text "Ari              "
.text "Astron           "
.text "Attilan          "
.text "Axon             "
.text "Bager            "
.text "Beast            "
.text "Blockmind        "
.text "Caisson          "
.text "Cengizermis      "
.text "codewarrior      "
.text "Coze             "
.text "Curt             "
.text "Datura           "
.text "doMiNO           "
.text "Drey             "
.text "eins             "
.text "Endo             "
.text "ExtMode          "
.text "F3R0             "
.text "fullgrim         "
.text "function         "
.text "Gaddar           "
.text "Geos             "
.text "gibraltar        "
.text "Hades            "
.text "Hydrogen         "
.text "Impetigo         "
.text "i_r_on           "
.text "İlkerG           "
.text "ilky             "
.text "Illcare_Barrelers"
.text "joker            "
.text "Madcat           "
.text "matahari         "
.text "Memrah           "
.text "MineCrafter6860  "
.text "modelist         "
.text "Nightlord        "
.text "Norvax           "
.text "overkill         "
.text "ozkano           "
.text "Peacer           "
.text "Perpetual        "
.text "Ragnor           "
.text "Ref              "
.text "Retromaster      "
.text "Savagery         "
.text "Shax             "
.text "Skate            "
.text "spritus          "
.text "SSG              "
.text "Senol            "
.text "TTalayman        "
.text "Vigo             "
.text "Wisdom           "
.text "Witchdoktor      "
.text "Wizardofwar      "
.text "Wizofwor         "
.text "Wolfiem          "
.text "YavuzG           "
.text "Zer0             " */