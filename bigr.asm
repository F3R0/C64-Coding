
initSpritesIntro:

	.for (var i = 0; i <= 7; i++)  // don't forget to change!!!!
	{	
	lda #337+i
	sta sprtPointer+i
	lda #$6
	sta $d027+i
	}

	lda	#%00111111
	sta $d015

	lda #%00000000
	sta $d01b

	lda #%00000000
	sta $d01c	

    lda #$89
	sta $d00c
	sta $d000
    sta $d004
    sta $d008
	adc #47
	sta $d00e
    sta $d002
    sta $d006
    sta $d00a


	lda #41
	sta $d00d
    sta $d00f

    lda #82
    sta $d001
    sta $d003
    adc #42
    sta $d005
    sta $d007
    adc #42
    sta $d009
    sta $d00b



    lda #%11111111
	sta $d01d
	sta $d017

  RasterIRQHazirla:
      sei
      
      lda #$7f
      sta $dc0d
      sta $dd0d
      
      lda $d01a
      ora #$01
      sta $d01a
      
      lda $d011
      and #$7f
      sta $d011
      
    lda #%00110101		/// no basic or kernal
	sta $01

	lda #rasterPos0
	sta $d012

   	lda #<irq00
	sta $fffe
	lda #>irq00
	sta $ffff

    cli

son:

jsr creditext

jmp son
  


irq00:

        sta irq00a
		stx irq00x
		sty irq00y

       
 
lda #$02
sta $d020

  jsr $1003

lda #$0e
sta $d020

                lsr $d019
		lda #<irq04		/// prepare irq vector
		sta $fffe
		lda #>irq04
		sta $ffff

		lda #rasterPos4 /// set next irq raster position
		sta $d012

     	irq00a_p: lda #$00
		.label irq00a = irq00a_p+1

		irq00x_p: ldx #$00
		.label irq00x = irq00x_p+1

		irq00y_p: ldy #$00
		.label irq00y = irq00y_p+1

		rti

irq04:

        sta irq04a
		stx irq04x
		sty irq04y

lda #$01
sta $d020
     
jsr upscroll

lda #$0e
sta $d020
                lsr $d019

		lda #<irq00		/// prepare irq vector
		sta $fffe
		lda #>irq00
		sta $ffff

                lda #rasterPos0	/// set next irq raster position
		sta $d012

     	irq04a_p: lda #$00
		.label irq04a = irq04a_p+1

		irq04x_p: ldx #$00
		.label irq04x = irq04x_p+1

		irq04y_p: ldy #$00
		.label irq04y = irq04y_p+1

		rti

upscroll:

        lda ydelay    //set out a delay for the upscroll
        cmp #$3		//delayed enough? because upscroll is faster
        beq ydelayok    //compared to side scroll.
        inc ydelay    //increment delay by 1 byte until 8
        rts
ydelayok:
              // are you ready to change the text?
        lda #$00      //reset delay
        sta ydelay
        lda ypos      //set y-position for upwards scroll
        cmp #$10
        bpl doscroll  //if scroll value is under $10 ... call doscroll
        sec           //else, subtract value by 1
        sbc #$01
        sta ypos //for smooth scroll.
        rts
doscroll:
        
        lda #$17 //reset value of y pos for scroll
        sta ypos

//move screen data upwards, one row after another
	
        ldx #$00
puttxt:
		
        .for(var i=5; i<20; i++) {
        lda $440e+i*40,x //take 40 chars from last row
        sta $440e+(i-1)*40,x  //position to row above ...
        }
                inx
                cpx #$b
                bne puttxt

                ldx rtextcount
                ldy #0
scrread:
        lda message,x    //self modifying message ...
        sta $440e+19*40,y
        inx
        iny
        cpy #$b
        bne scrread
        stx rtextcount
        rts

creditext:
        ldy #0
        ldx #0
cycletext:
        lda creditstxt,x    //self modifying message ...
        sta $440a+20*40,y
                cmp #$00 //@ spotted?
                bne skpcycrst
                lda #<creditstxt //reset message now @ is spotted.
                sta cycletext+1
                lda #>creditstxt
                sta cycletext+2
                jmp cycletext

skpcycrst: //standard message found, skip reset!
        inx
        iny
        cpx #20
        bne cycletext        
        lda cycletext+1 //move on to the next row of upscroll
        clc                        //text data. stored in message
        adc #20
        sta cycletext+1
        lda cycletext+2
        adc #$00
        sta cycletext+2

waitafewsec:
        ldy #20
waitloop:
        lda $d012
        cmp #$80
        bne waitloop
        dey
        bne waitloop

rts

colcyc:
            lda colortable3
            sta colortable3+128
            ldx #$0
coloop:     lda colortable3+1,x 
            sta colortable3,x      
            sta $d80e+20*40,x
            inx
            cpx #128
            bne coloop
            rts

rtextcount:
.byte 0

namescount:
.byte 0

bigrend:
 
