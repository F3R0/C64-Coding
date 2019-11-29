bigr:

initSpritesIntro:

	.for (var i = 0; i <= 7; i++)  // don't forget to change!!!!
	{	
	lda #337+i
	sta sprtPointer+i
	lda #$6
	sta $d027+i
	}

	lda	#%11111111
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


	lda #42
	sta $d00d
    sta $d00f

    lda #84
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


screenScrUp:
	jsr presents
	ldy #0
loop:
    lda #$80
    cmp $d012
    bne *-3
    jsr upscroll
	iny
	cpy #255
	bne loop
loop22:
    lda #$80
    cmp $d012
    bne *-3
    jsr upscroll
	dey
	cpy #1
	bne loop22
    jmp bigrend


upscroll:
		
        lda ydelay    //set out a delay for the upscroll
        cmp #$2			//delayed enough? because upscroll is faster
        beq ydelayok    //compared to side scroll.
        inc ydelay    //increment delay by 1 byte until 8
        rts

ydelayok:

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
				
                ldx #$00
scrread:
        lda message,x    //self modifying message ...
        sta $440e+19*40,x
                cmp #$00 //@ spotted?
                bne skipreset
                lda #<message //reset message now @ is spotted.
                sta scrread+1
                lda #>message
                sta scrread+2
                jmp scrread

skipreset: //standard message found, skip reset!
        inx
        cpx #$b //read 40 chars per row
        bne scrread        
        lda scrread+1 //move on to the next row of upscroll
        clc                        //text data. stored in message
        adc #$b
        sta scrread+1
        lda scrread+2
        adc #$00
        sta scrread+2
        rts

presents:    
            lda pretext,x
            sta $440e+20*40,x
            inx
            cpx #11
            bne presents
			rts

bigrend: