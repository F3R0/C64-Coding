intro:

            ldx #0
c64t_loop:    
            lda c64taxt,x     ////ct_loop + 2 (low) - ct_loop + 1 (high)
            sta $4400,x
            inx
            cpx #206
            bne c64t_loop

ldx #0
lda #0
ldy #0

cursor_blink:
            inx
            cpx #4
            beq start_taxt1
            txa
            pha
            lda #$20
            sta $44f0
            ldy #$ff
            ldx #0

cursor_delay:   dex
                bne cursor_delay
                dey
                bne cursor_delay
            lda #$e0
            sta $44f0

            ldy #$ff
            ldx #0

cursor_delay2:   dex
                bne cursor_delay2
                dey
                bne cursor_delay2
            pla
            tax
            jmp cursor_blink

start_taxt1:

ldx #0
jmp ct_loop

beep:
jsr sound

            pla
ct_loop:    
            lda taxt1,x      ////ct_loop + 2 (low) - ct_loop + 1 (high)
            sta $44f0,x
            pha
            inx
            txa
delay:
            ldy #$aa
            ldx #0
d_loop:	
            dex
            bne d_loop
            dey
            bne d_loop
            tax
            cpx #4
            bne beep

start_taxt2:

ldx #0
jmp ct_loop2

beep2:
jsr sound

            pla
ct_loop2:    
            lda taxt2,x      ////ct_loop + 2 (low) - ct_loop + 1 (high)
            sta $44f4,x
            pha
            inx
            txa
delay2:
            ldy #$bb
            ldx #0
d_loop2:	
            dex
            bne d_loop2
            dey
            bne d_loop2
            tax
            cpx #1
            bne beep2

start_taxt3:

ldx #0
jmp ct_loop3


beep3:
jsr sound


            pla
ct_loop3:    
            lda taxt3,x      ////ct_loop + 2 (low) - ct_loop + 1 (high)
            sta $44f5,x
            pha
            inx
            txa
delay3:
            ldy #$90
            ldx #0
d_loop3:	
            dex
            bne d_loop3
            dey
            bne d_loop3
            tax
            cpx #17
            bne beep3

start_taxt4:

ldx #0
jmp ct_loop4
beep4:
jsr sound


            pla
ct_loop4:    
            lda taxt4,x      ////ct_loop + 2 (low) - ct_loop + 1 (high)
            sta $4506,x
            pha
            inx
            txa
delay4:
            ldy #$cc
            ldx #0
d_loop4:	
            dex
            bne d_loop4
            dey
            bne d_loop4
            tax
            cpx #1
            bne beep4           


start_taxt5:

ldx #0
jmp ct_loop5
beep5:
jsr sound
            pla
ct_loop5:    
            lda taxt5,x      ////ct_loop + 2 (low) - ct_loop + 1 (high)
            sta $4507,x
            pha
            inx
            txa
delay5:
            ldy #$80
            ldx #0
d_loop5:	
            dex
            bne d_loop5
            dey
            bne d_loop5
            tax
            cpx #2
            bne beep5

            ldx #0
cursor_blink_end:
            inx
            cpx #4
            beq endintro
            txa
            pha
            lda #$20
            sta $4518
            ldy #$ff
            ldx #0

cursor_delay_end:   dex
                bne cursor_delay_end
                dey
                bne cursor_delay_end
            lda #$e0
            sta $4518

            ldy #$ff
            ldx #0

cursor_delay2_end:   dex
                bne cursor_delay2_end
                dey
                bne cursor_delay2_end
            pla
            tax
            jmp cursor_blink_end


endintro:



