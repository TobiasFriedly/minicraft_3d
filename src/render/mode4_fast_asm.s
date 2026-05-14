    .section .iwram, "ax", %progbits
    .align 2
    .arm

    .global mode4_fill_span_asm
    .type mode4_fill_span_asm, %function
mode4_fill_span_asm:
    stmfd sp!, {r4-r7, lr}
    cmp r2, #0
    ble 2f

    and r3, r3, #255
    orr r4, r3, r3, lsl #8
    orr r5, r4, r4, lsl #16
    mov r6, r1, lsr #1
    add r0, r0, r6, lsl #1

    tst r1, #1
    beq 0f
    ldrh r6, [r0]
    and r6, r6, #255
    orr r6, r6, r3, lsl #8
    strh r6, [r0], #2
    subs r2, r2, #1
    ble 2f
0:
    mov r6, r2, lsr #1
    and r2, r2, #1
    tst r0, #3
    beq 1f
    cmp r6, #0
    beq 1f
    strh r4, [r0], #2
    subs r6, r6, #1
1:
    mov r7, r6, lsr #1
    cmp r7, #0
    beq 3f
4:
    str r5, [r0], #4
    subs r7, r7, #1
    bne 4b
3:
    tst r6, #1
    beq 5f
    strh r4, [r0], #2
5:
    cmp r2, #0
    beq 2f
    ldrh r6, [r0]
    bic r6, r6, #255
    orr r6, r6, r3
    strh r6, [r0]
2:
    ldmfd sp!, {r4-r7, pc}

    .section .iwram, "ax", %progbits
    .align 2
    .arm

    .global mode4_draw_span_affine_asm
    .type mode4_draw_span_affine_asm, %function
mode4_draw_span_affine_asm:
    stmfd sp!, {r4-r11, lr}
    ldr r4, [sp, #36]
    ldr r5, [sp, #40]
    ldr r6, [sp, #44]
    ldr r7, [sp, #48]
    cmp r2, #0
    ble 18f

    mov r8, r1, lsr #1
    add r8, r0, r8, lsl #1

	    cmp r6, #0
	    beq 19f
	
	    tst r1, #1
	    beq 6f
    mov r9, r3, lsr #16
    and r9, r9, #15
    mov r10, r4, lsr #12
    and r10, r10, #240
    orr r9, r9, r10
    ldrb r11, [r7, r9]
    ldrh r10, [r8]
    and r10, r10, #255
    orr r10, r10, r11, lsl #8
    strh r10, [r8], #2
    add r3, r3, r5
    add r4, r4, r6
    subs r2, r2, #1
    ble 18f
6:
    mov r9, r2, lsr #1
    and r2, r2, #1
    tst r8, #3
    beq 8f
    cmp r9, #0
    beq 8f

    mov r10, r3, lsr #16
    and r10, r10, #15
    mov r11, r4, lsr #12
    and r11, r11, #240
    orr r10, r10, r11
    ldrb r10, [r7, r10]
    add r3, r3, r5
    add r4, r4, r6

    mov r11, r3, lsr #16
    and r11, r11, #15
    mov r0, r4, lsr #12
    and r0, r0, #240
    orr r11, r11, r0
    ldrb r11, [r7, r11]
    add r3, r3, r5
    add r4, r4, r6

    orr r10, r10, r11, lsl #8
    strh r10, [r8], #2
    subs r9, r9, #1
8:
    mov r10, r9, lsr #1
    cmp r10, #0
    beq 10f
9:
    mov r11, r3, lsr #16
    and r11, r11, #15
    mov r0, r4, lsr #12
    and r0, r0, #240
    orr r11, r11, r0
    ldrb r11, [r7, r11]
    add r3, r3, r5
    add r4, r4, r6

    mov r1, r3, lsr #16
    and r1, r1, #15
    mov r0, r4, lsr #12
    and r0, r0, #240
    orr r1, r1, r0
    ldrb r1, [r7, r1]
    add r3, r3, r5
    add r4, r4, r6

    orr r11, r11, r1, lsl #8

    mov r1, r3, lsr #16
    and r1, r1, #15
    mov r0, r4, lsr #12
    and r0, r0, #240
    orr r1, r1, r0
    ldrb r1, [r7, r1]
    add r3, r3, r5
    add r4, r4, r6

    mov r0, r3, lsr #16
    and r0, r0, #15
    mov r12, r4, lsr #12
    and r12, r12, #240
    orr r0, r0, r12
    ldrb r0, [r7, r0]
    add r3, r3, r5
    add r4, r4, r6

    orr r1, r1, r0, lsl #8
    orr r11, r11, r1, lsl #16
    str r11, [r8], #4
    subs r10, r10, #1
    bne 9b
10:
    tst r9, #1
    beq 12f
    mov r10, r3, lsr #16
    and r10, r10, #15
    mov r11, r4, lsr #12
    and r11, r11, #240
    orr r10, r10, r11
    ldrb r10, [r7, r10]
    add r3, r3, r5
    add r4, r4, r6

    mov r11, r3, lsr #16
    and r11, r11, #15
    mov r0, r4, lsr #12
    and r0, r0, #240
    orr r11, r11, r0
    ldrb r11, [r7, r11]
    add r3, r3, r5
    add r4, r4, r6

    orr r10, r10, r11, lsl #8
    strh r10, [r8], #2
12:
    cmp r2, #0
    beq 18f
    mov r10, r3, lsr #16
    and r10, r10, #15
    mov r11, r4, lsr #12
    and r11, r11, #240
    orr r10, r10, r11
    ldrb r10, [r7, r10]
    ldrh r11, [r8]
    bic r11, r11, #255
    orr r11, r11, r10
    strh r11, [r8]
18:
    ldmfd sp!, {r4-r11, pc}

19:
	    mov r12, r4, lsr #12
	    and r12, r12, #240
	    add r7, r7, r12
	
	    tst r1, #1
	    beq 20f
	    mov r9, r3, lsr #16
	    and r9, r9, #15
	    ldrb r11, [r7, r9]
    ldrh r10, [r8]
    and r10, r10, #255
    orr r10, r10, r11, lsl #8
    strh r10, [r8], #2
    add r3, r3, r5
    subs r2, r2, #1
    ble 18b
20:
    mov r9, r2, lsr #1
    and r2, r2, #1
    tst r8, #3
    beq 22f
    cmp r9, #0
    beq 22f

	    mov r10, r3, lsr #16
	    and r10, r10, #15
	    ldrb r10, [r7, r10]
    add r3, r3, r5

	    mov r11, r3, lsr #16
	    and r11, r11, #15
	    ldrb r11, [r7, r11]
    add r3, r3, r5

    orr r10, r10, r11, lsl #8
    strh r10, [r8], #2
    subs r9, r9, #1
22:
    mov r10, r9, lsr #1
    cmp r10, #0
    beq 24f
23:
	    mov r11, r3, lsr #16
	    and r11, r11, #15
	    ldrb r11, [r7, r11]
    add r3, r3, r5

	    mov r1, r3, lsr #16
	    and r1, r1, #15
	    ldrb r1, [r7, r1]
    add r3, r3, r5

    orr r11, r11, r1, lsl #8

	    mov r1, r3, lsr #16
	    and r1, r1, #15
	    ldrb r1, [r7, r1]
    add r3, r3, r5

	    mov r0, r3, lsr #16
	    and r0, r0, #15
	    ldrb r0, [r7, r0]
    add r3, r3, r5

    orr r1, r1, r0, lsl #8
    orr r11, r11, r1, lsl #16
    str r11, [r8], #4
    subs r10, r10, #1
    bne 23b
24:
    tst r9, #1
    beq 26f
	    mov r10, r3, lsr #16
	    and r10, r10, #15
	    ldrb r10, [r7, r10]
    add r3, r3, r5

	    mov r11, r3, lsr #16
	    and r11, r11, #15
	    ldrb r11, [r7, r11]
    add r3, r3, r5

    orr r10, r10, r11, lsl #8
    strh r10, [r8], #2
26:
    cmp r2, #0
    beq 18b
	    mov r10, r3, lsr #16
	    and r10, r10, #15
	    ldrb r10, [r7, r10]
    ldrh r11, [r8]
    bic r11, r11, #255
	    orr r11, r11, r10
	    strh r11, [r8]
	    b 18b
