.data
	
L0:
	.asciz "arg1 value is equal to arg2\n"
	
L1:
	.asciz "arg1 value is not equal to arg2\n"
	
	.text
	.global main
	.type main, %function
	
main:
	stmfd sp!,{v1,v2,v3,v4,v5,fp,lr}
	add fp,sp,#28
	sub sp,fp,#48
	mov a1,#8
	bl _Znwj(PLT)
	mov v1,a1
	str v1,[fp,#-12]
	mov v1,#1
	ldr v2,[fp,#-12]
	str v1,[v2,#-4]
	mov v1,#1
	ldr v2,[fp,#-12]
	str v1,[v2,#-8]
	ldr v2,[fp,#-12]
	ldr v1,[v2,#-4]
	str v1,[fp,#-16]
	ldr v2,[fp,#-12]
	ldr v1,[v2,#-8]
	str v1,[fp,#-20]
	ldr a1,[fp,#-12]
	ldr a2,[fp,#-16]
	ldr a3,[fp,#-20]
	bl A_0(PLT)
	mov v1,a1
	
.main_exit:
	sub sp,fp,#28
	ldmfd sp!,{v1,v2,v3,v4,v5,fp,pc}
	
A_0:
	stmfd sp!,{v1,v2,v3,v4,v5,fp,lr}
	add fp,sp,#28
	sub sp,fp,#44
	ldr v2,[fp,#-8]
	ldr v3,[fp,#-12]
	cmp v2,v3
	moveq v1,#1
	movne v1,#0
	str v1,[fp,#-16]
	ldr v2,[fp,#-16]
	mov v3,#0
	cmp v2,v3
	moveq v1,#1
	movne v1,#0
	cmp v1,#1
	beq .1
	ldr a1,=L0
	bl printf(PLT)
	b .2
	
.1:
	ldr a1,=L1
	bl printf(PLT)
	
.2:
	b .A_0_exit
	
.A_0_exit:
	sub sp,fp,#28
	ldmfd sp!,{v1,v2,v3,v4,v5,fp,pc}
