.data
	
L0:
	.asciz "%i\n"
	
L1:
	.asciz "foo\n"
	
L2:
	.asciz "%i\n"
	
L3:
	.asciz "I am in function a of the class A\n"
	
L4:
	.asciz "I am now printing the arguments passed to A.a\n"
	
L5:
	.asciz "%i\n"
	
L6:
	.asciz "%i\n"
	
L7:
	.asciz "%i\n"
	
L8:
	.asciz "%i\n"
	
	.text
	.global main
	.type main, %function
	
main:
	stmfd sp!,{v1,v2,v3,v4,v5,fp,lr}
	add fp,sp,#28
	sub sp,fp,#76
	str v1,[fp,#-36]
	ldr v2,[fp,#-8]
	ldr v3,[fp,#-16]
	add v1,v2,v3
	str v1,[fp,#-12]
	ldr v2,[fp,#-8]
	ldr v3,[fp,#-12]
	cmp v2,v3
	movge v1,#1
	movlt v1,#0
	str v1,[fp,#-20]
	ldr v2,[fp,#-28]
	ldr v3,[fp,#-32]
	and v1,v2,v3
	str v1,[fp,#-24]
	ldr a1,=L0
	mov a2,#3
	bl printf(PLT)
	ldr a1,=L1
	bl printf(PLT)
	ldr a1,=L2
	ldr a2,[fp,#-36]
	bl printf(PLT)
	ldr v1,[fp,#-12]
	str v1,[fp,#-8]
	mov a1,#8
	bl _Znwj(PLT)
	mov v1,a1
	str v1,[fp,#-40]
	ldr a1,[fp,#-40]
	mov a2,#1
	mov a3,#2
	mov a4,#3
	sub sp,sp,#4
	sub sp,sp,#8
	sub sp,sp,#12
	bl A_0(PLT)
	mov v1,a1
	ldr v2,[fp,#-8]
	ldr v3,[fp,#-12]
	cmp v2,v3
	movlt v1,#1
	movge v1,#0
	str v1,[fp,#-48]
	ldr v2,[fp,#-48]
	mov v3,#0
	cmp v2,v3
	moveq v1,#1
	movne v1,#0
	cmp v1,#1
	beq .1
	b .main_exit
	b .2
	
.1:
	b .main_exit
	
.2:
	
.main_exit:
	sub sp,fp,#28
	ldmfd sp!,{v1,v2,v3,v4,v5,fp,pc}
	
A_0:
	stmfd sp!,{v1,v2,v3,v4,v5,fp,lr}
	add fp,sp,#28
	sub sp,fp,#56
	ldr a1,=L3
	bl printf(PLT)
	ldr a1,=L4
	bl printf(PLT)
	ldr a1,=L5
	ldr a2,[fp,#-8]
	bl printf(PLT)
	ldr a1,=L6
	ldr a2,[fp,#-12]
	bl printf(PLT)
	ldr a1,=L7
	ldr a2,[fp,#-16]
	bl printf(PLT)
	ldr a1,=L8
	ldr a2,[fp,#-20]
	bl printf(PLT)
	b .A_0_exit
	
.A_0_exit:
	sub sp,fp,#28
	ldmfd sp!,{v1,v2,v3,v4,v5,fp,pc}
