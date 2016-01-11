.data
	
L0:
	.asciz "Computations terminated.\n"
	
	.text
	.global main
	.type main, %function
	
main:
	stmfd sp!,{v1,v2,v3,v4,v5,fp,lr}
	add fp,sp,#28
	sub sp,fp,#44
	mov a1,#8
	bl _Znwj(PLT)
	mov v1,a1
	str v1,[fp,#-8]
	mov a1,#12
	bl _Znwj(PLT)
	mov v1,a1
	str v1,[fp,#-12]
	mov a1,#12
	bl _Znwj(PLT)
	mov v1,a1
	str v1,[fp,#-16]
	ldr a1,=L0
	bl printf(PLT)
	
.main_exit:
	sub sp,fp,#28
	ldmfd sp!,{v1,v2,v3,v4,v5,fp,pc}
	
A_0:
	stmfd sp!,{v1,v2,v3,v4,v5,fp,lr}
	add fp,sp,#28
	sub sp,fp,#32
	b .A_0_exit
	
.A_0_exit:
	sub sp,fp,#28
	ldmfd sp!,{v1,v2,v3,v4,v5,fp,pc}
	
B_0:
	stmfd sp!,{v1,v2,v3,v4,v5,fp,lr}
	add fp,sp,#28
	sub sp,fp,#32
	b .B_0_exit
	
.B_0_exit:
	sub sp,fp,#28
	ldmfd sp!,{v1,v2,v3,v4,v5,fp,pc}
	
C_0:
	stmfd sp!,{v1,v2,v3,v4,v5,fp,lr}
	add fp,sp,#28
	sub sp,fp,#32
	b .C_0_exit
	
.C_0_exit:
	sub sp,fp,#28
	ldmfd sp!,{v1,v2,v3,v4,v5,fp,pc}
