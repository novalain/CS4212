.data
	
L0:
	.asciz "Hello World!\n"
	
	.text
	.global main
	.type main, %function
	
main:
	stmfd sp!,{v1,v2,v3,v4,v5,fp,lr}
	add fp,sp,#28
	sub sp,fp,#32
	ldr a1,=L0
	bl printf(PLT)
	b .main_exit
	
.main_exit:
	sub sp,fp,#28
	ldmfd sp!,{v1,v2,v3,v4,v5,fp,pc}
