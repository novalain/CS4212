.data
	
L0:
	.asciz "%i\n"
	
	.text
	.global main
	.type main, %function
	
main:
	stmfd sp!,{v1,v2,v3,v4,v5,fp,lr}
	add fp,sp,#28
	sub sp,fp,#56
	mov v1,#0
	str v1,[fp,#-8]
	mov v1,#1
	str v1,[fp,#-12]
	mov v1,#0
	str v1,[fp,#-16]
	mov v1,#0
	str v1,[fp,#-20]
	mov v1,#0
	str v1,[fp,#-24]
	mov a1,#4
	bl _Znwj(PLT)
	mov v5,a1
	str v1,[fp,#-28]
	ldr a2,[fp,#-24]
	ldr a1,[fp,#-28]
	bl Fibonacci_0
	str v1,[fp,#-20]
	ldr a1,=L0
	ldr a2,[fp,#-20]
	bl printf(PLT)
	
.main_exit:
	sub sp,fp,#28
	ldmfd sp!,{v1,v2,v3,v4,v5,fp,pc}
	
Fibonacci_0:
	stmfd sp!,{v1,v2,v3,v4,v5,fp,lr}
	add fp,sp,#28
	sub sp,fp,#52
	ldr v2,[fp,#-8]
	mov v3,#1
	add v1,v2,v3
	str v1,[fp,#-12]
	ldr a2,[fp,#-12]
	ldr a1,[fp,#-4]
	bl Fibonacci_0
	str v1,[fp,#-16]
	ldr a2,[fp,#-8]
	ldr a1,[fp,#-4]
	bl Fibonacci_0
	str v1,[fp,#-20]
	ldr v2,[fp,#-16]
	ldr v3,[fp,#-20]
	add v1,v2,v3
	str v1,[fp,#-24]
	ldr a1,[fp,#-24]
	b .Fibonacci_0_exit
	
.Fibonacci_0_exit:
	sub sp,fp,#28
	ldmfd sp!,{v1,v2,v3,v4,v5,fp,pc}
