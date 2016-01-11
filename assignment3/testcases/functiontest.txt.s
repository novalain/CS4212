.data

L0:
	.asciz "%i\n"

L1:
	.asciz "%i\n"

L2:
	.asciz "%i\n"

L3:
	.asciz "%i\n"

L4:
	.asciz "%i\n"

L5:
	.asciz "%i\n"

L6:
	.asciz "Hello World\n"

L7:
	.asciz "%i\n"

	.text
	.global main
	.type main, %function

main:
	stmfd sp!,{v1,v2,v3,v4,v5,fp,lr}
	add fp,sp,#28
	sub sp,fp,#56
	mov v1,#21
	str v1,[fp,#-12]
	mov v1,#16
	str v1,[fp,#-24]
	mov v2,#21
	mov v3,#16
	add v1,v2,v3
	str v1,[fp,#-20]
	ldr a1,[fp,#-16]
	mov a2,#2
	mov a3,#4
	bl SomeClass_0
	mov v1,a1
	str v1,[fp,#-28]
	ldr a1,=L0
	mov a2,#10
	bl printf(PLT)
	ldr a1,=L1
	ldr a2,[fp,#-12]
	bl printf(PLT)
	ldr a1,=L2
	ldr a2,[fp,#-24]
	bl printf(PLT)
	ldr a1,=L3
	ldr a2,[fp,#-28]
	bl printf(PLT)
	ldr a1,=L4
	ldr a2,[fp,#-20]
	bl printf(PLT)

.main_exit:
	sub sp,fp,#28
	ldmfd sp!,{v1,v2,v3,v4,v5,fp,pc}

SomeClass_0:
	stmfd sp!,{v1,v2,v3,v4,v5,fp,lr}
	add fp,sp,#28
	sub sp,fp,#44
	ldr a1,=L5
	ldr a2,[fp,#-12]
	bl printf(PLT)
	ldr v2,[fp,#-8]
	ldr v3,[fp,#-12]
	add v1,v2,v3
	str v1,[fp,#-16]
	ldr a1,=L6
	bl printf(PLT)
	ldr a1,=L7
	ldr a2,[fp,#-16]
	bl printf(PLT)
	ldr a1,[fp,#-16]
	b .SomeClass_0_exit

.SomeClass_0_exit:
	sub sp,fp,#28
	ldmfd sp!,{v1,v2,v3,v4,v5,fp,pc}