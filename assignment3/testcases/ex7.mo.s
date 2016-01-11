.data
	
L0:
	.asciz "1st if statement\n"
	
L1:
	.asciz "%i\n"
	
L2:
	.asciz "2nd if statement\n"
	
L3:
	.asciz "%i\n"
	
L4:
	.asciz "3rd if statement\n"
	
L5:
	.asciz "%i\n"
	
L6:
	.asciz "%i\n"
	
L7:
	.asciz "b3 is false\n"
	
L8:
	.asciz "b2 is false\n"
	
L9:
	.asciz "b1 is false\n"
	
	.text
	.global main
	.type main, %function
	
main:
	stmfd sp!,{v1,v2,v3,v4,v5,fp,lr}
	add fp,sp,#28
	sub sp,fp,#52
	mov v1,#0
	str v1,[fp,#-8]
	mov v1,#10
	str v1,[fp,#-12]
	mov v1,#1
	str v1,[fp,#-16]
	mov v1,#1
	str v1,[fp,#-20]
	mov v1,#1
	str v1,[fp,#-24]
	ldr v2,[fp,#-16]
	mov v3,#0
	cmp v2,v3
	moveq v1,#1
	movne v1,#0
	cmp v1,#1
	beq .5
	ldr v2,[fp,#-8]
	mov v3,#1
	add v1,v2,v3
	str v1,[fp,#-8]
	ldr v2,[fp,#-12]
	mov v3,#1
	add v1,v2,v3
	str v1,[fp,#-12]
	ldr a1,=L0
	bl printf(PLT)
	ldr a1,=L1
	ldr a2,[fp,#-8]
	bl printf(PLT)
	ldr v2,[fp,#-20]
	mov v3,#0
	cmp v2,v3
	moveq v1,#1
	movne v1,#0
	cmp v1,#1
	beq .3
	ldr v2,[fp,#-8]
	mov v3,#1
	add v1,v2,v3
	str v1,[fp,#-8]
	ldr a1,=L2
	bl printf(PLT)
	ldr a1,=L3
	ldr a2,[fp,#-8]
	bl printf(PLT)
	ldr v2,[fp,#-24]
	mov v3,#0
	cmp v2,v3
	moveq v1,#1
	movne v1,#0
	cmp v1,#1
	beq .1
	ldr v2,[fp,#-8]
	mov v3,#1
	add v1,v2,v3
	str v1,[fp,#-8]
	ldr a1,=L4
	bl printf(PLT)
	ldr a1,=L5
	ldr a2,[fp,#-8]
	bl printf(PLT)
	ldr a1,=L6
	ldr a2,[fp,#-12]
	bl printf(PLT)
	b .2
	
.1:
	ldr a1,=L7
	bl printf(PLT)
	
.2:
	b .4
	
.3:
	ldr a1,=L8
	bl printf(PLT)
	
.4:
	b .6
	
.5:
	ldr a1,=L9
	bl printf(PLT)
	
.6:
	
.main_exit:
	sub sp,fp,#28
	ldmfd sp!,{v1,v2,v3,v4,v5,fp,pc}
