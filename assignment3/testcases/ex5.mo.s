
======= MOOL Program =======

class Main{
  void main(Int i,Int a,Int d){
    Int t1;
    Int t2;
    Bool b;
    Compute help;
    i=0;
    a=0;
    d=0;
    help=new Compute();
    t1=[[help.addSquares(a,d)],[help.square(i)]](+);
    t2=[help.square(d)];
    If([t2,t1](>))
    {
      println("Square of d larger than sum of squares");
    }
    else
    {
      println("Square of d smaller than sum of squares");
    }
    While(b)
    {
      t1=[t1,1](+);
      If([t1,10](<=))
      {
        println(t1);
      }
      else
      {
        println("t1 is > 10");
        b=false;
      }
    }
  }

} 

class Compute{

  Int square(Int a){
    Return [a,a](*);
  }


  Int add(Int a,Int b){
    Return [a,b](+);
  }


  Int addSquares(Int a,Int b){
    If([a,b](==))
    {
      Return [add([square(a)],[square(a)])];
    }
    else
    {
      Return [add([square(a)],[square(b)])];
    }
  }

}

======= End of MOOL Program =======



======= MOOL Program =======

class Main{
  void main(Int i,Int a,Int d){
    Int t1;
    Int t2;
    Bool b;
    Compute help;
    (i:Int,2)=0;
    (a:Int,2)=0;
    (d:Int,2)=0;
    (help:Compute,2)=new Compute():Compute;
    (t1:Int,2)=[[(help:Compute,2):Compute.Compute_2((a:Int,2):Int,(d:Int,2):Int)]:Int,[(help:Compute,2):Compute.Compute_0((i:Int,2):Int)]:Int](+):Int;
    (t2:Int,2)=[(help:Compute,2):Compute.Compute_0((d:Int,2):Int)]:Int;
    If([(t2:Int,2):Int,(t1:Int,2):Int](>):Bool)
    {
      println("Square of d larger than sum of squares");
    }
    else
    {
      println("Square of d smaller than sum of squares");
    }
    While((b:Bool,2):Bool)
    {
      (t1:Int,2)=[(t1:Int,2):Int,1](+):Int;
      If([(t1:Int,2):Int,10](<=):Bool)
      {
        println((t1:Int,2):Int);
      }
      else
      {
        println("t1 is > 10");
        (b:Bool,2)=false;
      }
    }
  }

} 

class Compute{

  Int square(Int a){
    Return [(a:Int,2):Int,(a:Int,2):Int](*):Int;
  }


  Int add(Int a,Int b){
    Return [(a:Int,2):Int,(b:Int,2):Int](+):Int;
  }


  Int addSquares(Int a,Int b){
    If([(a:Int,2):Int,(b:Int,2):Int](==):Bool)
    {
      Return [Compute_1([Compute_0((a:Int,2):Int)]:Int,[Compute_0((a:Int,2):Int)]:Int)]:Int;
    }
    else
    {
      Return [Compute_1([Compute_0((a:Int,2):Int)]:Int,[Compute_0((b:Int,2):Int)]:Int)]:Int;
    }
  }

}

======= End of MOOL Program =======


======= IR3 Program =======

======= CData3 ======= 

class Main{
}

class Compute{
}

=======  CMtd3 ======= 

void main(Main this,Int i,Int a,Int d){
  Int t1;
  Int t2;
  Bool b;
  Compute help;
  Int _t1;
  Int _t2;
  Bool _t3;
  Bool _t4;
  i=0;
  a=0;
  d=0;
  help=new Compute();
  _t1=[Compute_2(help,a,d)];
  _t2=[Compute_0(help,i)];
  t1=[_t1,_t2](+);
  t2=[Compute_0(help,d)];
  _t3=[t2,t1](>);
  If([_t3,false](==)) goto 1;
  println("Square of d larger than sum of squares");
  goto 2;
 Label 1:
  println("Square of d smaller than sum of squares");
 Label 2:
 Label 5:
  If((!)[b]) goto 6;
  t1=[t1,1](+);
  _t4=[t1,10](<=);
  If([_t4,false](==)) goto 3;
  println(t1);
  goto 4;
 Label 3:
  println("t1 is > 10");
  b=false;
 Label 4:
  goto 5;
 Label 6:
}

Int Compute_0(Compute this,Int a){
  Int _t13;
  _t13=[a,a](*);
  Return _t13;
}

Int Compute_1(Compute this,Int a,Int b){
  Int _t12;
  _t12=[a,b](+);
  Return _t12;
}

Int Compute_2(Compute this,Int a,Int b){
  Bool _t5;
  Int _t6;
  Int _t7;
  Int _t8;
  Int _t9;
  Int _t10;
  Int _t11;
  _t5=[a,b](==);
  If([_t5,false](==)) goto 7;
  _t6=[Compute_0(this,a)];
  _t7=[Compute_0(this,a)];
  _t8=[Compute_1(this,_t6,_t7)];
  Return _t8;
  goto 8;
 Label 7:
  _t9=[Compute_0(this,a)];
  _t10=[Compute_0(this,b)];
  _t11=[Compute_1(this,_t9,_t10)];
  Return _t11;
 Label 8:
}

======= End of IR3 Program =======


Hash table of locals/parameters
a	12
d	16
last	48
b	28
t2	24
_t4	48
i	8
this	4
t1	20
_t1	36
help	32
_t3	44
_t2	40


Hash table of locals/parameters
a	8
last	12
this	4
_t13	12


Hash table of locals/parameters
a	8
_t12	16
last	16
b	12
this	4


Hash table of locals/parameters
a	8
_t10	36
last	40
_t6	20
_t5	16
b	12
_t11	40
_t8	28
this	4
_t7	24
_t9	32


.data
	
L0:
	.asciz "Square of d larger than sum of squares\n"
	
L1:
	.asciz "Square of d smaller than sum of squares\n"
	
L2:
	.asciz "%i\n"
	
L3:
	.asciz "t1 is > 10\n"
	
	.text
	.global main
	.type main, %function
	
main:
	stmfd sp!,{v1,v2,v3,v4,v5,fp,lr}
	add fp,sp,#28
	sub sp,fp,#76
	mov v1,#0
	str v1,[fp,#-8]
	mov v1,#0
	str v1,[fp,#-12]
	mov v1,#0
	str v1,[fp,#-16]
	mov a1,#0
	bl _Znwj(PLT)
	mov v5,a1
	str v1,[fp,#-32]
	ldr a3,[fp,#-16]
	ldr a2,[fp,#-12]
	ldr a1,[fp,#-32]
	bl Compute_2
	str v1,[fp,#-36]
	ldr a2,[fp,#-8]
	ldr a1,[fp,#-32]
	bl Compute_0
	str v1,[fp,#-40]
	ldr v2,[fp,#-36]
	ldr v3,[fp,#-40]
	add v1,v2,v3
	str v1,[fp,#-20]
	ldr a2,[fp,#-16]
	ldr a1,[fp,#-32]
	bl Compute_0
	str v1,[fp,#-24]
	ldr v2,[fp,#-24]
	ldr v3,[fp,#-20]
	cmp v2,v3
	movgt v1,#1
	movlt v1,#0
	str v1,[fp,#-44]
	ldr v2,[fp,#-44]
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
	
.5:
	ldr v2,[fp,#-28]
	rsb v1,v2,#1
	cmp v1,#1
	beq .6
	ldr v2,[fp,#-20]
	mov v3,#1
	add v1,v2,v3
	str v1,[fp,#-20]
	ldr v2,[fp,#-20]
	mov v3,#10
	cmp v2,v3
	movle v1,#1
	movgt v1,#0
	str v1,[fp,#-48]
	ldr v2,[fp,#-48]
	mov v3,#0
	cmp v2,v3
	moveq v1,#1
	movne v1,#0
	cmp v1,#1
	beq .3
	ldr a1,=L2
	ldr a2,[fp,#-20]
	bl printf(PLT)
	b .4
	
.3:
	ldr a1,=L3
	bl printf(PLT)
	mov v1,#0
	str v1,[fp,#-28]
	
.4:
	b .5
	
.6:
	
.main_exit:
	sub sp,fp,#28
	ldmfd sp!,{v1,v2,v3,v4,v5,fp,pc}
	
Compute_0:
	stmfd sp!,{v1,v2,v3,v4,v5,fp,lr}
	add fp,sp,#28
	sub sp,fp,#40
	ldr v2,[fp,#-8]
	ldr v3,[fp,#-8]
	mul v1,v2,v3
	str v1,[fp,#-12]
	ldr a1,[fp,#-12]
	b .Compute_0_exit
	
.Compute_0_exit:
	sub sp,fp,#28
	ldmfd sp!,{v1,v2,v3,v4,v5,fp,pc}
	
Compute_1:
	stmfd sp!,{v1,v2,v3,v4,v5,fp,lr}
	add fp,sp,#28
	sub sp,fp,#44
	ldr v2,[fp,#-8]
	ldr v3,[fp,#-12]
	add v1,v2,v3
	str v1,[fp,#-16]
	ldr a1,[fp,#-16]
	b .Compute_1_exit
	
.Compute_1_exit:
	sub sp,fp,#28
	ldmfd sp!,{v1,v2,v3,v4,v5,fp,pc}
	
Compute_2:
	stmfd sp!,{v1,v2,v3,v4,v5,fp,lr}
	add fp,sp,#28
	sub sp,fp,#68
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
	beq .7
	ldr a2,[fp,#-8]
	ldr a1,[fp,#-4]
	bl Compute_0
	str v1,[fp,#-20]
	ldr a2,[fp,#-8]
	ldr a1,[fp,#-4]
	bl Compute_0
	str v1,[fp,#-24]
	ldr a3,[fp,#-24]
	ldr a2,[fp,#-20]
	ldr a1,[fp,#-4]
	bl Compute_1
	str v1,[fp,#-28]
	ldr a1,[fp,#-28]
	b .Compute_2_exit
	b .8
	
.7:
	ldr a2,[fp,#-8]
	ldr a1,[fp,#-4]
	bl Compute_0
	str v1,[fp,#-32]
	ldr a2,[fp,#-12]
	ldr a1,[fp,#-4]
	bl Compute_0
	str v1,[fp,#-36]
	ldr a3,[fp,#-36]
	ldr a2,[fp,#-32]
	ldr a1,[fp,#-4]
	bl Compute_1
	str v1,[fp,#-40]
	ldr a1,[fp,#-40]
	b .Compute_2_exit
	
.8:
	
.Compute_2_exit:
	sub sp,fp,#28
	ldmfd sp!,{v1,v2,v3,v4,v5,fp,pc}
