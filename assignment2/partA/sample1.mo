class Main {

Void main(){
 Int t1;
 Int t2;
 return;
 }
}


class A{

 Int t1;
 Int t2;
 Bool b1;
 Void method1(){
 
	Int a;
	return;
 }

 Int test_method1(Int c){
     Bool b;
     if (true) {
         return t1 ;
     }
     else { c = t2 ; }
     while (c > 4+t1) {
         c = c + 1 ;
     }
     return t1;
  }
      

}

class B extends A{
  //  overriding attribute
  Int t1;
  Int s1;

  Void method1(){
       Int b;
       //method1(3);
       return;
  }

  Void method1(Int a){
       Int b;
       A t;

       t.method1();
       if(b>0){
	t.test_method1(b);
	}
	else{
	  method1();
	}
       t=new B();
       ((B)t).method1();
       method1(3);
       b = a + 1 - s1 * t1;
       return;
  }

  Int method2(){
    Int a;
    a=super.t1;
    //method1(3);
    
    return a;
    }
}