class Main {

    void main(){
    
        String s;
        A a;

        // create new value instance of A and call a method
        a = new A();
        
        // a attributes are defined
        a.a1 = 1;
        a.a2 = 1;

        // check in ma whether values of a1 and a2 are equal
        a.ma(a.a1, a.a2);
    }
}

class A {
  
    Int a1;
    Int a2;

    void ma(Int arg1, Int arg2) {

      if (arg1 == arg2) {
        println("arg1 value is equal to arg2");
      }
      else {
        println("arg1 value is not equal to arg2");
      }
      return;
    }
}
