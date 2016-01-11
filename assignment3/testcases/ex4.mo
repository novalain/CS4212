class Main {

    void main(){
    
        Int x;
        Int y;
        Int z;
        Bool b1;
        Bool b2;
        Bool c1;
        Bool c2;
        String s;
        A a;
        A b;

        // instantiate the string
        s = "Hello word!";
        // binary operation
        y = x + z;
        // relational operation
        b1 = x >= y;
        // boolean operation
        b2 = (c1 && c2);

        // PrintStmt3
        println(3);
        println("foo");
        // what is printed is the memory address... need to refer to the variable
        println(s);

        // AssignStmt3
        x = y;

        // create new value instance of A and call a method
        a = new A();
        a.a(1, 2, 3, 4, 5, 6);

        // if else then statement
        if(x<y) {
            return;
        }
        else {
            return;
        }
    }
}

class A {
  
    Int a1;
    Int a2;

    //void a(Int arg1, Int arg2, Int arg3, Int arg4, Int arg5, Int arg6) {

    void ma(Int arg1, Int arg2) {

      println("method a in class A");
      println("\nprinting arg1");
      println(arg1);
      println("\nprinting arg2");
      println(arg2);
      return;
    }
}
