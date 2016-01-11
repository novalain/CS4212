/*
Testcase for dynamic objects memory size allocation.
*/

class Main {
    
    void main() {

        // attributes
        A a;
        B b;
        C c;

        // heap when new constructor
        a = new A();
        b = new B();
        c = new C();

        println("Computations terminated.");
    }
}

class A {

    Int a1;
    Int a2;

    void ma() {
        return;
    }
}

class B {
    
    Int b1;
    Int b2;
    A a;

    void mb() {
        return;
    }
}

class C {
    
    Int c1;
    Int c2;
    B b;

    void mc() {
        return;
    }

}