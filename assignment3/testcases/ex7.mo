/*
Tests the nested loops variables access
*/

class Main {
    
    void main() {

    Int a;
    Int b;
    Bool b1;
    Bool b2;
    Bool b3;

    a = 0;
    b = 10;
    b1 = true;
    b2 = true;
    b3 = true;

    if(b1) {
        a = a+1;
        b = b+1;
        println("1st if statement");
        println(a);
        if(b2) {
            a = a+1;
            println("2nd if statement");
            println(a);
            if(b3) {
                a = a+1;
                println("3rd if statement");
                println(a);
                println(b);
            }
            else {
                println("b3 is false");
            }
        }
        else {
            println("b2 is false");
        }
    }
    else {
        println("b1 is false");
    }
    }
}