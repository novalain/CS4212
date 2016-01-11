/*
Such example tries to highlight the stackoverflow issue. Without success...
*/

class Main {
    
    void main() {

        // i,j are the initials values for the Fibonacci
        Int f0;
        Int f1;
        Int f2;
        Int res;
        Int n;
        Fibonacci fib;

        f0 = 0;
        f1 = 1;
        f2 = 0;
        
        res = 0;
        n = 0;
        fib = new Fibonacci();

        res = fib.f(n);
        println(res); // doesn't print anything for res...

        /*
        while(true) {
            f2 = f0+f1;
            f0 = f1;
            f1 = f2;
            println(f2);
        }
        */
    }
}

class Fibonacci {
    
    // Fibonacci sequence: F(n+2) = F(n+1) + F(n)
    Int f(Int n) {
        
        Int i;

        i = 10;
        // println(n);
        return f(n+1)+f(n);
    }
}