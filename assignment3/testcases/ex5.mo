class Main {

    void main(Int i, Int a, Int d) {
    
        Int t1;
        Int t2;
        Bool b;
        Compute help;

        i = 0;
        a = 0;
        d = 0;
        help = new Compute();
        t1 = help.addSquares(a,d) + help.square(i);
        t2 = help.square(d);

        if(t2>t1) {
            println("Square of d larger than sum of squares");
        }
        else {
            println("Square of d smaller than sum of squares");
        }
        
        while(b) {
            t1 = t1 + 1;
            if (t1<=10) {
                println(t1);
            }
            else {
                println("t1 is > 10");
                b = false;
            }
        }

    }
}

class Compute {

    Int square(Int a) {
        return a*a;
    }
   
    Int add(Int a, Int b) {
        return a+b;
    }

    Int addSquares(Int a, Int b) {
        if(a==b) {
            return add(square(a),square(a));
        }
        else {
            return add(square(a),square(b));
        }
    }
}
