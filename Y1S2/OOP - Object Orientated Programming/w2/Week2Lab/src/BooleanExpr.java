public class BooleanExpr {
    public static void main (String args[]){

        Boolean a = Boolean.parseBoolean(args[0]);
        Boolean b = Boolean.parseBoolean(args[1]);

        boolean phi = (!(a && b ) && (a || b )) || ((a && b) || !(a || b));

        System.out.println("A is: " + a);
        System.out.println("B is: " + b);
        System.out.println("Phi is: " + phi);

    }
}
