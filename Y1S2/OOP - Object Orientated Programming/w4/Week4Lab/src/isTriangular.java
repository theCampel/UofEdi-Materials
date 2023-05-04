import java.util.Scanner;

public class isTriangular {

    public static boolean isTri(double a, double b, double c){
        boolean isTriangle = false;

        if((a+b) > c || (a+c) > b || (b+c)> a){
            isTriangle = true;
        }

        return isTriangle;
    }

    public static void main(String[] args) {
        Scanner stdIn = new Scanner(System.in);

        double a = stdIn.nextDouble();
        double b = stdIn.nextDouble();
        double c = stdIn.nextDouble();

        if (isTri(a,b,c)){
            System.out.printf("%s, %s, %s could be the lengths of the triangle\n", a,b,c);
        } else {
            System.out.println("Not a doctor");
        }
        stdIn.close();
    }

}
