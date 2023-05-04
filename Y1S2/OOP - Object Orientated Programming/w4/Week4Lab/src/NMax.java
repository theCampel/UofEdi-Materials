import java.util.Scanner;

public class NMax {
    public static int max(int a, int b, int c){
        int maxi = a;


        if(b > a){
            maxi = b;
        }
        if(c > a && c > b){
            maxi = c;
        }

        return maxi;
    }

    public static void main(String[] args) {
        Scanner stdIn = new Scanner(System.in);

        int a = stdIn.nextInt();
        int b = stdIn.nextInt();
        int c = stdIn.nextInt();

        System.out.println(max(a,b,c));

    }
}
