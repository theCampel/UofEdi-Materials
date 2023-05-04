public class Main {
    public static void main(String[] args) {

        int i = 1;
        int n = Integer.parseInt(args[0]);
        int total = 0;

        while (i <= n){

            total += i;
            i++;

        }
        System.out.println(total);
    }


}