public class Distance1 {

    public static void main (String args[]){
        Integer int1 = Integer.parseInt(args[0]);
        Integer int2 = Integer.parseInt(args[1]);

        Integer bigger = Math.max(int1, int2);
        Integer smaller = Math.min(int1, int2);

        System.out.println(bigger-smaller);

    }

}
