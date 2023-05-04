import java.util.Arrays;

public class HelloWorld{

    public static void main (String[] args){

        int start = 10; 
        int end = 20;
        int n = 5;

        for (int i = start; i < end; i++){
            if ( i % n == 0){
                System.out.println("Should break now it's at " + n);
                break; // Alternatively, continue -> It's python "Pass" // Ideally use nested conditions

            }
            System.out.println(i);
        }

        double[] array1 = new double[10];
        String[] buildings = {"Build 1", "Build2", "Build3"};

        System.out.println(Arrays.toString(array1));
        System.out.println(buildings[buildings.length-1]);



    }


}