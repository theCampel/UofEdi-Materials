import java.lang.reflect.Array;
import java.util.Arrays;

public class Main {

    public static int[][] nbyn(int N){

        int[][] matrix = new int[N][N];

        for(int i = 0; i<N; i++){

            matrix[i][i] = i;
        }

        return matrix;

    }
    public static void main(String[] args) {

        int[][] matrix = nbyn(8);
        for (int[] matric : matrix){
            for(int m : matric){
                System.out.print(m);
            }
            System.out.println();
        }

        System.out.println(Arrays.toString(nbyn(9)));

    }
}