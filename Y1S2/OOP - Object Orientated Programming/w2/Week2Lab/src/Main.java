import java.util.Arrays;

import javax.print.event.PrintEvent;

public class Main {


    public static void printBiggest(double num1, double num2, double num3) {
        if (num1 == num2 && num2 == num3){
            System.out.println("The numbers are the same");
        } else {
            double max = num1;
            
            if(num2 > max){
                max = num2;
            } 
            if (num3 > max) {
                max = num3;
            }
            System.out.printf("The biggest numer is %.1f", max); 
        }
    }

    public static void bubbleSort(int[] arr){
        int n = arr.length;
        for (int i = 0; i < n - 1; i++) {
            for (int j = 0; j < n - i - 1; j++) {
                if (arr[j] > arr[j + 1]) {
                    int temp = arr[j];
                    arr[j] = arr[j + 1];
                    arr[j + 1] = temp;
                }
                System.out.println(Arrays.toString(arr));
            }
        }
    }

    public static void main(String[] args) {

        bubbleSort(new int[] {4,7,3,1,2});
    
    }
}