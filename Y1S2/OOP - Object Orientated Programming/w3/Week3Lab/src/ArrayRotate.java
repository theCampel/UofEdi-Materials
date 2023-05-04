import java.util.Arrays;

public class ArrayRotate {
    public static void main(String[] args) {
        int[] nums = new int[args.length];
        int[] copy = new int[args.length];

        for (int i = 0; i < args.length; i++){
            nums[i] = Integer.parseInt(args[i]);
        }

        for (int i = 0; i<nums.length-1; i++){
            copy[i] = nums[i+1];
        }
        copy[copy.length-1] = nums[0];

        for (int i = 0; i<copy.length; i++){
            System.out.print(copy[i] + " ");
        }
        System.out.println("");

    }
}
