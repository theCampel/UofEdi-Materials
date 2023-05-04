public class ArrayFront9 {

    public static boolean arrayFront9(int[] nums){
        boolean verify = false;

        int max = nums.length < 4 ? nums.length : 4;

        for(int i = 0; i< max; i++){
            if(9 == nums[i]){
                verify = true;
                break;
            }
        }

        return verify;
    }

    public static void main(String[] args) {

        int[] nums = new int[args.length];
        for (int i = 0; i<nums.length; i++){
            nums[i] = Integer.parseInt(args[i]);
        }

        System.out.println(arrayFront9(nums));

    }

}
