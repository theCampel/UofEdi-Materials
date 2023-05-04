public class App {
    public static void main(String[] args) {
    
        printFrequencies(new int[]{4,3,2,1,0,1,2,3,4,5});
        System.out.println();     
    }  


    public static void printFrequencies(int[] input) {
        int i = 0;
        for(int number : input){

            System.out.printf("%02d" + ": " + "%2d" + " = <" + "*".repeat(number) + ">\n", i, number);
            i++;

        }  


    }

}