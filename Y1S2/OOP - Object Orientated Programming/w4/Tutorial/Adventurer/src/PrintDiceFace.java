import java.util.Scanner;

/**
 * todo JavaDoc class
 */
public class PrintDiceFace {
    /**
     * Takes input from a scanner and prints a generated dice face of ASCII Art
     *
     */
    public static void main(String[] args) {

        Scanner scan = new Scanner(System.in);

        int givenInt = 0;
        while (!(givenInt >= 1 && givenInt <= 6)){
            System.out.printf("Provide an integer between 1-6: ");
            givenInt = scan.nextInt();

        }

        System.out.println("The Int provided was " + givenInt);


        // Beginning of pretty printing the dice
        System.out.println("________");

        if(givenInt == 1){
            System.out.println("|      |");
            System.out.println("|  0   |");
            System.out.println("|      |");
        }
        else if(givenInt == 2){
            System.out.println("|    0 |");
            System.out.println("|      |");
            System.out.println("| 0    |");
        }
        else if(givenInt == 3){
            System.out.println("|   0  |");
            System.out.println("|   0  |");
            System.out.println("|   0  |");
        }
        else if(givenInt == 4){
            System.out.println("| 0  0 |");
            System.out.println("|      |");
            System.out.println("| 0  0 |");
        }
        else if(givenInt == 5){
            System.out.println("| 0  0 |");
            System.out.println("|   0  |");
            System.out.println("| 0  0 |");
        }
        else if(givenInt == 6){
            System.out.println("| 0  0 |");
            System.out.println("| 0  0 |");
            System.out.println("| 0  0 |");
        }
        System.out.println("¯¯¯¯¯¯¯¯");

    }
}

/* Done 1 Open the to do tab to help you see what needs doing.
            You can rename these comments to remove them from the list.
            This is better than deleting them because you can see what did need
            doing. Alternatively add "DONE" to the comment.
*/
// Done 2 Make multiple designs on paper
// Done 3 Compare designs
/* todo 4 Upload design(s) as images into the design folder,
            keeping file size small
*/
// todo 5 Code, test, and refine a design
// todo 6 Add comments and JavaDoc comments
/* todo 7 Generate the JavaDocs from Tools > Generate JavaDoc...
            Save in the javadoc directory
            Open javadoc/index.html in IntelliJ and use the in-built browser
            to view (the IJ logo that is left-most in the list of browsers)
*/
// todo 8 Optionally, start filling in writing.md
// todo 9 Maybe code, test, and refine a different design for comparison
// todo 10 Complete the reflections.md file