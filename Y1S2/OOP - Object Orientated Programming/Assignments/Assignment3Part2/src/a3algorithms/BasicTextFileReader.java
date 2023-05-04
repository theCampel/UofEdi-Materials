package a3algorithms;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class BasicTextFileReader {
    private BasicTextFileReader() {} // 01/04/2023 updated to have private visibility, do not change

     /* 01/04/2023 updated to remove incorrect comment about being the only code
                   in the package to read a file
      */

    public static String removeChars(String sentence, String charsToRemove){
        return sentence.replaceAll(charsToRemove, " ");
    }

    /**
     * ReadFile: read all the words of a file.
     *  Process lines in order.
     *  Remove charsToDelete.
     *  Split each line into its words.
     *  Add a normalised version of each word but only if it is not already present.
     *  Do not add "words" that are blank.
     *  Sample input: input/*.txt
     *
     * @param filename
     * @return
     */
    public static List<String> readFile(String filename) {
        final String charsToDelete = "[^A-Za-z0-9'\\s]+";

        List<String> allWords = new ArrayList<>();

        try ( final Scanner sc = new Scanner(new File(filename)) ) {
            while ( sc.hasNextLine() ) {
                String line = sc.nextLine();

                line = Normaliser.normalise(line);
                line = removeChars(line, charsToDelete);

                String[] words = line.split("\\s+");

                for (String word : words){
                    if(!word.isBlank()){
                        if(!allWords.contains(word)){
                            allWords.add(word);
                        }
                    }
                }
            }

        } catch ( FileNotFoundException e ) {
            throw new RuntimeException(e);
        }

        return allWords;
    }
}