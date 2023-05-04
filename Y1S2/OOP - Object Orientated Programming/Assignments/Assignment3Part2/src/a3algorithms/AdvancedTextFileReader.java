package a3algorithms;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class AdvancedTextFileReader {

    public static final String START_MARKER = "**START";
    public static final String STOP_MARKER  = "**STOP";

    private AdvancedTextFileReader() {} // 01/04/2023 updated to have private visibility, do not change

    private static String removeChars(String sentence, String charsToRemove){
        return sentence.replaceAll(charsToRemove, sentence);
    }

    private static String cleanString(String sentence, String charsToRemove){
        sentence = Normaliser.normalise(sentence);
        sentence = removeChars(sentence, charsToRemove);

        return sentence;

    }

    /**
     * advancedReadFile: read all the words of a file between two specific lines.
     *  Works like BasicTextFileReader but with an extra restriction. 01/04/2023  updated
     *  Don't call the BasicTextFileReader code: put all the code in this method. 01/04/2023  updated
     *  Process lines in order.
     *  Skip all lines up to and including the start marker.
     *  Process every line (in order) up to but excluding the stop marker.
     *  Process these lines the same as BasicTextFileReader.readFile().
     *  Sample file: input/advanced-01-portion.txt
     *
     * @param filename
     * @return
     */
    public static List<String> advancedReadFile(String filename) {
        final String charsToDelete = "[^A-Za-z0-9'\\s]+";

        boolean started = false;

        List<String> allWords = new ArrayList<>();

        try ( final Scanner sc = new Scanner(new File(filename)) ) {
            while ( sc.hasNextLine() ) {
                String line = sc.nextLine();

                if(line.contains(STOP_MARKER)){
                    started = false;
                } else if (started) {

                    line = cleanString(line, charsToDelete);

                    String[] lineWords= line.split("\\s+");

                    for (String word : lineWords){
                        if(!word.isBlank()){
                            if(!allWords.contains(word)){
                                allWords.add(word);
                            }
                        }
                    }
                } else if (line.contains(START_MARKER)){
                    started = true;
                }
            }
        } catch ( FileNotFoundException e ) {
            throw new RuntimeException(e);
        }

        return allWords;
    }
}

