package literatureStats;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

public class FrequencyDocumentReader {
    private FrequencyDocumentReader() {
    } // 01/04/2023 updated to have private visibility

    public static final String DEFAULT_NON_WORD_CHARS = "[^a-zA-Z0-9'\\s]+";

    /**
     * Reads the named document file using default settings. Use the
     * defaults for information not provided.
     *
     * @param dictionaryFileName
     * @return
     */
    public static Map<String, FrequencyWord> readDocument(String dictionaryFileName) {

        FrequencyReaderConfig config = new FrequencyReaderConfig(dictionaryFileName,
                FrequencyReaderConfig.EMPTY_MARKER,
                FrequencyReaderConfig.EMPTY_MARKER,
                FrequencyReaderConfig.DEFAULT_VERBOSITY);

        return readDocument(config, DEFAULT_NON_WORD_CHARS);
    }

    /**
     * Reads a document using default settings for those not provided.
     *
     * @param dictionaryFileName
     * @param nonWordChars
     * @return
     */
    public static Map<String, FrequencyWord> readDocument(String dictionaryFileName, String nonWordChars) {

        FrequencyReaderConfig config = new FrequencyReaderConfig(
                dictionaryFileName,
                FrequencyReaderConfig.EMPTY_MARKER,
                FrequencyReaderConfig.EMPTY_MARKER,
                FrequencyReaderConfig.DEFAULT_VERBOSITY);

        return readDocument(config, nonWordChars);
    }

    /**
     * Reads a document using the default set of non-word characters.
     *
     * @param config
     * @return
     */
    public static Map<String, FrequencyWord> readDocument(FrequencyReaderConfig config) {
        return readDocument(config, DEFAULT_NON_WORD_CHARS);
    }

    private static void log(String message, Verbosity verbose) {
        if (verbose.isVerbose()) {
            System.out.println("[FrequencyDocumentReader]:\t" + message);
        }
    }

    public static ArrayList<String> convertStringToList(String sentence) {
        // Could alternatively be written using .stream()
        return new ArrayList<>(List.of(sentence.split("\\s+")));
    }

    /**
     * Reads the file specified in the configuration and obeys the
     * start and stop markers.
     * If the configuration has a non-zero verbosity then print the following messages:
     * if the word is new:
     * Added normalisedWord
     * if the word already exists:
     * Incremented normalisedWord to newCount
     * In both cases substitute normalisedWord with the actual normalised form.
     * If a word already exists print the count that includes the instance you
     * are processing.
     * <p>
     *
     * @param config
     * @param nonWordChars
     * @return
     */
    public static Map<String, FrequencyWord> readDocument(
            FrequencyReaderConfig config, String nonWordChars) {

        boolean started = (config.START_MARKER == null);
        Map<String, FrequencyWord> dictionary = new HashMap<>();

        try (final Scanner sc = new Scanner(new File(config.DOCUMENT_FILENAME))) {
            while (sc.hasNextLine()) {
                String line = sc.nextLine();

                if (config.STOP_MARKER != null && line.contains(config.STOP_MARKER)) {
                    started = false;

                } else if (started) {
                    // Preprocess the line, convert it to words.
                    line = FrequencyWord.normalise(line);
                    line = line.replaceAll(nonWordChars, " ");
                    ArrayList<String> words = convertStringToList(line);

                    for (String word : words) {
                        // Checks for edge cases
                        if (!word.equals("") && !word.equals(" ")) {
                            if (!dictionary.containsKey(word)) {
                                dictionary.put(word, new FrequencyWord(word));
                                log(String.format("Added %s", word), config.getVerbosity());
                            } else {
                                dictionary.get(word).incrementCount();
                                log(String.format("Incremented %s to %d", word, dictionary.get(word).getCount()),
                                        config.getVerbosity());
                            }
                        }
                    }
                } else if (config.START_MARKER != null && line.contains(config.START_MARKER)) {
                    started = true;
                }
            }
        } catch (FileNotFoundException e) {
            throw new RuntimeException(e);
        }
        return dictionary;
    }
}
