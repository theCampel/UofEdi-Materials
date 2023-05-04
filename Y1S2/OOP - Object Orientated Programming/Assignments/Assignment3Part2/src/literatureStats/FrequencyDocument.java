package literatureStats;

import java.util.Map;

// updated comment to explain how to use the object properly
/**
 * A FrequencyDocument stores all the words and their respective frequencies of an entire single
 * document or file.
 * <p>
 * Constructors should use initialise but not read the file
 * <p>
 * The correct way to use this object is (in order)
 * construct it
 * initialise it if and only if the default constructor was used
 * read the file
 * <p>
 * Every instance must have a configuration file {@link FrequencyReaderConfig}
 */
public class FrequencyDocument {


    protected Map<String, FrequencyWord> words ;
    protected FrequencyReaderConfig config;

    protected String nonWordChars;

    /**
     * If calling the default constructor, you need to explicitly call one of the
     * initialise() methods before you can use the object safely.
     */
    public FrequencyDocument() {
        this.words        = null; // 01/04/2023 reordered to match declarations
        this.config       = null;
        this.nonWordChars = null; // 01/04/2023 updated: added for consistency
    }

    /**
     * Constructor based on a filename.
     *
     * @param filename
     */
    public FrequencyDocument(String filename) {
        initialise(filename);
    }

    /**
     * Constructor for filename and a pattern specifying characters
     *  that are not allowed in words.
     *
     * @param filename
     * @param nonWordChars
     */
    public FrequencyDocument(String filename, String nonWordChars) {
        initialise(filename, nonWordChars);
    }

    /**
     * Constructor using a configuration object.
     *
     * @param config
     */
    public FrequencyDocument(FrequencyReaderConfig config) {
        initialise(config);
    }

    /**
     * Constructor using a configuration object and a pattern
     *  specifying characters that are not allowed in words.
     *
     * @param config
     * @param nonWordChars
     */
    public FrequencyDocument(FrequencyReaderConfig config, String nonWordChars) {
        initialise(config, nonWordChars);
    }

    /**
     * Set or reset the configuration object.
     *
     * @param config
     */
    public void setConfig(FrequencyReaderConfig config) {
        this.config = config;
    }


    public void initialise(FrequencyReaderConfig config) {
        initialise(config, FrequencyDocumentReader.DEFAULT_NON_WORD_CHARS);
    }

    public void initialise(String filename) {
        FrequencyReaderConfig config = new FrequencyReaderConfig(
                filename,
                FrequencyReaderConfig.EMPTY_MARKER,
                FrequencyReaderConfig.EMPTY_MARKER,
                FrequencyReaderConfig.DEFAULT_VERBOSITY);

        initialise(config);
    }

    public void initialise(String filename, String nonWordChars) {
        FrequencyReaderConfig config = new FrequencyReaderConfig(
                filename,
                FrequencyReaderConfig.EMPTY_MARKER,
                FrequencyReaderConfig.EMPTY_MARKER,
                FrequencyReaderConfig.DEFAULT_VERBOSITY);

        initialise(config, nonWordChars);

    }

    /**
     * initialise based on a configuration and a pattern specifying characters
     *  that are not allowed in words. This version of initialise() needs the
     *  full set of information provided. // I think it's done?
     *
     * @param config
     * @param nonWordChars
     */
    public void initialise(FrequencyReaderConfig config, String nonWordChars) {
        setConfig(config);
        setNonWordChars(nonWordChars);
        readDocument();
    }

    /**
     * Gets the pattern showing the characters that are not allowed in words.
     *
     * @return
     */
    public String getNonWordChars() {
        return nonWordChars;
    }

    /**
     * Sets the pattern of characters that are not allowed in words.
     *
     * @param nonWordChars
     */
    public void setNonWordChars(String nonWordChars) {
        this.nonWordChars = nonWordChars;
    }

    /**
     *  readDocument() calls the helper class's method to read a file.
     */
    public void readDocument() {
        this.words = FrequencyDocumentReader
                .readDocument(config, getNonWordChars());
    }

    /**
     * getStatsNormalisedWords() returns the statistics of normalised words
     *  with the String in the default format provided by the FrequencyWord class.
     *
     * @return
     */
    public String[] getStatsNormalisedWords() {

        String[] normalisedWords = new String[words.size()];


        int i=0;
        // Nifty line that lets you iterate over a HashMap's entries.
        for (Map.Entry<String, FrequencyWord> entry : words.entrySet()){
            normalisedWords[i] = String.format(FrequencyWord.DEFAULT_WORD_STATS_PATTERN,
                    entry.getValue().getCount(),
                    entry.getKey());
            i++;
        }
        return normalisedWords;
    }

    /**
     * getStatsNormalisedWords() returns the statistics of normalised words
     *  with the String in the format provided by the parameter.
     *
     * @param pattern
     * @return
     */
    public String[] getStatsNormalisedWords(String pattern) {
        String[] normalisedWords = new String[words.size()];

        int i=0;
        // Nifty line that lets you iterate over a HashMap's entries.
        for (Map.Entry<String, FrequencyWord> entry : words.entrySet()){
            normalisedWords[i] = String.format(pattern,
                    entry.getValue().getCount(),
                    entry.getKey());
            i++;
        }
        return normalisedWords;
    }

    /**
     * printStatsNormalisedWords() prints the statistics of normalised words
     *  with the String in the default format provided by the FrequencyWord class.
     */
    public void printStatsNormalisedWords() {
        String[] stats = getStatsNormalisedWords();
        for (String stat : stats){
            System.out.println(stat);
        }
    }

    /**
     * printStatsNormalisedWords() prints the statistics of normalised words
     *  with the String in the format provided by the parameter.
     *
     * @param pattern
     */
    public void printStatsNormalisedWords(String pattern) {
        String[] stats = getStatsNormalisedWords(pattern);
        for (String stat : stats){
            System.out.println(stat);
        }
    }
}
