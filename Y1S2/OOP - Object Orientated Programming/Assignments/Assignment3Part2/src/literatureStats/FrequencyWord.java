package literatureStats;

import java.util.Set;

/**
 * This is the proper class for storing a word and its frequency count.
 */
public class FrequencyWord implements Comparable<FrequencyWord> {
    public static final String DEFAULT_WORD_STATS_PATTERN = "%4d\t%s%n";

    public static final Set<String> VOWELS = Set.of("a", "e", "i", "o", "u");

    protected final String normalised;

    protected int count;


    FrequencyWord(String word) {
        this.normalised = normalise(word);
        this.count = 1;
    }

    /**
     *  Note this returns a normalised form of the parameter.
     *  Normalise means a word is entirely lowercase and
     *  has no leading or trailing whitespace.
     *
     * @param word the String to be normalised
     * @return
     */
    public static String normalise(String word) {
        return word.toLowerCase().trim();
    }

    /**
     * A getter for the normalised form of the current word.
     *
     * @return
     */
    public String getNormalised() {
        return normalised;
    }


    /**
     * A getter for the current frequency of the current word.
     *
     * @return
     */
    public int getCount() {
        return count;
    }

    public void incrementCount() {
        count++;
    }

    /**
     * Gets a String of this object's data (frequency and normalised word-form)
     *  formatted according to the default word statistics pattern.
     *
     * @return
     */
    @Override
    public String toString() {
        return String.format(DEFAULT_WORD_STATS_PATTERN, getCount(), getNormalised());
    }

    /**
     * Gets a String of this object's data (frequency and normalised word-form)
     *  formatted according to the provided word statistics pattern.
     *
     * @param wordStatePattern
     * @return
     */
    public String toString(String wordStatePattern) {
        return String.format(wordStatePattern, getCount(), getNormalised());
    }

    /**
     * Returns the difference between the two frequencies.
     * If this one is less than the counterpart one, this one will be negative and vice versa.
     * If they are the same, the output will be 0. All of this is in accordance with
     * equals().
     *
     * @param other the object to be compared.
     * @return
     */
    @Override
    public int compareTo(FrequencyWord other) {
        return this.count - other.count;
    }
}
