package a3algorithms;

/**
 * Stores a word with its frequency of usage.
 * This word has no knowledge of any other words.
 */
public class SimpleFrequencyWord implements Comparable<SimpleFrequencyWord> {

    protected final String word;

    protected int count;

    SimpleFrequencyWord(String word) {
        this.word = word;
        this.count = 1;
    }

    public String getWord() {
        return word;
    }

    public int getCount() {
        return count;
    }

    public void incrementCount() {
        count++;
    }

    /**
     * toString generates a one-line String according to the pattern
     *              digits right-justified in 4 spaces
     *              tab
     *              the word
     *
     * @return Output could look like "1234   hello" or "  22    howdy"
     */
    @Override
    public String toString() {
        return String.format("%4d\t%s", getCount(), getWord());
    }

    /**
     * toString(String) generates a one-line String of the frequency
     *  then the normalised word according to the supplied pattern.
     *
     * @param wordStatePattern
     * @return
     */
    public String toString(String wordStatePattern) {
        return String.format(wordStatePattern, getCount(), getWord());
    }

    /**
     * The frequencies are compared and the difference between them is calculated.
     * If this one is less than the counterpart one, this one will be negative and vice versa.
     * If they are the same, the output will be 0. All of this is in accordance with
     * equals().
     *
     * @param other the counterpart object to be compared with.
     * @return
     */
    @Override
    public int compareTo(SimpleFrequencyWord other) {
        return this.count - other.count;
    }
}
