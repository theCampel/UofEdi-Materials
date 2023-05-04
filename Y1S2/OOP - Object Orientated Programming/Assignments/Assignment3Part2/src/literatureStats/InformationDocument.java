package literatureStats;

import java.util.*;
import java.util.stream.Collectors;

/**
 * An InformationDocument combines a {@link FrequencyDocument} or descendant
 * with functionality that lets users of this class gather information about that
 * document so that they can do interesting stuff with that information.
 * <p>
 * This class has a generic parameter so that a FrequencyDocument or descendant
 * can be created using just this class.
 *
 * @param <T>
 * @see DataScientist for examples.
 */
public class InformationDocument<T extends FrequencyDocument> {
    protected final T doc;

    public InformationDocument(Class<T> cls,
                               String filename) throws InstantiationException,
                                                       IllegalAccessException {
        /* Although IntelliJ might complain about parts of the next lines being
           redundant or deprecated, leave them because this is the way that the
           Java developers specify performing this operation for Java 8.
         */
        T d = cls.newInstance();
        this.doc = d;

        this.doc.initialise(filename);
        this.doc.readDocument();
    }

    public Map<String, FrequencyWord> getTopNMapEntries(int n, SortingOrder so){

        Map<String, FrequencyWord> orderedWords = sortByValue(this.doc.words, so);

        Map<String, FrequencyWord> topOrderedWords = new LinkedHashMap<>();

        int i = 0;
        for (Map.Entry<String, FrequencyWord> entry : orderedWords.entrySet()){
            if(i==n){
                break;
            }
            topOrderedWords.put(entry.getKey(), entry.getValue());
            i++;
        }
        return topOrderedWords;
    }

    /**
     * Returns a list of the first N most frequent words (or last N if reversed)
     *  Only return the normalised word forms and nothing else.
     *
     * @param n
     * @param so
     * @return
     */
    public List<String> getTopNWords(int n, SortingOrder so) {

        Map<String, FrequencyWord> topNOrderedEntries = getTopNMapEntries(n, so);
        List<String> words = new ArrayList<>();

        for (Map.Entry<String, FrequencyWord> entry : topNOrderedEntries.entrySet()){
            words.add(entry.getKey());
        }
        return words;
    }

    /**
     * Return a short-list of the first N most frequent words (or last N if reversed)
     *  with each prefixed by its frequency, all formatted as a String using
     *  the default word statistics pattern.
     *
     * @param n
     * @param so
     * @return
     */
    public List<String> getTopNWordsEnumerated(int n, SortingOrder so) {
        Map<String, FrequencyWord> topNOrderedEntries = getTopNMapEntries(n, so);

        List<String> words = new ArrayList<>();

        for (Map.Entry<String, FrequencyWord> entry : topNOrderedEntries.entrySet()){
            words.add(String.format(FrequencyWord.DEFAULT_WORD_STATS_PATTERN,
                    entry.getValue().getCount(),entry.getKey()));
        }
        return words;
    }

    /**
     * return a short-list of the first N most frequent words (or last N if reversed).
     *  Returns the entire {@link FrequencyWord} object for each.
     *
     * @param n
     * @param so
     * @return
     */
    public List<FrequencyWord> getTopNFrequencyWords(int n, SortingOrder so) {
        // This consistently failed the tests, but produces the same as Brian?

        // Also a different nifty way to stream all the values
        List<FrequencyWord> words = sortByValue(this.doc.words, SortingOrder.DESCENDING)
                                                .values()
                                                .stream()
                                                .limit(n)
                                                .collect(Collectors.toList());
        if(!so.isReversed()){
            Collections.reverse(words);
        }

        return words;

    }

    /**
     * Sort the document word list by word frequency. DO NOT CHANGE THIS.
     * This method is written in a way that supports generics and could be placed in a utility class.
     * Adapted from <a href="https://stackoverflow.com/a/2581754">this answer</a>
     * and <a href="https://stackoverflow.com/a/42535164">this answer</a> on
     * StackOverflow.
     *
     * @param map   the dictionary to be sorted
     * @param order whether to sort in ascending or descending order
     * @param <K>   the key is a String of a normalised version of the word
     * @param <V>   the value is an Integer of the word's frequency
     * @return an appropriately sorted version of the word list
     */
    public <K, V extends Comparable<? super V>> Map<K, V> sortByValue(
            Map<K, V> map, SortingOrder order) {
        List<Map.Entry<K, V>> list = new ArrayList<>(map.entrySet());
        if ( order.isReversed() ) {
            list.sort(Map.Entry.<K, V>comparingByValue().reversed());
        } else {
            list.sort(Map.Entry.comparingByValue());
        }

        Map<K, V> result = new LinkedHashMap<>();
        for ( Map.Entry<K, V> entry : list ) {
            result.put(entry.getKey(), entry.getValue());
        }

        return result;
    }
}
