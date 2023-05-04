package a3algorithms;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;

class BasicTextFileReaderTest {

    @Test
    void readFile() {
        assertEquals(BasicTextFileReader.readFile("input/00-empty.txt"),
                     new ArrayList<String>());

        assertEquals(
                Arrays.asList("one", "word", "per", "line", "no", "whitespace",
                              "punctuation", "capitals", "fun"),
                BasicTextFileReader.readFile("input/01-one-word-per-line.txt"));
        for ( String word : BasicTextFileReader.readFile(
                "input/07-putting-it-all-together.txt") ) {

            System.out.printf("\"%s\", ", word);
        }

        assertEquals(Arrays.asList("finally", "we're", "putting", "it", "all",
                                   "together", "so", "welcome", "to", "lines",
                                   "with", "only", "one", "word", "blank", "at",
                                   "the", "end", "of", "file", "leading", "and",
                                   "trailing", "whitespace", "multiple",
                                   "spaces", "between", "words", "wrods",
                                   "spelling", "mistakes", "spilling", "mis",
                                   "takes", "you", "don't", "have", "handle",
                                   "these", "as", "just", "genuine",
                                   "celebrate", "too", "soon", "check", "how",
                                   "your", "processing", "is", "handling",
                                   "like", "see", "punctuation", "handled",
                                   "solve", "problems", "be", "aware",
                                   "entertaining", "input", "can", "do", "need",
                                   "basic", "situation", "ignoring", "white",
                                   "space", "no", "matter", "extreme",
                                   "deleting", "different", "orthography",
                                   "for", "same", "treat", "misspellings",
                                   "new", "happy", "best", "ever"),
                     BasicTextFileReader.readFile(
                             "input/07-putting-it-all-together.txt"));
    }
}