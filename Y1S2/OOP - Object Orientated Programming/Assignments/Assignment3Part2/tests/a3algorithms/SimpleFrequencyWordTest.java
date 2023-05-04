package a3algorithms;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
class SimpleFrequencyWordTest {

    @Test
    void constructor() {
        SimpleFrequencyWord empty = new SimpleFrequencyWord("");
        SimpleFrequencyWord sfw   = new SimpleFrequencyWord("simple");
    }

    private final SimpleFrequencyWord kyle = new SimpleFrequencyWord("kyle");

    @Test
    void getWord() {
        assertEquals("kyle", kyle.getWord());
    }

    @Test
    void getCount() {
        assertEquals(1, kyle.getCount());
    }

    @Test
    void incrementCount() {
        kyle.incrementCount();
        assertEquals(2, kyle.getCount());
        kyle.incrementCount();
        assertEquals(3, kyle.getCount());
    }

    @Test
    void testToString() {
        for ( String minion : new String[]{"kevin", "stuart", "bob"} ) {
            assertEquals(String.format("<1>:<%s>", minion),
                         new SimpleFrequencyWord(minion).toString("<%d>:<%s>"));
        }
    }
}