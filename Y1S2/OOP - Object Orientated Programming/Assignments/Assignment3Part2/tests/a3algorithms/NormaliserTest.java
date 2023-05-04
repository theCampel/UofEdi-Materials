package a3algorithms;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;

class NormaliserTest {

    @Test
    void normalise() {
        assertArrayEquals(new String[]{"this", "that", "and", "the", "other"},
                          new String[]{Normaliser.normalise(" This"),
                                       Normaliser.normalise("that"),
                                       Normaliser.normalise("  AND  "),
                                       Normaliser.normalise(" tHe  "),
                                       Normaliser.normalise("OTHer ")});
    }
}