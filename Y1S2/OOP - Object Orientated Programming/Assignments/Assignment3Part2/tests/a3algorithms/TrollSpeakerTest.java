package a3algorithms;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;

class TrollSpeakerTest {

    @Test
    void translateIntoTroll() {
        final String trollish = "grunt";
        assertArrayEquals(new String[]{TrollSpeaker.translateIntoTroll("This"),
                                       TrollSpeaker.translateIntoTroll("that"),
                                       TrollSpeaker.translateIntoTroll("and"),
                                       TrollSpeaker.translateIntoTroll("the"),
                                       TrollSpeaker.translateIntoTroll("other")},
                          new String[]{trollish, trollish, trollish, trollish, trollish});
    }
}