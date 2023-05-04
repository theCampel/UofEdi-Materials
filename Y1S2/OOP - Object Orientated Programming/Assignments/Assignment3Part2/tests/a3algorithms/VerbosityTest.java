package a3algorithms;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class VerbosityTest {

    @Test
    void getVerbosityLevel() {
        assertArrayEquals(new int[]{0, 1, 5, 9},
                          new int[]{Verbosity.SILENT.getVerbosityLevel(),
                                    Verbosity.MINIMUM.getVerbosityLevel(),
                                    Verbosity.MEDIUM.getVerbosityLevel(),
                                    Verbosity.MAXIMUM.getVerbosityLevel()});
    }

    @Test
    void isVerbose() {
        assertArrayEquals(new boolean[]{false, true, true, true},
                          new boolean[]{Verbosity.SILENT.isVerbose(),
                                        Verbosity.MINIMUM.isVerbose(),
                                        Verbosity.MEDIUM.isVerbose(),
                                        Verbosity.MAXIMUM.isVerbose()});
    }
}