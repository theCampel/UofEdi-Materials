package a3algorithms;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;

class VowelCheckerTest {

    @Test
    void isVowel() {
        assertArrayEquals(new boolean[]{VowelChecker.isVowel("a"),
                                        VowelChecker.isVowel("e"),
                                        VowelChecker.isVowel("i"),
                                        VowelChecker.isVowel("o"),
                                        VowelChecker.isVowel("u"),
                                        VowelChecker.isVowel("y"),
                                        VowelChecker.isVowel("b"),
                                        VowelChecker.isVowel("r"),
                                        VowelChecker.isVowel("n")},
                          new boolean[]{true, true, true, true, true,
                                        false, false, false, false});
    }
}