package a3algorithms;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AdvancedTextFileReaderTest {

    @Test
    void advancedReadFile() {
        List<String> empty = new ArrayList<>();

        assertEquals(empty,
                     AdvancedTextFileReader
                             .advancedReadFile("input/00-empty.txt"));
        assertEquals(empty,
                     AdvancedTextFileReader
                             .advancedReadFile("input/01-one-word-per-line.txt"));

        assertEquals(Arrays.asList("keep", "1", "keep2", "3", "4", "5", "6"),
                     AdvancedTextFileReader.advancedReadFile(
                             "input/advanced-01-portion.txt"));
    }
}