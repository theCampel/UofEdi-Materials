package a3algorithms;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class SortingOrderTest {

    @Test
    void isReversed() {
        assertArrayEquals(new boolean[]{false, false, true, false, true}, // updated 02/04/2023 to swap indices 2 and 3
                          new boolean[]{SortingOrder.NORMAL.isReversed(),
                                        SortingOrder.NOT_REVERSED.isReversed(),
                                        SortingOrder.DESCENDING.isReversed(),
                                        SortingOrder.ASCENDING.isReversed(),
                                        SortingOrder.REVERSED.isReversed()});
    }
}