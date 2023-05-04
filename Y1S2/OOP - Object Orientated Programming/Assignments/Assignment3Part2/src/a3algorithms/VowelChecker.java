package a3algorithms;

public class VowelChecker {
    private VowelChecker() {}

    /**
     * isVowel() checks if a String is one of the five English vowels. Efficiently uses Switch cases
     *  "y" is not considered a vowel here. Assume lowercase input.
     *
     * @param s
     * @return
     */
    public static boolean isVowel(String s){
        switch (s){
            case "a":
            case "e":
            case "i":
            case "o":
            case "u":
                return true;
        default:
            return false;
        }
    }
}
