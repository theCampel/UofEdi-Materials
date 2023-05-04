package literatureStats;

/**
 * Code to translate words according to the language is directly inside this enum.
 */
public enum Translation {
    NONE {@Override public String translate(String word) {return word;}},

    TROLL {@Override public String translate(String word) {return "grunt";}},

    DOG {

        public String getFirstNonVowelChunk(String word){
            String vowels = "aeiou";
            int index = -1;

            // Iterates over the vowels until you get the char at word[i].
            // If you don't, moves onto the next. Else, that's the first vowel in the word.
            for (int i = 0; i<word.length(); i++){
                if(vowels.indexOf(word.charAt(i)) >= 0){
                    index = i;
                    break;
                }
            }
            // In case a blank word is fed in.
            if(index == -1){
                return "";
            } else{
                // Checks for edge case like "Rhythm".
                return word.substring(0, index);
            }
        }

        @Override
        public String translate(String word) {

            if(word.isEmpty()){
                return "";
            }
            String ending;
            switch (word.charAt(0)){
                case ' ':
                    ending = "";
                    break;
                case 'b':
                    ending = "bark";
                    break;
                case 'g':
                    ending = "rrrowl";
                    break;
                case 'r':
                    ending = "rruf";
                    break;
                case 'w':
                    if(word.charAt(1)=='o'){
                        ending = "oofWoof";
                    } else {
                        ending = "oof";
                    }
                    break;
                default:
                    ending = "ay";
            }
            String firstChunk = getFirstNonVowelChunk(word);
            word = word.replaceFirst(firstChunk, "");
            return String.format("%s%s%s", word, firstChunk, ending);
        }
    };

    /**
     * Translates the word component of a {@link FrequencyWord}.
     *
     * @param frequencyWord
     * @return
     */
    public String translate(FrequencyWord frequencyWord) {
        return translate(frequencyWord.normalised);
    }

    /**
     * The base pattern for translating. DO NOT CHANGE THIS IN ANY WAY.
     *
     * @param word
     * @return
     */
    public abstract String translate(String word);
}
