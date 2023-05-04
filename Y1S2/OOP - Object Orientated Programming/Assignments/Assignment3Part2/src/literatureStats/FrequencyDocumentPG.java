package literatureStats;
/**
 * A FrequencyDocumentPG stores all the words and their words of a
 * Project Gutenberg ebook. This means it only stores the words of the actual
 * text and not the preamble and postamble added by Project Gutenberg.
 * <p>
 * Every instance must have a configuration file {@link FrequencyReaderConfig}
 */
public class FrequencyDocumentPG extends FrequencyDocument {
    public static final String PG_DOCUMENT_START =
            "*** START OF THE PROJECT GUTENBERG EBOOK";
    public static final String PG_DOCUMENT_STOP  =
            "*** END OF THE PROJECT GUTENBERG EBOOK";


    public FrequencyDocumentPG() {
        super();
    }

    public FrequencyDocumentPG(String filename) {
        initialise(filename);
    }

    public FrequencyDocumentPG(String filename, String nonWordChars) {
        initialise(filename, nonWordChars);
    }

    public FrequencyDocumentPG(FrequencyReaderConfig config) {
        initialise(config);
    }


    public FrequencyDocumentPG(FrequencyReaderConfig config,
                               String nonWordChars) {
        initialise(config, nonWordChars);
    }


    @Override
    public void initialise(String filename) {
        initialise(filename, FrequencyDocumentReader.DEFAULT_NON_WORD_CHARS);
    }



    @Override
    public void initialise(String filename, String nonWordChars) {
        FrequencyReaderConfig config =
                new FrequencyReaderConfig(filename,
                        PG_DOCUMENT_START,
                        PG_DOCUMENT_STOP,
                        FrequencyReaderConfig.DEFAULT_VERBOSITY);

        initialise(config, nonWordChars);
    }
}