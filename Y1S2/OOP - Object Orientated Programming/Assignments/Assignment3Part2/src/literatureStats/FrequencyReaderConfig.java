package literatureStats;

public class FrequencyReaderConfig {
    public final String START_MARKER;

    public final String STOP_MARKER;

    public final String DOCUMENT_FILENAME;

    /**
     *  null is the default for empty markers: DO NOT CHANGE THIS.
     */

    public final static String EMPTY_MARKER = null;

    /**
     * Silent is the default verbosity: DO NOT CHANGE THIS.
     */
    public final static Verbosity DEFAULT_VERBOSITY = Verbosity.SILENT;

    private Verbosity verbosity;

    public FrequencyReaderConfig(String dictionaryFilename,
                                 String startMarker,
                                 String stopMarker,
                                 Verbosity verbosity) {

        // 01/04/2023 updated ordering to match the parameter sequence
        this.DOCUMENT_FILENAME = dictionaryFilename;
        this.START_MARKER      = startMarker;
        this.STOP_MARKER       = stopMarker;
        this.verbosity         = verbosity;
    }

    /**
     * Gets the verbosity for the current configuration.
     *
      * @return
     */
    public Verbosity getVerbosity() {
        return verbosity;
    }

    /**
     * Sets the verbosity for the current configuration.
     * @param verbosity
     */
    public void setVerbosity(Verbosity verbosity) {
        this.verbosity = verbosity;
    }
}
