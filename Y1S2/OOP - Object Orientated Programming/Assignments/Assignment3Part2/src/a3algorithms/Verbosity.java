package a3algorithms;

public enum Verbosity {
    SILENT(0),
    MINIMUM(1),
    MEDIUM(5),
    MAXIMUM(9);

    private final int verbosityLevel;

    Verbosity(int verbosityLevel) {

        this.verbosityLevel = verbosityLevel;
    }

    public int getVerbosityLevel() {
        return verbosityLevel;
    }

    public boolean isVerbose() {
        return verbosityLevel > 0;
    }
}
