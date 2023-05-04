package a3algorithms;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.stream.IntStream;

public class ExampleRunner {

    public static final String PROGRAM_NAME =
            new Throwable().getStackTrace()[0].getClassName();

    public static final Verbosity VERBOSE = Verbosity.MAXIMUM;

    public static final int MIN_NUMBER_ARGS = 1;

    public static final String LOG_PREFIX =
            String.format("[%s]:\t", PROGRAM_NAME);

    public static final String PRELIMINARY_OK_MSG = "Preliminary checks passed";
    public static final String EXTRA_OK_MSG       = "Extra checks passed";

    public static final int JDK_MINIMUM = 11;

    /**
     * Only checks that can be done at compile-time.
     */
    private static final boolean[] PRELIMINARY_CHECKS =
            new boolean[]{
                    jdkAcceptable() // this is the only one in this program
            };

    /**
     * Collapse all the boolean results into a single result
     */
    private static final boolean PRELIMINARY_CHECKS_PASSED =
            IntStream.range(0, PRELIMINARY_CHECKS.length)
                     .allMatch(i -> PRELIMINARY_CHECKS[i]);

    /**
     * Make the command-line arguments available to the whole class
     */
    private static String[] args;

    /**
     *
     * @return name of the method that called this method.
     */
    private static String getCallingMethodName() {
        /**
         * grandparent because:
         * 0 = {@link Throwable} object
         * 1 = the current method because it generated a {@link Throwable}
         * 2 = the method that called the current method
         */
        final int grandparent = 2;
        return new Throwable()
                   .getStackTrace()[grandparent]
                   .getMethodName();
    }

    private static void log(boolean ok) {
        if ( VERBOSE.isVerbose() ) {
            System.out.printf("%s%s()\t%S%n",
                              LOG_PREFIX,
                              getCallingMethodName(),
                              ok);
        }
    }
    private static void log(String message) {
        if ( VERBOSE.isVerbose() ) {
            System.out.printf("%s%s()\t%s%n",
                              LOG_PREFIX,
                              getCallingMethodName(),
                              message);
        }
    }

    /**
     * Finds the major version of the JDK. Doesn't work for early versions
     * of Java.
     * @return The major version of the JDK being used
     */
    private static int getJavaVersion() {
        return Integer.parseInt(System.getProperty("java.version")
                                      .split("\\.")[0]); // 01/04/2023 updated to capture the first part
    }

    private static boolean jdkAcceptable() {
        log("Checking the JDK version");
        boolean ok = getJavaVersion() >= JDK_MINIMUM;
        log(ok);
        return ok;
    }

    /**
     * Ensures args is acceptable and logs the result.
     *   Acceptable is measured against the required minimum number of arguments.
     *
     * @param args
     * @return
     */
    private static boolean argsLengthOK(String[] args) {
        boolean ok = false;
        log(String.format("Checking %d is an acceptable number of command-line arguments",
                          args.length));
        if (args.length >= MIN_NUMBER_ARGS){
            ok = true;
        }
        log(ok);
        return ok;
    }

    /**
     * Perform basic checks on a file: whether it exists, is readable, and is not empty.
     *
     * @param filename the name of the file to check: the path can be relative
     *                 or absolute but should ideally be constructed in a
     *                 platform-independent format
     * @return whether or not the file exists, is readable, and is not empty
     */
    private static boolean fileOK(String filename) {
        log(String.format("checking file \"%s\"", filename));
        boolean ok;
        try (
                final FileInputStream fis = new FileInputStream(filename)
        ) {
            ok = fis.read() != -1; // check file is not empty by reading something
        } catch ( IOException e ) {ok = false;}

        log(ok);
        return ok;
    }

    /**
     * Iterate over files to ensure they're valid.
     *
     * @param filelist -> The list of all the files to check
     * @return if all files are valid.
     */
    private static boolean filesOK(String[] filelist){
        log(String.format("checking files '%s'", Arrays.asList(filelist)));

        for (String file : (filelist)){
            if (!fileOK(file)){
                log(String.format("File '%s' is not valid", file));
                return false;
            }
        }
        log(true);
        return true; // Since no file caused it to jump out, they all must be valid
    }

    private static boolean extraChecksPassed() {
        log("running extra checks");
        return argsLengthOK(args) && filesOK(args);
    }

    public static void main(String[] clArgs) {
        args = clArgs;

        if ( PRELIMINARY_CHECKS_PASSED ) {
            log(PRELIMINARY_OK_MSG);
            if ( extraChecksPassed() ) {
                log(EXTRA_OK_MSG);
                System.out.println("This is where you would start your program properly");
            }
        }
    }
}
