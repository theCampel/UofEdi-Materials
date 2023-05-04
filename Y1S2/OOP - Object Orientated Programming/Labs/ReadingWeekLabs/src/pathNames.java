import java.util.Arrays;

public class pathNames {

    public static String[] splitByPath(String fileName){
        String[] subs = new String[4];
        int fileBeginning = fileName.lastIndexOf("/") + 1;
        int suffixBeginning = fileName.lastIndexOf(".");

        subs[0] = fileName.substring(0, fileBeginning);
        subs[1] = fileName.substring(fileBeginning);
        subs[2] = fileName.substring(fileBeginning, suffixBeginning);
        subs[3] = fileName.substring(suffixBeginning);
        return subs;
    }

    public static void main(String[] args) {
        System.out.println(Arrays.toString(splitByPath("/home/bill/work/weedfertilisers.pdf")));
    }
}
