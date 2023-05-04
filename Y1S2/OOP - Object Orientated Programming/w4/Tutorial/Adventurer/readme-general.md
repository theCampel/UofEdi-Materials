# General Instructions #

**Note:** These instructions are generic to this entire tutorial,
irrespective of pathway.

**Specific** instructions for this IntelliJ project are available in `readme.md`

## Project structure ##

It is vital the project is structured properly and has been opened correctly
in `IntelliJ`. Generically, projects are conventionally structured like this:

```
NameOfProject    <--- project root directory
├───.idea        <--- IntelliJ files go somewhere under here
├───out          <--- .class files go somewhere under here
└───src          <--- .java files must go here
```

However this project is configured like this:

```
NameOfProject    <--- project root directory
├───.idea        <--- IntelliJ files go somewhere under here
├───modelInput   <--- Sample input files for self-testing 
├───modelOutput  <--- Model output files corresponding to the sample input
├───out          <--- .class files go somewhere under here
└───src          <--- .java files must go here
```

### Check you are off to the right start ###

1. If not already open, activate the `Project` view tab (typically in the
   top-left) by clicking on the tab or for better productivity use the
   short-cut key `Alt+1`
2. Check your project structure is compliant with the diagram; if not you
   may need to re-unpack the `zip` file or open the project in a different way
3. While you are here, it is useful to activate the `Structure` tab
   (typically in the bottom-left) with the short-cut key `Alt+7`. This
   facilitates finding and editing the relevant parts of your code.

## Setting and changing command-line arguments ##

To set the command-line arguments (also known as command-line parameters)
you need to edit your program's run configuration. Although you can create a
run configuration from scratch it is easier to let IntelliJ do it for you:

1. Run your program: if it complains you haven't provided enough
   command-line arguments, complain to the programmer `>:-]`
2. `Run > Edit Configurations...`
3. Choose the name of your program from the left-hand list of Applications
   (the likelihood is IntelliJ will already be selected it)
4. Add your command-line arguments to the line that reads `Program arguments`
5. `OK` the dialogue box and re-run your program
6. Use this same method every time you wish to change the command-line arguments

## Setting and changing input ##

Programs can read input from the keyboard. But this is tedious, slow, and
unreliable for testing because you have to try to type exactly the same things
in exactly the same order every time. A massive improvement on these counts
comes from using the contents
of a pre-written file instead. This is known as "redirecting the input" or
"input redirection." There is a related mechanism for redirecting your
program's output to a file, see `Saving program output to a file`.

IntelliJ can take the contents of a text file and make your program think it
is reading the keyboard:

1. If you haven't run your program once already, do so now: you might need
   to type some input to keep it happy; if your program keeps demanding more
   input first press `Ctrl+D` to tell the program there is no more input,
   then complain to the programmer `>:-]`
2. `Run > Edit Configurations...`
3. Choose the name of your program from the left-hand list of Applications
   (the likelihood is IntelliJ will already be selected it)
4. `Modify Options` (`Alt+M`)
5. `Redirect input`
6. A `Redirect input from` line will appear in the dialogue
7. Click the folder icon at the end of that line
8. Navigate to the appropriate file that you want to use an input

### Time saver ###

If you want to change which file is taken as input, you need to repeat the
process. Alternatively, there is a file called `yourInput.txt` which you can
leave as the input file. Open that file in IntelliJ and paste in the
contents from another file. Edit to suit your needs. This is a great
time-saver.

You also need reliability in testing, so for that use the provided files or
create your own, for example by merging the contents of multiple other test
files. This is important for reliability because the contents of those files
should not have changed. The provided sample input files are read-only to
help guarantee this.

## Provided sample input files ##

**You can and probably should make your own test files.** Sample input files for
self-testing your work are in the `modelInput` directory. These files have 
been set as read-only to help reduce the risk of accidentally introducing 
errors into the testing. The provided files are named according to the 
following convention: `direction-level-shortDescription.txt`

Notes:

- the three components are separated by a single hyphen;
- the `shortDescription` uses
  [`camelCase`](https://en.wikipedia.org/wiki/CamelCase);
- this naming convention allows the filename to be split into its three
  components by using `-` as the separator providing the `shortDescription`
  avoid using hyphens, hence camelCase.

| **Direction** | **Meaning**                    |
|:--------------|:-------------------------------|
| input         | Send to your program           |
| output        | Generated by the model program |

| **Level** | **Meaning**                                           |
|:----------|:------------------------------------------------------|
| core      | A complete program must handle all these              |
| defensive | Faulty input to test your program's added resilience  |
| extra     | For if you want to go beyond the basic specifications |

There is also a file called `descript.ion` which matches filenames to brief
descriptions. **The list in `descript.ion` may be incomplete.**
The name of that file is historical. If you're interested, investigate `4DOS`.

## Saving program output to a file ##

Sample output files for self-testing your work are in the `modelOutput`
directory. See the section on sample input for an explanation of file names.

Your program should be configured to save its output to
`yourOutput.txt` in the root directory of the project:

1. If you haven't run your program once already, do so now: you might need
   to type some input to keep it happy; if your program keeps demanding more
   input then first press `Ctrl+D` to tell the program there is no more
   input, then complain to the programmer `>:-]`
2. `Run > Edit Configurations...`
3. Choose the name of your program from the left-hand list of Applications
   (the likelihood is IntelliJ will already be selected it)
4. `Modify Options` (`Alt+M`)
5. `Save console output to file` (a `Logs` section appears near the bottom
   of the dialogue box)
6. Click the folder icon at the end of the line in the `Logs` section that says
   `Save console output to file`
7. Navigate to the `yourOutput.txt` file
8. `OK` the dialogue box

If you have `yourOutput.txt` open in IntelliJ, it will automatically update 
when the contents have changed, though the Java program will have to terminate 
first.

See the next section on `Comparing output`.

## Comparing output with the model answer ##

It can be hard to spot differences between the required model output and your 
program's actual output. To help you there is a world of tools related to 
"diff" (for "difference"). IntelliJ has a visual diff tool built-in.

First, ensure your program is saving its output to a file, see `Saving
program output to a file`.

In the `Project` view, right-click `yourOutput.txt` and choose
`Compare With...` (`Ctrl+D`) and select the appropriate equivalent file from
the `modelOutput` directory. This opens a comparison window that shows you the
differences and is configurable for sensitivity to some types of difference such
as spaces and blank lines.

<STYLE>
* {
  font-size:   1.1rem;
  /*font-size:   1.2rem;*/
  background-color: #2A252A;
  color:            #D5DAD5;
  /*background-color: DarkSlateGray;*/
  /*color:            AntiqueWhite;*/
  /*background-color: black;*/
  /*color: white;*/
  /*background-color: white;*/
  /*color: black;*/
}
body {
  width: 80%;
  font-family: "OpenDyslexic", serif;
  /*font-family: sans-serif;*/
  line-height: 180%;
  /*line-height: 200%;*/
}
pre,
code,
pre code {
  font-family: "OpenDyslexicMono", monospace;
  line-height: 150%;
}
ol,
ol ol,
ol ol ol {
  list-style-type: decimal;
}  
em {
  font-style: normal;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  padding-bottom:      2px;
  /*text-decoration: underline;*/
  text-decoration-skip-ink: auto;
}
h2 {
  border-top:  1px solid #D5DAD5;
  margin-top:  80px;
  padding-top: 20px;
}
</STYLE>