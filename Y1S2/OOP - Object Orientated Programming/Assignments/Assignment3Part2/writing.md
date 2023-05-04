# Writing for Assignment&nbsp;3 Part&nbsp;2 2023 #

- **Leo Camacho**
- **S2222816**
- **11**
- **Ray Jiang**
- **2023-04-07**


## Acknowledgements ##

| Person / Tech                                                                                                                     | How they / it helped                                                                                                                                     |
|-----------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------|
| Piazza                                                                                                                            | Post @302, @295, @288. All of these helped clarify what was allowed from and expected of my work. They also helped clarify how to operate FrequencyWord. |
| Brian Mitchell (Instructor)                                                                                                       | He answered both public and private questions related to the the form and possible errors of my DataTypes.                                               |
| [StackOverflow](https://stackoverflow.com/questions/2889777/difference-between-hashmap-linkedhashmap-and-treemap/2889800#2889800) | Helped determine which data type to use as the basis for FrequencyWord                                                                                   |
| [StackOverflow](https://stackoverflow.com/questions/25899929/in-java-sort-hash-map-by-its-key-length)                             | Helped me find out how to sort the FrequencyWord                                                                                                         |
| [StackOverflow](https://stackoverflow.com/questions/23079003/how-to-convert-a-java-8-stream-to-an-array)                          | Helped me appropriately work with .stream() in Java                                                                                                      |
| [Java Documentation](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html)                          | Helped understand the purpose of Try-With-Resources and thus come up with a                                                                              |
| Lectures weeks 4,5,10                                                                                                             | Helped me implement those concepts. Also managed version control using Git.                                                                              |
| Code Quality & Conventions                                                                                                        | Helped understand what was appropriate to implement where.                                                                                               |

## Code location ##

The majority of the code within the methods moved from **a3algorithms** to **literatureStats** was easily transferred. 
For example, the two from `ExampleRunner` to `Runner`, namely `filesOk()` and `ArgsLengthOk()`, was easily transferred. 
The method names, class names and purposes were all identical, making their designated location intuitive. 
The same was true for the entire `SortingOrder` and `Verbosity` Enum. 
Additionally, all of these files were smaller in size and complexity, making their migration simple.

Unfortunately, from that point on the code required more inspection of its purpose and slight reconfiguration. 
As `SimpleFrequencyWord` gained complexity and became just `FrequencyWord`, it engulfed code from across **a3algorithms**. 
`Normaliser`’s single method crossed into this different class. 
Knowing its destination was simple as the method names were the same.

The complexity of the code migration ramped up significantly for transferring `AdvancedFileReader`’s processing code to 
`FrequencyDocumentReader`. As it was not a simple copy-and-paste job, it meant that the usage of the code had to be adapted. 
It was obvious the bulk of the reading code would move, but analysis of the config and a 
custom data type showed that it could all DRY-ly fit in **FrequencyDocumentReader**’s `readDocument`.



## DRY programming ##

This assignment, like all coding projects, contained many “_traps_”, where budding programmers 
would be tempted to simply copy and paste code blocks, leading to obviously WET programming.

The first area that comes to mind is in `FrequencyDocumentReader`. 
First, all the `readDocument` methods are basically identical with only minor changes due to a lack of 
information passed through to it. It is highly effective, efficient and elegant to call the method 
requiring the most information from that less equipped methods. For the information you lack, 
it is sufficient to supply the default values. By contrast, a WET approach would involve copying
and pasting the `readDocument` code into every single `readDocument` method, changing only the required 
fields to their defaults. With `readDocument` being a heavy method to begin with, 
the WET approach would multiply that by 5 other methods.

Less obvious “_traps_” for WET programming appeared in the project as well. 
In the `Translation` enum, my initial instinct was to use if and if-else statements for the entire translate method. 
Each if block would have had the full non-vowel-chunk shift and reordering. 
Upon review of the concept on paper, I deemed it too WET, opting for a much more DRY and efficient switch-case method.

## Relationships ##


The `DataScientist` class serves as the central orchestrator that connects and calls the various classes 
`InformationDocument`, which in turn calls and uses `FrequencyDocument` and `FrequencyDocumentPG`. 
This helps make the file modular, making it more maintainable and understandable. 
`InformationDocument` uses Java’s generics, which allow it to take and safely operate on several different 
types without duplicating code. This adheres to DRY principals, but also to the Robustness section as laid out 
in the Code Quality and Conventions. Multiple different types will work and be handled safely.

It also incorporates helper methods, like `experimentXPhaseY()` before finally calling `runExperimentX()`. 
Again, the use of modularising each experiment phase before finally calling it as a one-liner somewhere 
else greatly increases its readability and flexibility.

Additionally, the `Translation` enum is called from within `DataScientist`. Using an `Enum`, as opposed to basic 
classes, was the most appropriate choice, adhering to the “Use appropriate language features” section of the 
Code Quality and Conventions. `Enum`’s are a special type of implicitly final class which represent fixed 
constants and no new instances of these classes can be created.


## Explain reading a file ##

The first call for reading a file is upon initialising `FrequencyDocument.words`. 
Provided with a `config` and `nonWordChars`, the main `readDocument` method checks if a `startMarker` exists and 
begins reading if not (as the file should all be read).

The next section involves Java’s `try-with-resources` block for safely reading a file. 
This was a feature that simplifies and makes the reading process safer, and was only introduced in Java `7`. 
Without that version the file will not safely run, hence the recommendation of the minimum Java version.

The reading continues in the `readDocument` method, until _preprocessing_ the line is required. 
The first step is outsourced to `FrequencyWord.normalise()` to normalise it. 
This is done first so each sentence has no leading/trailing whitespace, otherwise increasing the chances of 
empty strings creeping in. Then `nonWordChars` are removed before converting it to a list of words 
(both using helper methods). This order was chosen as by removing illegal characters first, 
the list could be easily created and added to. Also, this reduced the chances of 
blank spaces making their way into the final `HashMap`.

Additionally, I created a separate helper method `log`. This helper method took care of outputting 
relevant information when config verbosity dictated it appropriate.


## Explain your translate-to-dog implementation ##

My algorithm first has a helper method to find the first `nonVowelChunk`. 
That is the group of consonants (if any) that appear before the first vowel. 
This method iterates through the word until the index of the first vowel is found, returning the initial substring.

To best implement the `TODO`’s, the algorithm of shifting had to be understood. 
Essentially, it was removing the first chunk, appending it to the end and adding a custom ending depending on the 
starting letter(s). The ending it needed adding was not necessarily the same as the one

I structured the method to first account for edge cases, analyse the special cases and then deal with 
the common case. The main `Switch` logic looks to find the ending based on the beginning letter. 
If it’s no special letter, it defaults to the default ending; “`ay`”.

The final string itself is created from a concatenation of all 3 parts, the original word 
(minus the removed first consonants, the first chunk and then the ending). This is done once after the `switch` 
case, to be `DRY` and avoid copying and pasting the creation of the string in each case. 


