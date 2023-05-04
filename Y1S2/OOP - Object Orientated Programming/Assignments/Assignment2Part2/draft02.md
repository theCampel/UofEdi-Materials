# Explaining the challenge to a beginner #

- Leo Camacho
- S2222816
- 11
- Ray Jiang
- 2023-02-16

# Description #

Want a fun word challenge?
What do the words `THERMODYNAMICS` and `RAMBUNCTIOUSLY` have in common? They're _isomorphs_.
If you're after shaking your head thinking "What in the world is an Isomorph?", don't worry.
That was me before beginning the challenge.

Specifically, Isomorphic words are words that have the
same repetition scheme of letters in both words. Our goal
is: given 2 uppercase words, we must determine if they're isomorphic or not.

To understand the concept of character repetition, take the "word" `XYX`.
It has the character repetition scheme of **"1 2 1"** because:

- That is the first letter is a unique letter
- The second letter is a unique letter
- The third letter is the same as the first letter.
  This is more often represented as **"aba"**.

Now take `ESTATE`. It has the character repetition scheme of **"abcdca"**.
The word `DUELED` has the character repetition scheme of **"abcdca"** as well.
Since they both have the same scheme, they're isomorphic.

#### Technical Specifications: ####

Assume:

- You'll always be given 2 uppercase strings
- They will be non-empty

We must:

- Determine if they're isomorphic
- Output a boolean if they are or are not

#### Proposed Solutions Explanation ####

My proposed solution will:

1. First eliminate (return false) words that cannot be isomorphic. (E.g. Words of unequal length)
2. Initialise Integer Array representations of the words
3. Iterate over the first word and check if each letter has been seen before
    - If it's a new letter, add a new number to that position in the Int Array
    - If it's not a new letter, add the number that appeared when the letter was first seen
4. Repeat for the second word
5. Return true if the Integer Arrays are equal

# Original challenge question from codegolf #

[Short link to codegolf challenge](https://www.codegolf.stackexchange.com/questions/50472/check-if-words-are-isomorphs)

Two words are isomorphs if they have the same pattern of letter repetitions. For example, both ESTATE and DUELED have
pattern abcdca

`ESTATE`

`DUELED`

`abcdca`

because letters 1 and 6 are the same, letters 3 and 5 are the same, and nothing further. This also means the words are
related by a substitution cipher, here with the matching E <-> D, S <-> U, T <-> E, A <-> L.

Write code that takes two words and checks whether they are isomorphs. Fewest bytes wins.

**Input**: Two non-empty strings of capital letters A...Z. If you wish, you can take these as a collection of two
strings or
as a single string with a separator.

**Output**: A consistent Truthy value for pairs that are isomorphs, and a consistent Falsey value if they are not.
Strings
of different lengths are valid inputs that are never isomorphs.

### Examples / Test cases: ###

| Word 1          | Word 2          | Output |
|-----------------|-----------------|--------|
| AAA             | BBB             | True   |
| XYX             | ABA             | True   |
| ESTATE          | DUELED          | True   |
| DUELED          | ESTATE          | True   |
| RAMBUNCTIOUSLY  | THERMODYNAMICS  | True   |
| A               | B               | True   |
| _ (Blank space) | _ (Blank space) | True   |
| SEE             | SAW             | False  |
| ANTS            | PANTS           | False  |
| BANANA          | SERENE          | False  |
| AB              | CC              | False  |
| XXY             | XYY             | False  |
| ABAB            | CD              | False  |

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
  line-height: 210%;
  /*line-height: 180%;*/
  }
  pre,
  code,
  pre code {
  font-family: "OpenDyslexicMono", monospace;
  line-height: 200%;
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
  margin-top:  40px;
  padding-top: 10px;
  font-size: 1rem;
  }
</STYLE>
