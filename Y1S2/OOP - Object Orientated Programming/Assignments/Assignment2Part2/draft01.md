# Explaining the challenge to a beginner #

- Leo Camacho
- S2222816
- 11
- Ray Jiang
- 2023-02-16

# Description #

The CodeGolf challenge is all to do with _isomorphs_. Isomorphic words
are words that have the same repetition scheme of letters in that word.

For example the word `ESTATE` has the character repetition scheme of
*"abcdca"*.
The word `DUELED` has the character repetition scheme of *"abcdca"* as well.
Thus, they're isomorphic.

The goal of this coding challenge is to determine if two given words are
isomorphic.

We can assume the inputs are always two uppercase strings. Our output
must be a True/False boolean on if it's an isomorphic duo.

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

Input: Two non-empty strings of capital letters A..Z. If you wish, you can take these as a collection of two strings or
as a single string with a separator.

Output: A consistent Truthy value for pairs that are isomorphs, and a consistent Falsey value if they are not. Strings
of different lengths are valid inputs that are never isomorphs.

Test cases:

##### True: #####

`ESTATE` `DUELED`

`DUELED` `ESTATE`

`XXX` `YYY`

`CBAABC` `DEFFED`

`RAMBUNCTIOUSLY` `THERMODYNAMICS`

`DISCRIMINATIVE` `SIMPLIFICATION`

##### False: #####

`SEE` `SAW`

`ANTS` `PANTS`

`BANANA` `SERENE`

`BANANA` `SENSES`

`AB` `CC`

`XXY` `XYY`

`ABCBACCBA` `ABCBACCAB`

`ABAB` `CD`


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
  margin-top:  40px;
  padding-top: 10px;
  font-size: 1rem;
  }
</STYLE>
