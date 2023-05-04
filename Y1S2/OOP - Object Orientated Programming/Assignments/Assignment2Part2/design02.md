# First design #

- Leo Camacho
- S2222816
- 11
- Ray Jiang
- 2023-02-15

# Diagram #

![Diagram is split into explained steps in the style of a flowchart. Word inputs are given.
Arrays are drawn and changed throughout the steps](design02.jpg)

#### Accessibility Description ####

The diagram shows two inputs, "ESTATE" and "DUELED", represented as lists of 0's.
The lists are iterated over and each letter is checked for whether it has been seen before.
If it has, the number at its original occurrence is added to the list.
If not, a new number is added in the corresponding place.
The resulting lists are compared for equality and the value of that comparison is returned.

# High Level explanation what algorithm does #

The algorithm will take 2 (uppercase and non-empty) strings.
It will then initialise 2 lists of 0's of the length of each of the words.
It will take 1 word, and iterate over it. If the letter has not been seen
before, it will replace a corresponding 0 with a new unique number.
Else, it will replace the corresponding 0 with the number
that appeared the first time the letter was seen.
It will iterate this process over the entire word and
again for the second word. It will then compare if the 2 lists are the
same and return the boolean.

# Algorithm pseudo-code #

```
word1 = INPUT[0]
word2 = INPUT[1]


INITIALISE word1Array // Initialises to [0,0...,0]
INITIALISE word2Array

FOR both words 
i = 0
FOR every letter in word1
  IF (letter has NOT been seen before)
    Add i to the array at the letters position
  ELSE 
     prevI = Get the previous i used at that letter's position
     Add prevI to the array
  i = i+1

IF (Arrays have same Value)
    return true
ELSE
    return false

```

<STYLE>
* {
  font-size:   1rem;
  /*font-size:   1.2rem;*/
  /*font-size:   0.9rem;*/
  background-color: #2A252A;
  color:            #FFF;
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
