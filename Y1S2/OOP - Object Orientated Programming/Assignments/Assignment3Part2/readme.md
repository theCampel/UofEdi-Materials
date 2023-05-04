# Welcome to Assignment&nbsp;3 Part&nbsp;2 ##

## Notes ##

1. You can personalise the styling of this document for accessibility
   (and fun) by editing the `<STYLE>` section at the end. See
   Tutorial&nbsp;3 for further details. In IntelliJ there should be a
   pane next to this showing the rendered version of this `markdown`
   source. Instructions are in Tutorial&nbsp;3.
2. Although you may edit this file for readability and accessibility,
   you will be working in different files in this project: **do not
   put your assignment work in this file**. Put your work in the
   appropriate places otherwise your work might not be reviewed.
3. This file contains the instructions for Assignment&nbsp;3
   Part&nbsp;2.
4. Instructions for submitting Assignment&nbsp;3 Part&nbsp;2 are on
   the Assessment part of the website: this is so that there is a
   single, definitive set of instructions in case they need updating.
5. The more you have explored Assignment&nbsp;3 Part&nbsp;1, the less
   work you will likely have to to do Part&nbsp;2 as many of the ideas
   and some of the code can be transferred.
6. If you are a non-native speaker of English do not spend too long
   understanding the instructions by translating: you need to learn to
   read longer texts quickly &mdash; 45&nbsp;minutes should be more
   than enough

## Quick start ##

1. Read and follow the instructions in this document.
2. The tasks are a series of `TODO` comments, so make use of the
   `TODO` tab in IntelliJ. The complete list of original `TODO`s is
   provided in the file `updatedTODOs.txt`. I say "complete" but
   IntelliJ has recently started ignoring TODOs in some files unless
   you edit a comment which then triggers a refresh and it "finds" the
   `TODO`s that were already there. So I cannot guarantee that all the
   TODOs will show up in the `TODO` tab or `updatedTODOs.txt`.
3. You MUST remove the string `TODO: ` from each task that you
   **complete successfully**: this is so your marker can understand
   what you have (not) finished. Partial solutions for a TODO task are
   unlikely to score any marks: it's generally all or nothing per
   TODO.
4. DO NOT change existing visibility declarations (`public`,
   `protected`, `private`).
5. DO NOT change existing method or constructor signatures unless
   there is a specific comment instructing you to do so in a specific
   case.
6. You may do the work in whatever order you wish but are advised to:
   1. do `a3algorithms` first because the require code is algorithmic
      in nature and it is likely to be similar to some of the
      algorithms you developed for Part&nbsp;1 &mdash; the
      `a3algorithms` package will tested independently of the
      `literatureStats` package. You can self-test using the JUnit5
      tests provided in `tests/a3algorithms`. To enable the tests, go
      into the `Project Structure > Modules > Sources` and click the
      `tests` directory in the folder list and then select the `Tests`
      tag from the `Mark as:` line above. Once the tests are enabled,
      your code might not compile and run, so you might want to move
      some of the JUnit tests into another folder or just comment out
      the tests for code you haven't written yet.
   2. do `literatureStats` second because this largely involves
      copying the code from `a3algorithms` and pasting into the
      appropriate place in the classes in `literatureStats`. You are
      advised to install the `UML Generator` plugin and generate a
      diagram that shows you how the classes rely on each other. You
      are not required to understand the finer points of UML diagrams
      for this assignment: mainly you should look at the lines
      connecting classes. Classes in the UML diagram are draggable so
      you can rearrange for your own clarity. Do NOT include a UML
      diagram in your submission.
   3. do the writing third but give it sufficient time. You might find
      it useful to do some writing to help you understand the
      structures used in `literatureStats`.
7. You are recommended to generate the javadocs but DO NOT include
   them in your submission.
8. You can use files in the `input` directory to help you develop your
   file reading code. The `macbeth.txt` file is the Project Gutenberg
   version of that play. You are welcome to test the DataScientist
   class on other files Project Gutenberg: download the `UTF-8` text
   version of an ebook. You don't need to restrict your extra tests to
   English but if you use an ebook written in something other than the
   Roman alphabet, your word-splitting might need some (serious)
   adjustment.

## About this assignment ##

This assignment is in three parts:
1. algorithmic programming and unit testing
2. putting code into object-oriented structures
3. explaining your knowledge in writing

To preempt complaints that writing has no place on a programming
course: documenting code and explaining code to non-technical people
is a massive part of software engineering. In an academic setting you
need to be able to explain your code to allay suspicions of the code's
provenance. You also need to write in a way that shows your text was
not generated by AI.

The setting for this scenario is some basic Data Science or Natural
Language Processing, subjects that are coming up in subsequent
years. This assignment does not do anything sophisticated with respect
to those subjects. What it does do though is introduce a significant
change in the end goal of your programming. This year you have been
learning programming for the sake of learning to program. In
subsequent years you will be using programming languages as the medium
through which you achieve the goals of another subject. Think of it
like learning a foreign language: at school you learn the language and
concentrate on matters such as the syntax. When on holiday you use
that language to convey and understand information.

In this assignment you are essentially reading a text file, splitting
it into words, normalising the word forms (original word forms are
completely discarded to simplify the assignment), and counting how
often each normalised word form appears. The end goal is to be able to
produce some "interesting" statistics about a document, mainly the
most (or least) frequently occurring normalised word forms. It's not
much in terms of Data Science or NLP, but it's a useful start.

Another task of this assignment is to translate normalised words into
another language. This is because a friend of yours is putting on a
play &mdash; Macbeth, also known as The Scottish Play &mdash; using
animals (don't ask) and the animals need to read the script in their
own language (obviously). The current cast is exclusively trolls and
Scottie dogs (again, don't ask; also don't ask what happened to the
cast of cats).

## Primary aims ##

The primary aims of this assignment are for you to collate the skills
you have developed during this course and apply them consciously to a
larger block of code (my solution totals 1000 lines of code). The
lectures and reading have contributed the core knowledge, the
tutorials have put key concepts into practice, the labs have given you
plenty of opportunity to practice. Assignment&nbsp;1 tested your
knowledge of basic Java concepts, fragments of code, and short
programs: any large software system is made of such small components.
Assignment&nbsp;2 showed you the value of designing before programming
and the real value of going beyond your first draft. It also gave you
practice at explaining code functionality. Assignment&nbsp;3
Part&nbsp;1 gave you the chance to learn through practice for the
tasks in this Part. Assignments 2 and 3 and the labs and tutorials
have given you plenty of opportunity to learn to make good use of
IntelliJ's tools to help you improve your coding.

Another primary aim is for you to use this assignment to have an
honest conversation with yourself about whether you are really ready
for your second year. If you are not, there are lots of options: talk
to whoever is in your student support circle.

## Assignment rules ##

1. **Follow the instructions:** not following the instructions is the
   number one criticism teachers tend to make of students &mdash;
   markers are permitted to penalise and even largely ignore
   submissions that have not followed the instructions.
2. **Work within the spirit of the rules** not just the exact wording.
3. The deadline is on the Course Schedule page on the Inf1B Learn page
   &mdash; it is not repeated here because we want to have a single,
   definitive source for the deadline in case it needs to be changed.
4. You may use tools to help you draft any writing however the version
   you submit must be mostly your own effort and it must contain
   specific and exact examples for the challenge and your design(s),
   therefore the generic, non-specific output of AI-generated text is
   unsuitable for the final version.
5. Do as the engineering requirements ask.

## Engineering requirements for this part of the assignment ##

These requirements outline what is required, what is encouraged or
discouraged, and what is banned. These are neither exhaustive nor a
challenge to find loopholes. They are here to guide you.

Your design **MUST**:
1. have the language level of the project set to 11
2. leave the code in the packages and locations where provided
3. be predominantly and demonstrably your own work and effort
4. acknowledge and reference other sources and resources if they
   inspired or influenced your design
5. keep the `a3algorithms` and `literatureStats` code separate: some
   classes deliberately appear in both by design, even if they are
   essentially identical
6. be able to run the `a3algorithms` classes completely independently
   of the classes in `literatureStats`
7. be able to run the `literatureStats` classes completely
   independently of the classes in `a3algorithms`
8. run on the automarker provided: we don't care whether it runs on
   your computer: it must run on ours

Your design **MUST NOT**:
1. rely on third party libraries or software: JDK 11 libraries are
   all fine
2. be plagiarised in part or as a whole
3. use any programming language other than Java
4. change the package declarations
5. change the signatures inclusive-or visibility of identifiers
   provided to you, unless there is a specific instruction that you
   may change a particular one

You **MUST**:
1. be able to explain your entire design if challenged, including the
   generics and any regular expressions you have used
2. submit your work according to the submission instructions on the
   Learn Assignments page

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
