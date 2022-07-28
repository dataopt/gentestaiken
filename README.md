# Gentestaiken

A simple command-line Haskell program that generates a random test in Markdown
format from a question bank in
[Aiken format](https://docs.moodle.org/400/en/Aiken_format).
It also serves as an example of using 
`optparse-applicative` for parsing command-line options and.
`attoparsec` for general parsing.

## Description

This is a program that takes a text file consisting a question bank in Aiken
format and outputs to `stdout` a test in Markdown format consisting of a
specified number of questions randomly drawn from the question bank. Correct
answers for the selected questions appear after the questions.

## Getting Started

### Dependencies

The program can be built with cabal using GHC version 8.10.7 with
the following package dependencies:

 - attoparsec
 - combinat
 - optparse-applicative
 - question
 - random
 - text

### Installing

#### macOS/Linux

1. Clone the repository:

   `git clone https://github.com/dataopt/gentestaiken`

2. Build the executable:

   `cd gentestaiken; cabal build`

3. (Optional) Install into path for cabal-built binaries:

   `cabal install`

### Usage

```
Usage: gentestaiken FILENAME [-r|--random COUNT] [-t|--title TITLE]

  Generate test from Aiken question bank.

Available options:
  FILENAME                 File name of question bank in Aiken format
  -r,--random COUNT        Number of random-selected questions. Otherwise, all
                           questions used unshuffled.
  -t,--title TITLE         Test title. (default: "Test")
  -h,--help                Show this help text
```

### Example

The following creates a test of 3 questions randomly selected
from the sample question bank `test.aiken` under `tests`:

```
gentestaiken test.aiken -r 3 -t "My test"
```

The following is one possible output:

```
% My test

# Questions

1.  What is the capital of France?

    A) Rome
    B) Paris
    C) London
    D) Beijing

1.  What is the value of 4+3?

    A) 1
    B) 3
    C) 5
    D) 7

1.  Which of the following countries is in Asia?

    A) Germany
    B) Rwanda
    C) Japan
    D) Brazil

\newpage

# Answer key to My test

1. B
1. D
1. C
```

Note that the numbering does not increase since (most)
Markdown viewers will perform proper labelling.

## Version History

* 0.1.0.0
    * Initial Release

## License

This project is licensed under the BSD-3-Clause License - see the LICENSE.txt file for details
