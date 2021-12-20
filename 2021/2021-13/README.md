# Advent of Code 2021 day 13

`2021-13.rkt` has my original solution.

`2021-13-vector.rkt` has a version that uses vectors instead of complex numbers to represent the points.

`test.txt` is the example data from the puzzle.

`input.txt` was my input.

`harder.txt` is another input found on Reddit.

`font8x8_basic.h` is a font used by `encode.rkt`. (See below.)

## Decode and encode

`decode.rkt` and `encode.rkt` are command line utilities. To use them, you'll need Racket installed and the `racket` executable in your `PATH`. (Or give the full path on the command line if you wish, of course.) You may also need to install some Racket modules. I'm assuming Racket will prompt you if necessary.

To decode a file:

    $ racket decode.rkt < input.txt

To encode the text "This is a test!" with 10 folds:

    $ racket encode.rkt "This is a test!" 10

Note that `font8x8_basic.h` needs to be in the current working directory when running `encode.rkt`.

