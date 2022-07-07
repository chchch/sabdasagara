# sabdasagara

Splits a set of Sanskrit texts (in FASTA/FASTT format) into ngrams (akṣaras), with normalization and optional fuzzy matching.

To build:

    stack build

To run:

    stack exec -- sabdasagara-exe -n 4 -o csv -f example.fas > output.csv

This will split the texts in example.fas into 4grams, with fuzzy matching. For more options, run

    stack exec -- sabdasagara-exe

