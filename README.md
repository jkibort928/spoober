# spoober
A text file parser, designed for keeping organized lists of packages for easy reinstall.

Note: this readme is currrently for my own reference. This will probably be turned into a manpage if I figure out how to do that lol.

# .spoob file parser specification:

    Input: A file containing a newline separated list of strings
    Output: A space separated list of strings (to be used in pacman/paru/etc.)

    Lines are parsed as text.
    Whitespace should be ignored. (trim whitespace before parsing)

    '#'     is a comment, the parser should ignore everything after it on the same line
    '!#'    is an optional. Optionals are like comments, but can be "uncommented" by specifying a certain flag in the parser
    '?#'    is an optional.

    '/*'    is a multiline comment start
    '*/'    is a multiline comment end

    '<'     is the start of a heading
    '>'     is the end of a heading

    Headings allow you to specify certain "modules"

    To list the headings in a file, use the -l flag.

    To only output strings in desired headings, specify the -h flag,
    then the parser will take all headings in as command-line arguments
    (note: the first argument will always be the infile)

    ex: spoob infile.spoob -h module1 module2 module3
    ex: spoob -h infile.spoob module1 module2 module3

    Passing the -e flag will do the same, but EXCLUDE modules.
    This will supercede the -h flag.

    Headings can be nested in the input file, for example:

        <h1>
            item1
            <h2>
                item2

    In the above example, item1 is a part of module h1, but not h2.
    item2 is a part of module h1 and h2.
    Invoking the parser with module h1 specified, you will get item1 and item2 in the output.
    Invoking the parser with module h2 specified, you will get item2.
    (Note: indentation is irrelevant to the parser)

    Headers can be closed by simply putting the same header again. Ex:

        <h1>
            item1
        <h1>
        <h2>
            item2
        <h2>

    The above example excludes item2 from module h1.

    Optionals Legend:
        '!#' = unneeded      (don't need this)
        '?#' = optional      (probably don't need this)
        '*#' = prospective   (useful but unneeded atm, install when needed)

    Optionals can be specified by passing --<optional> to the parser, ex. --unneeded
