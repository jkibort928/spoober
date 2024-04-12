# spoober
A text file parser, designed for keeping organized lists of packages for easy one-command installation, written in Haskell.

I made this project to assist with reinstalling my OS

Note: this readme sucks lol

# INSTALLATION
- Install ghc using ghcup if you haven't already
- Run 'make' in the repo directory
- You now have a binary for spoober located at src/bin/Main
- Move src/bin/Main anywhere you like, and rename it whatever you want (only if the name is "spoober")
(very professional and cool installation instructions)

# USAGE
	spoober [OPTIONS] <FILE> [MODULES]

	OPTIONS: 
		-h: 		Display this help message
		-l: 		list all modules within the file
		-m: 		Only select packages within specified modules
		-e: 		Exclude packages within specified modules
	 
	 	--all: 		Uncomment all conditional comments 	(*#, ?#, !#)
   		--prospective: 	Uncomment prospective packages 		(*#)
		--optional: 	Uncomment optional packages 		(?#)
		--unneeded: 	Uncomment unneeded packages 		(!#)
	FILE:
		The infile to read
	MODULES:
		The modules you wish to specify
		(will do nothing unless -m or -e is active)

  	Examples:
		spoober -l infile.spoob
		spoober -m infile.spoob module1 module2
		spoober -e infile.spoob module3
		spoober infile.spoob --prospective --optional

This program will take an input file, and spit out a space-separated list of packages.
(Example input files can be found in the examples directory)

Use this program as an input to a package manager install command through a subshell.

Note: FOR SAFETY, REDIRECT STDERR TO /dev/null TO AVOID UNINTENDED CONSEQUENCES

Example: 
	`# pacman -S $(spoober infile.spoob 2>/dev/null)`

(I really should think of a better way around this)

# .spoob file format specification:

	Input: A file containing a newline separated list of strings
	Output: A space separated list of strings (to be used in pacman/paru/etc.)
	
	Whitespace will be ignored.
	If a line contains multiple words, only the first one will be recognized
	
	'#'  is a comment, the parser should ignore everything after it on the same line
	'!#' is an optional. Optionals are like comments, but can be "uncommented" by specifying a certain flag in the parser
	'?#' is an optional.
	'*#' is an optional.
	
	'/*' is a multiline comment start
	'*/' is the end of a multiline comment (can't type it or it would uncomment this)
	
	'<' is the start of a heading
	'>' is the end of a heading
	
	Headings allow you to specify certain "modules"
	
	To list the headings in a file, use the -l flag.
	
	To only output strings in desired modules, specify the -m flag,
	the parser will read command line arguments for desired modules.
	(note: the first argument should always be the infile)
	
	ex: spoober infile.spoob -m module1 module2 module3
	ex: spoober -m infile.spoob module1 module2 module3
	
	Passing the -e flag will do the same, but EXCLUDE specified modules.
	This will supercede the -m flag.
	
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
	'*#' = prospective   (useful but unneeded atm, install when needed)
	'?#' = optional      (probably don't need this)
	'!#' = unneeded      (don't need this)
	
	Optionals can be specified by passing --<optional> to the parser, ex. --unneeded
	Passing --all to the parser will specify all modules at once.
