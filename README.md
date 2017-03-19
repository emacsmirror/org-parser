# org-parser.el

This project parses org files into structured datatypes.

## Entry points

There are three methods that can be used to parse an org file.

1. `org-parser-parse-buffer` -- This method parses a buffer as an org file.
2. `org-parser-parse-file` -- This method parses an org file.
3. `org-parser-parse-string` -- This method parses a string as an org file.

Each function returns a list of structure objects.

## Org structure objects

A structure object is a hash table that represents an item in an org file. A structure has the following keys:

* `:text` -- the text on the first line of the block.
* `:properties` -- the propreties of the block, as an alist. For example:
```
* this is the 'text'
:PROPERTIES:
:a key: And here's a value!
:another key: Other thing.
:END:
```
results in a properties alist of

`'(("a key" . "And here's a value!")
  ("another key" . "Other thing."))`
  
Note that the properties are in the alist are in the same order as the source org file.

* `:body` -- the text on following lines of the block, as a list, where each line is represented by a list of items.
For example:
```
* this is the 'text'
  This is the 'body', which can
  continue to multiple lines.
```
Results in:
`'((\"This is the 'body', which can\") (\"continue to multiple lines.\"))`

* `:children` -- a list of structure objects, one for each child of the original item. If there are no children, this will be `nil`.
* `:bullet-type` -- a character indicating the type of bullet used, either `*`, `-`, `+`, `.`, or `)` .  For ordered lists (either `)` or `.`) this is the character /after/ the number. For headlines, `*`, even if the item the structure represents is nested, and has multiple asterisks.

## Release History

### 0.3

* Parse properties drawers.

### 0.2

* Allow for the first line to not be a headline.
* Rename to org-parser.
* Better Emacs code style.
* We now require Emacs 25.1 or greater.

### 0.1

* Initial release.
* Work with headlines, plain lists.
