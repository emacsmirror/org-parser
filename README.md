# org-parser.el

This project parses org files into structured datatypes.

## Entry points

There are three methods that can be used to parse an org file.

1. `org-parser/parse-buffer` -- This method parses a buffer as an org file.
2. `org-parser/parse-file` -- This method parses an org file.
3. `org-parser/parse-string` -- This method parses a string as an org file.

Each function returns a list of structure objects.

## Org structure objects

A structure object represents a node in an org file. A structure has the following keywords:

* `:text` -- the text on the first line of the block.
* `:body` -- the text on following lines of the block, as a list, where each line is represented by a list of items.
For example:
```
* this is the 'text'
  This is the 'body', which can
  continue to multiple lines.
```
Results in:
`'((\"This is the 'body', which can\") (\"continue to multiple lines.\"))`

* `:children` -- a list of child structure objects.
* `:bullet-type` -- a character indicating the type of bullet used, either `*`, `-`, `+`, `.`, or `)` .  For ordered lists (either `)` or `.`) this is the character /after/ the number. For headlines, `*`, even if the node the structure represents is nested, and has multiple asterisks.