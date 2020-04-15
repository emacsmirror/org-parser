# Org Parser

This project parses org files into structured datatypes.

It has a [bug tracker](https://todo.sr.ht/~zck/org-parser).

## Entry points

There are three methods that can be used to parse an org file.

1. `org-parser-parse-buffer` -- This method parses a buffer as an org file.
2. `org-parser-parse-file` -- This method parses an org file.
3. `org-parser-parse-string` -- This method parses a string as an org file.

## Returned values

Parsing Org data returns a single hash with two keys: `:in-buffer-settings` and `:content`.

### In-buffer settings

`:in-buffer-settings` corresponds to [Org's in-buffer settings](http://orgmode.org/manual/In_002dbuffer-settings.html#In_002dbuffer-settings), which are global settings for the file.

### Content structure objects

`:content` corresponds to the data in the file: the headlines, plain lists, etc. This data is represented by a list of structure objects.

A structure object is a hash table that represents an item in an org file. A structure object has the following keys:

* `:text` -- the text on the first line of the block. This is a list of text items.
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
  
Because properties and property values are strings, you can't use `#'alist-get`, but must use `#'assoc` to get properties here. Properties are in the alist are in the same order as the source org file.

* `:body` -- the text on following lines of the block, as a list, where each line is represented by a list of text items.
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

### Text items

A text item is a string or a hash table representing some text. If the represented text has no properties, its text item is a string. If the text is a link, it's represented by a hash table.

All text item hash tables have the key `:type`, which designates the type of text being represented. The only current possible `:type` is `:link`.

#### Link text items

Link text items have the following keys:

* `:type` -- this is always `:link`.
* `:target` -- the target of the link.
* `:text` -- the text of the link.

For example, this org link:

    [[https://hg.sr.ht/~zck/org-parser][the org parser repository]]

Results in a hash table with `:target` `"https://hg.sr.ht/~zck/org-parser"` and `:text` `"the org parser repository"`.

## Release History

### 0.4

* The top-level returned value is now a hash table. Current keys are `:content` and `:in-buffer-settings`

### 0.3

* Parse properties drawers.
* Parse tags.

### 0.2

* Allow for the first line to not be a headline.
* Rename to org-parser.
* Better Emacs code style.
* We now require Emacs 25.1 or greater.

### 0.1

* Initial release.
* Work with headlines, plain lists.
