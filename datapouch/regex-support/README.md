# Regex support module

Anything regarding simplification of regex usage goes here.

Draft 0 + 0.0(2).

---

# main.lisp

Why would anyone need a regex wrapper?
For one, because named regex groups are a thing, but their support is atrocious most of the time.

## Intent

Two things should be of note here.
The first is that using named groups with `cl-ppcre` library isn't exactly a user-friendly affair.
The second is that even with named groups some functionality remains beyond one's reach.
For example, two named regex groups with the same name would be a pain to use.

The intent then, is to ease these problems, or eliminate them entirely.
The file uses `cl-ppcre`'s ability to represent regular expressions as s-forms.
Why? Because it allows to store additional information inside an augmented s-form, then strip all additional data right before `cl-ppcre` has to read the regex.
This opens some avenues. E.g.: two or more named regex groups with the same name, but different user-data attached to them.

The structural pinnacle of this intent (as far as author's skill goes, however small it is) is "scanning to tree".
The process, as clumsy as it is for general purposes, is very good for purely named groups based approach.
It transforms a string into a tree, in accord with named groups that had been matched, and puts user-data neatly right beside each match inside the tree.
This allows easy examination during tree traversal (and disambiguation in the case of same-named groups).

## Quick how to (with expressions or patterns)

Most of these functions are too low level to be of any good use, when parts of regex matches need to be processed separately.
And these processors are to be combined and annotated.
For usage in expressions and/or patterns, please refer to '*Quick how to*'s of corresponding files.

The usage of `SCAN-TO-TREE` is explicitly forbidden (via a comment in the sources).
In essence, if one uses `SCAN-TO-TREE` with expressions or patterns, then the code *smells* like a nasty fermented landfill.

## Quick how to (barebones)

Self-contained usage for other purposes (including learning), looks somewhat like this:

First, a regex should be created.
This does not mean that the user has to learn internal representation.
`REGEX-FROM-STRING`, `MAKE-NAMED-GROUP`, `D.IFACE:CONCAT` and `D.IFACE:COMBINE` accept strings as their arguments, and handle them as if they were string representations of regular expressions.
Note that `MAKE-NAMED-GROUP` has optional argument `INFO`: this is the aforementioned user-data to be passed around.

Then, one should compose smaller parts into bigger ones with the use of
* `WRAP-IN-NONCAPTURING-GROUP`
* `MAKE-NAMED-GROUP`
* `MAKE-OPTIONAL`
* `D.IFACE:CONCAT`
* `D.IFACE:COMBINE`
* `INTERCHANGE`
* `INTERCHANGE-THREE`

Then, completed regex should be transformed into a scanner with `MAKE-SCANNER`.

The scanner can be used with `SCAN`, `MATCH-TO-ASSOC`, `MATCH-TO-GROUP-TABLE` and `SCAN-TO-TREE`. Voila.

Theoretically, raw object of `REGEX` class can be used for `SCAN`, `MATCH-TO-ASSOC`, `MATCH-TO-GROUP-TABLE`, but then, baked-group-map and group-list should be prepared manually.
It's not that hard, but tiresome.
For the first one, one call to `BAKE-GROUP-MAP` should be enough.
For the second one, the user should just collect all groups from baked map, sequentially into a list.
That should be it.

But instead of dealing with that, all these operations were included in the process of producing a scanner.
Because scanner is no longer mutable, and groups won't need their indices to be updated anymore.

## External

### `ALLOW-NAMED-REGISTERS`
Lambda-list: `(&OPTIONAL (FLAG T))`

Simple wrapper around PPCRE named register toggle.
Disabling it makes the whole file useless.

### Class `REGEX`
Slots:
* `TREE` (reader `TREE`; initarg `:TREE`)
* `GROUP-MAP` (reader `GROUP-MAP`; initarg `:GROUP-MAP`)

`REGEX` class objects are wrappers around PPCRE's regexes.

They retain information about named groups in them.
GROUP-IDs help to differ between same-named groups for matching, while contextual info contains critical user information for match processing.

### `REGEX-FROM-STRING`
Lambda-list: `(STRING)`
Declaim: `(FUNCTION (STRING))`

Make `D.REGEX:REGEX` instance from a `STRING`, that contains a regex.

### Method `SCAN` for `REGEX`/`REGEX-SCANNER`
Wrapper for `PPCRE:SCAN`.

### Method `WRAP-IN-NONCAPTURING-GROUP`
Equivalent to `\rx -> (?:rx)`.

### Method `MAKE-OPTIONAL`
Equivalent to `\rx -> (?:rx)?`.
Additional noncapturing-group is due to the nature of regexes in s-expression form.
Regex part cannot 'leak', so to speak, so no `abc -> abc?` situations.

### Method `MAKE-NAMED-GROUP`
Equivalent to `\name rx -> (?<name>rx)`
Additionally, it can store an optional user info, that is bound to the group.
This feature is used heavily in expression.lisp and pattern.lisp of command module.

### Methods `CONCAT-TWO`, `COMBINE-TWO`

### `INTERCHANGE`
Lambda-list: `(SEPARATOR ONE ANOTHER)`
Declaim: `(FUNCTION (RELAXED-REGEX RELAXED-REGEX RELAXED-REGEX))`

Make regex that matches two separated regexes in any order.
Equivalent to `\<rx1> <rx2> <separator> -> (?:<rx1><separator><rx2>|<rx2><separator><rx1>)`.

### `INTERCHANGE-THREE`
Lambda-list: `(SEPARATOR-REGEX FIRST-REGEX SECOND-REGEX THIRD-REGEX)`

Make regex that matches three separated regexes in any order.
Equivalent to concatenating all six permutations.

### Class `REGEX-SCANNER` 
Slots:
* `SCANNER` (reader `SCANNER`; initarg `:SCANNER`)
* `GROUP-MAP` (reader `GROUP-MAP`; initarg `:GROUP-MAP`)
* `GROUP-LIST` (reader `GROUP-LIST`; initarg `:GROUP-LIST`)

### Method `MAKE-SCANNER`
Makes `D.REGEX:REGEX-SCANNER` from `D.REGEX:REGEX` object, converting group-map into a baked group-map and a group-list.

### `*MAKE-GROUP-RESULT-FUN*`
Lambda-list: `(GROUP-NAME GROUP-INFO MATCH)`

When matched groups need to be substituted in the tree (or somewhere else), this is the function that regex wrapper uses to shape it.
Note that user info is in there, as `GROUP-INFO`.
This is the point where named group match result will be injected into resulting match tree.
When variable is set to `NIL` (and by default it is), the function will be
```
(lambda (group-name group-info match)
  (list group-name group-info match)))
```

### `MATCH-TO-ASSOC` 
Lambda-list: `(STR GROUP-LIST GROUP-MAP MATCH-START MATCH-END GROUP-STARTS GROUP-ENDS)`

This function, along with `MATCH-TO-GROUP-TABLE` are made to be used after a successful `SCAN`.

## Internal

### Class `GROUP-ID`
Slots:
* `NAME` (reader `NAME`; initarg `:NAME`)
* `INDEX` (reader `INDEX`; initarg `:INDEX`)

`GROUP-ID` represents indexed group id.
This allows for differentiating between multiple groups with the same name.

### `MAKE-GROUP-ID`
Lambda-list: `(GROUP-NAME GROUP-INDEX)`
Declaim: `(FUNCTION (STRING INTEGER))`

Shortcut for `GROUP-ID` class instance creation.

### `BAKE-GROUP-ID`
Lambda-list: `(GROUP-ID)`
Declaim: `(FUNCTION (GROUP-ID))`

Group names inside regex must be strings with restricted character set.
'Baking' just serializes `GROUP-ID` object in some regex-friendly manner.

### Type `GROUP-MAP` / `GROUP-MAP-P`
Designates either:
* `NIL`,
* Assoc with elements `(GROUP-ID :NAME STRING :INFO T)`:
  * First element is of type `GROUP-ID`,
  * Rest is a p-list:
    * `:NAME` must be a string; named group name,
    * `:INFO` can be anything; user info to be carried with the named group.

### `BAKE-REGEX-TREE`
Lambda-list: `(REGEX-TREE)`

Prepare regex-tree, so it can be read with PPCRE lib. Indices can no longer
be changed.

### `BAKE-GROUP-MAP`
Lambda-list: `(GROUP-MAP)`

Prepare group-map to be used along regex-tree. Indices cannot be changed
anymore, unless this is reverted, so 'baking'.

### `SHIFT-GROUP-ID-INDEX`
Lambda-list: `(GROUP-ID OFFSET)`
Declaim: `(FUNCTION (GROUP-ID INTEGER))`

### `SHIFT-GROUP-INDICES-IN-TREE`
Lambda-list: `(REGEX-TREE OFFSET)`
Declaim: `(FUNCTION (T INTEGER))`

### `SHIFT-GROUP-MAP-INDICES`
Lambda-list: `(GROUP-MAP OFFSET)`
Declaim: `(FUNCTION (GROUP-MAP INTEGER))`

Add an offset to indices of named groups in the whole regex-tree/group-map.
Helper functions for all concatenations, combinations, etc.
