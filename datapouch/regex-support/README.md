# Regex support module

Anything regarding simplification of regex usage goes here.

Draft 0 + 0.0(1) + i.

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
`REGEX-FROM-STRING` and `MAKE-NAMED-GROUP` accept strings as their arguments, and handle them as if they were string representations of regular expressions.
`D.IFACE:CONCAT` (`CONCAT-TWO`) and `D.IFACE:COMBINE` (`COMBINE-TWO`) allow for objects of `REGEX` class to be concatenated with strings seamlessly, transforming these strings into regexes in the process.
Note that `MAKE-NAMED-GROUP` has optional argument `INFO`: this is the aforementioned user-data to be passed around.

Then, one should compose smaller parts into bigger ones with the use of
* `WRAP-IN-NONCAPTURING-GROUP`
* `MAKE-NAMED-GROUP`
* `MAKE-OPTIONAL`
* `D.IFACE:CONCAT` / `CONCAT-TWO`
* `D.IFACE:COMBINE` / `COMBINE-TWO`
* `INTERCHANGE`
* `INTERCHANGE-THREE`

Then, completed regex should be transformed into a scanner with `MAKE-SCANNER`.

The scanner can be used with `SCAN` to get match intervals for named groups/registers.

Match results could be used with `MATCH-TO-ASSOC` or `SEGMENT-STRING-WITH-MATCH`.

Segmented string (as well as filtered matched groups) could be used with `RECONSTRUCT-MATCH-TREE` to get the tree.

Or, one could use aggregate function `SCAN-TO-TREE` that does all that with just a scanner and a string, hiding all argument passes under the hood. Voila.

Because of its convenience it could be misused *very easily*. That is why there is an explicit blanket ban on its usage for the purposes of expression/pattern building.

Theoretically, raw object of `REGEX` class can be used for `SCAN`, `MATCH-TO-ASSOC`, and all others, but then, baked-group-map, group-list (and sometimes regex-tree-topology) must be prepared manually.
It's not that hard, but tiresome.
For the first one, one call to `BAKE-GROUP-MAP` should be enough.
For the second one, the user should just collect all groups from baked map, sequentially into a list.
For the third one (when needed), a combined call to `(BAKE-REGEX-TREE (GET-REGEX-TREE-TOPOLOGY (TREE REGEX)))` is necessary.
That should be it.

But instead of dealing with that, all these operations were included in the process of producing a scanner.
Because scanner is no longer mutable, and groups won't need their indices to be updated anymore.
Use scanners, they are cool.
