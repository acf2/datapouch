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

---

# sampled.lisp

'Samples' are testing strings for regexes. Right ones should match, wrong ones should not.

## Intent

When many regexes are bundled at once, there could be collisions.
Sampled regexes are a naive, trivial attempt to fix that.
Do not expect much.

There is almost no code here.
`SAMPLED-REGEX` and `SAMPLED-REGEX-SCANNER` are lightweight descendants of according base classes.

They are incompatible with normal `REGEX` objects.
If `D.IFACE:CONCAT` or `D.IFACE:COMBINE` are used on a mix of normal `REGEX` and `SAMPLED-REGEX` objects, the result will always be a normal `REGEX`.
Rule of thumb: to use samples, every little bit must provide them beforehand.

This is where the strength of this file lies.
If samples for short regular expressions could be concise and neat, samples for long ones are very complex.
Concatenation and combination (alternation) methods do all this work automatically.
(Also, please, beware of combinatorial explosion.
Concatenation of N sampled regexes, even if each one has just two samples, will produce a sampled regex with (expt 2 N) samples.
Take care.)

## Quick how to

Everything mentioned in main.lisp "How to" applies here, because `SAMPLED-REGEX` objects are a drop-in replacement for `REGEX` ones.

pattern-expressions.lisp utilizes sampled regexes to check for collisions before creating `D.APP:APPLICATION` class objects.
So, no manual usage is required, if Datapouch's functionality is used in full.

Main takeaways are these:

- `SAMPLED-REGEX-FROM-STRING` allows to create small sampled regexes to combine them later - this is the main function to manually use them;
- `REGEX-ALLOWS-ALL-SAMPLES` and `REGEX-DENIES-ALL-SAMPLES` could be used with normal `REGEX` objects and any list of test strings;
- `FIND-INCOMPATIBLE-SAMPLED-REGEXES` is the main workhorse here, as it allows to check any list of sampled regexes for collisions;
- Use `MAKE-ANYCASE-SAMPLES` with extra caution - every warning about combinatorial explosion has an order of magnitude more weight here.
