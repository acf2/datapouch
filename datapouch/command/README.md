# Command module

Anything regarding commands goes here.

Draft 0 + 1/6

---

# reader-macro.lisp
AKA `:datapouch.command.reader-macro`

## Intent

Reader macros are a very convenient way to make your own syntax with Common Lisp. This file streamlines this, despite being somewhat inefficient in implementation.

## Quick how to

To use this file in the intended way, one must set `*rmacro-callbacks*` and call `install-command-reader-macro`. Order is not important, and `*rmacro-callbacks*` may be changed at any time (which is used later).

Each callback function is given the full string representation of a suspected command. Then, it must either parse it and do some transformations, returning `(values T <new form to eval>)`, or quickly fail and return `NIL`. So the intended way to interpret the meaning of rmacro callbacks is to think of them as of combined parser and payload function simultaneously.

E.g.: invocation of
```
/hello senile old man
```
will give reader macro callback the string `"hello senile old man"`

Then, let's suppose that it had been parsed, and the function has returned these values:
```
T
(format t "Hello, ~:(~A~)!~&" "senile old man")
```
Which will ultimately result in printing
```
Hello, Senile Old Man!
```

Notice that the function had returned a form, despite not being a macro. This is correct, as that form will be returned from reader macro unchanged and evaluated.

`install-command-reader-autoprint-hook` was made for end-user convenience. E.g.: when commands are shortened down to single characters it is very bothersome to prepend them with the same control character every time.

## External

### `*enable-execution*`
Controls whether successfully read reader macros will be evaluated, or simply discarded.
Default: `T`

### `*rmacro-callbacks*`
List of functions, each of which represents a tandem of parser+processor to be called.

Each function must take one argument:
1) String value, representing a possible command to be parsed and evaluated.

Each must return two values:
1) `T` or `NIL`, that will indicate, was parsing successful or not,
2) Resulting form to be evaluated/returned from macro, if command had been parsed successfully (= the first return value had been `T`).

### `*stop-characters*`
`command-reader-macro` will read all characters from the stream up to any character from this list.
If you want commands to contain any of these chars, or want `command-reader-macro` stop at others, then change the variable.

Default: `(list #\newline #\;)`

### `*control-character*`
It is merely a suggestion for control character, which is to be used with `set-macro-character` during set up phase.
It was made into a dynamically bound variable to alter previously written code with more ease, if need arises.
Otherwise, `install-command-reader-macro` has perfectly reasonable `:CHARACTER` keyword argument.

Default: `#\/`

### `install-command-reader-macro`
Lambda-list: `(&key ((:character character) *control-character*) ((:readtable table)))`

Before anything could be done, reader must recognize the need to call new macros. This is the chosen method of this module.
Adds character to designated readtable, allowing the reader to call `command-reader-macro` function (and iterate over rmacro callbacks).

NB: `NON-TERMINATINGP` is set to `T` because the default `#\/` character is too common.
* [CLHS: SET-MACRO-CHARACTER](https://www.lispworks.com/documentation/HyperSpec/Body/f_set_ma.htm);
* [SO: "What does the non-terminating-p argument of set-macro-character do?"](https://stackoverflow.com/questions/30699851/what-does-the-non-terminating-p-argument-of-set-macro-character-do).

### `install-command-reader-autoprint-hook`
Lambda-list: `(&key ((:character character) #\/))`

`cl-readline` library has several hooks that could be configured. Since usage of these reader macros relies heavily on one character, it could be beneficial to add this character as a prefix to any user input. This function does exactly that: it adds a pre-input hook that inserts control character as a first character.

## Internal

### `read-line-up-to`
Lambda-list: `(stream stop-characters)`

Reads line character by character, with `peek-char`+`read-char` up to one of characters from `stop-characters` list.
* [SO: "Appending character to string in Common Lisp"](https://stackoverflow.com/questions/18045842/appending-character-to-string-in-common-lisp);
* [SO: "Read input into string in Lisp reader macro"](https://stackoverflow.com/questions/30942815/read-input-into-string-in-lisp-reader-macro).
NB: Probably, one will need to allow recursive calls for peek/read-char in case if this file is combined with more "tame" and "sane" reader macro usage, i.e. not string/regex based.

### `return-to-stream`
Lambda-list: `(string stream)`

Unreads the whole string character by character.

### `command-reader-macro`
Lambda-list: `(stream char)`

Common wrapper function for all reader macro callbacks. If callback call isn't successful, returns all characters back, beside reader macro character.
