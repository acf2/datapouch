Fast dirty comment, that does not explain crap.

This facility is an another layer (or two) of abstraction from rmacro
commands. Rmacro commands give the user an ability to have handlers for
user defined commands.
Its functionality is rather simple:
  - User must provide a regex and a function
  - When, during rmacro processing, regex matches, user function is called
    with two arguments:
    - Full command string, that matched the regex
    - All match groups from matched regex
It's simple, but with named regex groups (NRGs) still kinda viable.

There is a problem, however. What if some parts of regexes are repetitive?
As a solution, s-forms are introduced.
S-form is essentially just another way to write a regex.

Here is how it looks like in EBNF:
```ebnf
 <s-form>       ::= <command> {<term>}
 <command>      ::= <regex>
 <term>         ::= <simple term> | <complex term>
 <simple term>  ::= <regex> | (<name> . <regex>)
 <complex term> ::= (<simple term> <modifier> {<modifier>})
 <modifier>     ::= :optional | :immediate | :optionally-immediate
 <regex>        ::= <string> | <d.regex:regex>
 <name>         ::= <keyword> | <string>
```

More info about modifiers in sources of make-regex-s-form-scanner
function.
The definition of <name> is changed later on.

With this abstraction, one could write regexes like this:
```lisp
 (let* ((substring-rx ".*")
        (substring-arguments `(((:substring . ,substring-rx) :optional)))
        (tags-rx "(?:\\w+,)*\\w+")
        (tag-arguments `((,(concat "\\+" (make-named-group :ptags tags-rx)) :optional)
                         (,(concat "-" (make-named-group :ntags tags-rx)) :optional)))
        (exponent-arguments `(((:exponent . "[1-9]\\d*") :optional)))
        (link-type-arguments `(((:type . "forward|back") :optional)))
        (link-arguments `(,@link-type-arguments
                          ,@exponent-arguments
                          ((:closure . "\\*") :optional :immediate)
                          ,@tag-arguments
                          ,@substring-arguments))
        (goto-rxs `(("goto" ,@link-arguments)))
        (links-rxs `(("links" ,@link-arguments))))
   bla-bla-bla)
```

Notice that nothing restricts user from using d.regex:regex objects.
Hence these "out-of-place" concats in tag-arguments.

Now another problem arises.
For each of these commands, user should check its input correctness.
That's a big chunk of boilerplate code, that goes nowhere.
Also, the use of match groups wears down anyone with considerable amount of
commands.

Enter the second layer of abstraction: subexpressions and fixed argument names.

Fixed argument names are a simple one. Now, user handler has key arguments
instead on the usual command-string and match pair.
All NRGs now _must_ only use keywords as their names, and NRGs names must
match user handler key arguments.
Thus, user explicitly awaits only these arguments.

Subexpressions are a little more complex.

Henceforth expression is a pair of s-form and user handler function.
Expressions can be terminal (complete) or can be subexpressions.
The difference? Subexpressions are used as arguments for other expressions.

How is it used? User can consolidate expressions in objects of a shell class.
(Name's not great, but it delivers the point across)
There can be special subexpression forms inside any expression s-form.
They look like `(<name> . <type>)`, where:
 - `<name>` is a specific name of this argument for user handler (like in NRGs),
 - `<type>` is a key that user specifies himself for referring to subexpression in expression table
(it's better to use keywords for both `<name>` and `<type>`).

Then, when user has completed shell instance, he calls build-shell with his object as argument.

Every s-form is ground up to simpler s-forms:
 - Each subexpression is substituted to it's s-form,
 - Each produced NRG has its name scoped with previously used `<name>` of subexpression.

Every user handler is wrapped:
 - "Args info" is formed, that consist of subexpression names and it's NRGs'
   names. They're used only in wrapped handlers, to identify NRGs that belong
   to certain subexpression, and for scoping of these names.
 - Wrapped user handlers are formed as lambdas, that accept all keys
   (allow-other-keys). Then they call wrapped user handlers of subexpressions
   (with approprietly scoped names). After that, they call their user handler,
   using all the returned subexpression values, as well as directly required
   NRGs.

These "processed s-forms" are effectively the same as previously used simple
s-forms for regexes. So they can be easily transformed into regexes.

These "wrapped user handlers" are accepting any keyword arguments. So they can
be wrapped in another lambda (proxy-command) with only two arguments:
command-string and match.  And then all matched groups can be passed down to
wrapped handler as keyword arguments. This is why new <name> for s-forms always
*must* be a keyword. And this is why args info is needed: to get necessary
keyword arguments from lambda p-list without the need of reflection.

So, after that user must get all built expressions (s-forms and wrapped user
handlers) and transform them again. Now s-forms are translated to regexes, and
wrapped user handlers are wrapped in proxy-commands.

Then, pairs of regex + proxy-command could be used in rmacro-commands module.

=================================================================================

IMPLEMENTATION DETAILS

Fast dirty note, that does not explain crap
but just maybe it will remind me, some months after writing

Only complete expressions can be transformed into rmacro commands.
Transformation incorporates previously used obtainment of regex from
s-form. This is why processed s-form of complete expression must contain
all the info about every regex and NRG for every subexpression: it will be used
only once, when lexification occurs.

Call process looks like this:
  - Call to rmacro command (third layer).
    - Lexify string to tokens with compiled regex.
  - Call to corresponding wrapped handler (second layer).
    It is expecting tokens/NRGs only.
    - Call to wrapped handlers of all subexpressions.
      They are expecting only tokens/NRGs too.
    - Wrapped handlers of subexpressions return their results.
  - Call to user handler (first layer).
    User handler could expect raw tokens/NRGs, as well as subexpression results.
    As a warning: subexpression results can be of any type.
    If this expression is a subexpression, then return value of user handler
    must be returned from wrapper.

"Args info" is used _only_ by user handler wrappers.
It's there so that all argument names can be constructed to eliminate
collisions.
Implementation-wise it means that args info is constructed within
lexical scope of a user handler wrapper, and then forgotten about.

EXAMPLE

```
=== EXPRESSIONS ===
(this is what user sees)

key :a,  handler = #'fun1,  s-form = (<rx1> (:g1 . <rx2>) <rx3>)
key :b,  handler = #'fun2,  s-form = (<rx4> (:g2 . <rx5>) (:some-a . :a) <rx6> <rx7> (:another-a . :a))
no key,  handler = #'fun3,  s-form = (<rx8> (:g3 . <rx9>) (:some-b . :b))

=== BUILT EXPRESSIONS ===
(this is what middle-layer sees, before compiling it into regexes and commands)

|-----|-------------------------------|----------------------------------------------------|----------------------------------------------------------|
| key | args info                      | processed s-form arguments (newlines don't matter) | subcalls in wrapper before user handler                  |
|=====|===============================|====================================================|==========================================================|
| :a  | nil                           | (<rx1> (:g1 . <rx2>) <rx3>)                        | None                                                     |
|-----|-------------------------------|----------------------------------------------------|----------------------------------------------------------|
| :b  | (#'wrap-fun1 :some-a :g1)     | (<rx4> (:g2 . <rx5>)                               | some-a =                                                 |
|     | (#'wrap-fun1 :another-a :g1)  |  <rx1> (:some-a.g1 . <rx2>) <rx3> <rx6>            |  (funcall wrap-fun1 :g1 some-a.g1)                       |
|     |                               |  <rx7> <rx1> (:another-a.g1 . <rx2>) <rx3>)        | another-a =                                              |
|     |                               |                                                    |  (funcall wrap-fun1 :g1 another-a.g1)                    | 
|-----|-------------------------------|----------------------------------------------------|----------------------------------------------------------|
|     | (#'wrap-fun2 :some-b          | (<rx8> (:g3 . <rx9>) <rx4>                         | some-b =                                                 | 
|     |   :g2                         |  (:some-b.g2 . <rx4>) <rx1>                        |   (funcall wrap-fun2 :g2           some-b.g2             |
|     |   :some-a.g1                  |  (:some-b.some-a.g1 . <rx2>) <rx3> <rx6> <rx7>     |                      :some-a.g1    some-b.some-a.g1)     |
|     |   :another-a.g1)              |  <rx1> (:some-b.another-a.g1 . <rx2>) <rx3>)       |                      :another-a.g1 some-b.another-a.g1)) |
|-----|-------------------------------|----------------------------------------------------|----------------------------------------------------------|

You can see that processed s-forms preserve *everything*. That's because they
will be ground down to regexes eventually.

P-list is better for args info. For clarity. Exampli gratia:

(:wrap-fun #'wrap-fun1
 :name :some-a
 :args '(:g1))
```

Call diagram

```
  C:command ---> C:wrap (wrap-fun3) ---> C:user (fun3)
                         |
                         v
                 B:wrap (wrap-fun2) ---> B:user (fun2)
                      |     |
                      v     v
                 A:wrap (wrap-fun1) ---> A:user (fun1)
```

Transformation process:
 1) expressions -> built expressions
    which includes:
   - s-forms to processed s-forms (NRG-only)
   - user handlers to wrapped user handlers (Again NRG only are expected)
 2) built expressions -> commands
   - processed s-forms -> regexes
   - wrapped user handlers (of complete expressions) -> command handlers

Subexpression regexes could be formed as "floating" expressions without explicit ^$
But I haven't used this capability yet.

;;;;;;;;;

Let's try again.

`(name . class)` is a designation of a subexpression in another expression
Let A, B and C be classes of expressions. A and B are subexpressions, C is a terminal expression
They depend as C -> B -> A

User handler is a function, that gets regex groups and subexpressions, and can return result
For terminal expressions this result is discarded.
User handlers should be wrapped, so that they have only regex groups as their arguments
Let's name user handler as class:user, wrapped user handler as class:wrap
Wrapped user handler arguments have all the info to call dependent wrapped user handlers
In example above: B:wrap has all the info to call A:wrap
And wrapped handler should compose all the info to call user handler.
That is why B:wrap will first call A:wrap, and with returned result of A it will call B:user.
Because B:user expects result of A class expression.

When building final reader-wrapper command from these we need two things:
 - regex
 - function that can work with regex match
This is literally the wrapper of a terminal expression

How to make these two things?
Expressions without subexpression dependensies can be used directly:
 - s-form without any subexpressions can be transformed by make-simple-command-scanner
 - wrapper for user handler should just pass arguments along
