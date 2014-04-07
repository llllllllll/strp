strp
====

A program that attempts to mimic plan9 string plumbing.

### What it does ###

strp allows users to take an input string and attempt to preform an action
on it, based on certain user defined rules. For example: one may make a rule
that an input of a web URL should open a new tab in their favorite browser, or
highlighting a single word should open a search on their favorite browser. Maybe
highlighting a C function brings up the man page. strp is completely modular,
and users may decide which rules they want to define by adding or removing
`StrpModule`s in the config strp.hs file.

### Modules ###

strp uses a modular design, allowing users to add or remove modules as they
wish. A module is a pair of functions, the first being `strpMatch :: String ->
Bool`, which checks the input string to see if the module should be used, the
second is `strpFunction :: String -> IO ()`, which is the function that is
invoked when the module has been matched. To add new modules, just define
a new `StrpModule` with your rule and function, and add it to the list of
modules that are passed to the `strp` function in main.

### Usage ###

strp can be invoked by passing the string you want processed as an argument, or
with the `-c` flag to read out of the X primary clipboard (highlighted text).

Example (using only urlModule):

    $ strp www.fsf.org

This invokes strp with the string `www.fsf.org`. If the user is using the
provided urlModule to catch urls, then this would call `firefox www.fsf.org`.
Users may pass mutiple strings to strp like so:

    $ strp www.fsf.org test
    strp: test: no module used

This is the same as calling `strp www.fsf.org` and then `strp test`, and the
output reflects that.


Users may also read a string out of their X primary clipboard. The following
two calls are equivelent (assuming the primary clipboard holds `test`):

    $ strp -c
    strp: test: no module used
    $ strp "$(xclip -o)"

This feature is mainly targeted towards hotkeys, allowing users to highlight a
string, then press a hotkey to preform some action.


### Bugs and Contact ###

To report any bugs, or contact me, please email: joejev@gmail.com with a subject
of strp.
