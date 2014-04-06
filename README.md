strp
====

A program that attempts to mimic plan9 string plumbing.

### Modules ###

strp uses a modular design, allowing users to add or remove modules as they
wish. A module is a regular expression pattern matches with a function from
String to IO (). In the configuration file, strp.hs, users pass a list of
`StrpModule`s to the strp function, along with the commnand line arguments.
This list of modules is searched linearly until a match is found, at which
point the modules function is called on the input string. If no modules match
the input, the defualt behavior is to print: `strp: <str>: no modules used` to
stdout to notify the user that nothing has been done.

### Usage ###

strp can be invoked by passing the string you want processed as an argument, or
with the `-c` flag to read out of the X primary clipboard (highlighted text).

Example (using only urlModule):

    $ strp www.fsf.org

This invokes strp with the string `www.fsf.org`. If the user is using the
provided urlModule to catch urls, then this would call `$BROWSER www.fsf.org`.
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
