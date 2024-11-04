# paz

Paz is a password tool for the command line basd on [SGP](https://chriszarate.github.io/supergenpass/), or SuperGenPass. The idea is simple, just make a string out of the master password and the site, and hash that a few times. Then you (sort of) base64 encode it and cut it to the desired length. If it fulfills some popular random requirements like containing lower case and upper case letter and a number, we're done. If not, re-hash and repeat.

You can regard SGP as a password manager that has no vault, and definitely no cloud anything.

This CLI adds a couple of features, most importantly the possiblity to store sites and their settings in an ini file.

It is only meant to run on UNIX-like systems, but should run on wsl or cygwin.

## Implementations

This repo contains multiple implementations in various language. The "complete" ones are:

- Python (but bishop fingerprinting is a separate c program)
- Haskell
- Ocaml

Furthermore, the core functionality is also implemented in:

- Hare
- V

Because differences in the languages' argument parsers, the command lines may differ slightly, but I have tried to be consistent.

## Installation

This depends a lot on which implementation you choose, but the Python version has no further dependencies besides Python so you may just symlink it as `paz` to somewhere that is in your local path.

In the Ocaml version you can do `dune install paz` for the `ocaml/paz` directory.

## The config file

The config is stored in `$HOME/.pazrc` and follows the ini format. You can start with a `[DEFAULT]` section that overrides the program defaults and the a section for each site with further overrides. Example:

```ini
[DEFAULT]
length = 15
min-iterations = 10

[github]
hash = sha512

[reddit]
hash = md5
revision = 4
```

Avaliable settings are (a little bit depending on implementation):

- `length` (int) the generated password length (before additions)
- `min-iterations` (int) the minimum number of hash iterations to do
- `hash` (string) the hash function to use. can be MD5, SHA512, or SHA256. The latter is not supported by the original SGP js applet.
- `revision` (int) is a number that if `>0`, will be appended to the site name. This is useful for sites that require periodica; password changes.
- `addition` (string) a bit to add at the and of the password in order to conform to arbitrary password rules.
- `username` (string) will be shown at the master password prompt
- `strategy` (string) a hint towards which master password to use if you have several
- `linebreak` (yes/no) add a linebreak after the password

You are free to add any other keys as notes. They will be ignored.

Paz does no longer have any built-in way of synching the ini file, but something like syncthing and a symlink should be fine for most cases.

## Auto-completion

Paz will print out all available sites if run with no arguments. This can be used to aid auto-completion. This example in `fish` will provide this as well as two aliases for copying the results to primary or secondary clipboard:

```fish
function pac
    paz $argv -b | wl-copy
    echo "Password will self-destruct in 15 seconds"
    sleep 15
    echo -n "poff" | wl-copy
    echo "Done"
end

function pas
    paz $argv -b | wl-copy --primary
    echo "Password will self-destruct in 15 seconds"
    sleep 15
    echo "poff" | wl-copy --primary
    echo "Done"
end

complete --command paz --arguments '(paz)' -x
complete --command pac --arguments '(paz)' -x
complete --command pas --arguments '(paz)' -x
```

Notes:

- In bash, you can do something a bit uglier to achieve the same result
- This is for the Ocaml version, Python would require the `-s` flag to print the result to stdout
- This assumes the `paz` executable is in the `$PATH`

## The algorithm in detail

- The `source` is constructed as `<master>:<site>[revision]` where `[revision]` is an optional integer. The number `0` is never printed and negative numbers are not supported.
- (recursion point)
- Hash `source` with the selected hashing function
- Base64-encode the result, except the last two characters in the "alphabet" should be changed to `98` and the padding should be `A`. This is called `hash`.
- Cut `hash` to `length` and call it `password`.
- If the iteration count is less than `min-iterations`, jump to (recursion point) with `hash` as `source`.
- Check that the following rules are satisfied by `password`:
  - Has at least one upper case letter
  - Has at least one lower case letter
  - Has at least one number
  - Starts with a lower case letter
- If the rules are not satisfied, jump back to (reqursion point) with `hash` as `source`.
- Return `<password>[addition]`

## Maintainer

Johan Egneblad (johan at egneblad.se) is the author and maintainer of this program. Exceptions to thisi are:

- `pyperclip.py` is not my code
- `bishop.c` is not my code either
- The idea of SuperGenPass is not mine
