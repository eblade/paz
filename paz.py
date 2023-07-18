#!/usr/bin/env python3

import os
import sys
import hashlib
import base64
import re
import argparse
import time


class Resources:
    trans = str.maketrans('+/=', '98A')
    startsWithLowerCase = re.compile(r'^[a-z]')
    containsUpperCaseLetter = re.compile(r'[A-Z]')
    containsNumeral = re.compile(r'[0-9]')


def encode(s: bytes) -> str:
    encoded = base64.b64encode(s).decode('utf-8')
    return encoded.translate(Resources.trans)


class HashFunctions:
    @classmethod
    def get(klass, algorith):
        return getattr(klass, 'hash_' + algorith)

    @staticmethod
    def hash_sha256(s: bytes) -> bytes:
        m = hashlib.sha256()
        m.update(s)
        return m.digest()

    @staticmethod
    def hash_sha512(s: bytes) -> bytes:
        m = hashlib.sha512()
        m.update(s)
        return m.digest()

    @staticmethod
    def hash_md5(s: bytes) -> bytes:
        m = hashlib.md5()
        m.update(s)
        return m.digest()


def check_it(s):
    return None not in (
        Resources.startsWithLowerCase.match(s),
        Resources.containsUpperCaseLetter.search(s),
        Resources.containsNumeral.search(s),
    )


def iterate(s: str, hash_fun, length=15, min_iters=10, debug=False, debug_lite=False) -> str:
    n = 0
    while True:
        s = hash_fun(s.encode('utf-8'))
        s = encode(s)
        n += 1
        if debug or (debug_lite and n >= (min_iters - 10)):
            print(n, '\t', s)

        if n < min_iters: continue

        chopped = s[:length]
        if check_it(chopped): break

    return chopped


def getpass(query: str):
    if 'win' in sys.platform:
        import subprocess
        sys.stdout.flush()
        sys.stderr.write(query)
        sys.stderr.flush()
        subprocess.check_call(["stty","-echo"])
        password = input()
        subprocess.check_call(["stty","echo"])
        return password
    else:
        import getpass
        return getpass.getpass(query)

def load_config(args):
    import urllib.request, configparser

    config_path = os.path.join(os.path.expanduser('~'), '.pazrc')
    remote_path = os.path.join(os.path.expanduser('~'), '.pazrc.remote')
    config = configparser.ConfigParser()

    if os.path.exists(config_path):
        config.read(config_path)

    default = config['DEFAULT']
    remote = default.get('remote')
    remote_gpg = default.getboolean('remote-gpg')
    if remote and not args.no_remote:
        if args.update_remote:
            print('Downloading', remote)
            req = urllib.request.Request(remote, headers={'User-Agent': 'Mozilla/5.0'})
            with urllib.request.urlopen(req) as response:
                remote_data = response.read()
            print('Download complete', len(remote_data), 'bytes')
            with open(remote_path, 'wb') as f:
                if remote_gpg:
                    import gpg
                    print('Remote config is encrypted. Need key.')

                    with gpg.Context() as c:
                        try:
                            # Do not use the default passphrase dialog (this succeeds
                            # if the passphrase is cached)
                            plain, result, _ = c.decrypt(remote_data, passphrase='blerg')

                            # If failing, this means the passphrase was not cached. Need
                            # ask for a hashed one for the site "remote"
                        except gpg.errors.GPGMEError:
                            remote_site = config['remote']
                            if not remote_site:
                                raise KeyError('Missing section in local config: [remote]')
                            complete_args(default, args, remote_site)
                            master = get_master(args)

                            if not master:
                                print('Abort.')
                                exit(0)

                            passphrase = get_result(args, master, 'remote')
                            plain, result, _ = c.decrypt(remote_data, passphrase=passphrase)

                        if result:
                            print('Decryption successful. Writing to', remote_path)
                            f.write(plain)
                else:
                    print('Remote config is not encrypted. Writing to', remote_path)
                    f.write(remote_data)

        if os.path.exists(remote_path):
            config.read(remote_path)
            config.read(config_path)

    if args.site is None:
        for site in config.sections():
            print(site)

    if args.site is not None and args.site in config.sections():
        site = config[args.site] or default
    else:
        site = default

    complete_args(default, args, site)


def complete_args(default, args, site):
    if args.hash is None:
        args.hash = site.get('hash', default.get('hash', 'sha512'))
    if args.length is None:
        args.length = site.getint('length', default.getint('length', 15))
    if args.min_iterations is None:
        args.min_iterations = site.getint('min-iterations', default.getint('min-iterations', 10))
    if args.revision is None:
        args.revision = site.get('revision', default.get('revision'))
    if args.addition is None:
        args.addition = site.get('addition', default.get('addition'))
    if args.wait_time is None:
        args.wait_time = site.getint('wait-time', default.getint('wait-time', 15))
    if args.bishop_path is None:
        args.bishop_path = default.get('bishop-path')
    if args.strategy is None:
        args.strategy = site.get('strategy', default.get('strategy', 'default'))
    if args.username is None:
        args.username = site.get('username', default.get('username'))


def bishop(path, s):
    import subprocess
    bishop_path = os.path.expanduser(os.path.expandvars(path))
    m = hashlib.sha512()
    m.update((s + '\n').encode('utf8'))
    hashed = m.hexdigest()
    sys.stderr.write(subprocess.run([bishop_path, hashed], capture_output=True, encoding='utf8').stdout)

def get_master(args):
    if args.master:
        return args.master
    else:
        if args.username:
            question = 'Password for %s (%s): ' % (args.username, args.strategy)
        else:
            question = 'Password (%s): ' % (args.strategy, )
        return getpass(question)


def get_result(args, master, site=None):
    seed = master + ':' + (site or args.site)
    if args.revision:
        seed += str(args.revision)

    if args.verbose:
        print('Using hash', args.hash)

    hash_fun = HashFunctions.get(args.hash)

    result = iterate(seed, hash_fun, length=args.length, min_iters=args.min_iterations, debug=args.debug, debug_lite=args.debug_lite)
    if args.addition:
        result += args.addition

    return result


if __name__ == '__main__':
    parser = argparse.ArgumentParser('paz')
    parser.add_argument('site', nargs='?', help='Site to render password for')
    parser.add_argument('-m', '--master', help='Master password')
    parser.add_argument('-H', '--hash', help='Hash algorithm')
    parser.add_argument('-n', '--length', type=int, help='Password length, not counting addition')
    parser.add_argument('-i', '--min-iterations', type=int, help='Minimum number of iterations')
    parser.add_argument('-r', '--revision', type=int, help='Add this number to the site')
    parser.add_argument('-a', '--addition', help='Add this suffix to the password')
    parser.add_argument('-s', '--stdout', action='store_true', help='Print password to stdout')
    parser.add_argument('-l', '--linebreak', action='store_true', help='Adds a linebreak to the result')
    parser.add_argument('-c', '--copy', action='store_true', help='Copy result to secondary clipboard (Ctrl-C)')
    parser.add_argument('-p', '--primary', action='store_true', help='Copy result to primary clipboard (Middle-click)')
    parser.add_argument('-R', '--no-remote', action='store_true', help='Do not load remote configs')
    parser.add_argument('-u', '--update-remote', action='store_true', help='Update the latest remote')
    parser.add_argument('-w', '--wait', action='store_true', help='Wait after copy and reset clipboard afterwards')
    parser.add_argument('-W', '--wait-time', type=int, help='Time to wait after copy')
    parser.add_argument('-v', '--verbose', action='store_true', help='Show the resulting config')
    parser.add_argument('-d', '--debug', action='store_true', help='Print some debug info along the way')
    parser.add_argument('-D', '--debug-lite', action='store_true', help='Print a little debug info along the way')
    parser.add_argument('-b', '--bishop', action='store_true', help='Use bishop to paint random art on stderr')
    parser.add_argument('--bishop-path', help='Path to bishop executable for random art')
    parser.add_argument('--strategy', help='Specify a strategy')
    parser.add_argument('--username', help='Specify a username')
    args = parser.parse_args()

    load_config(args)

    if args.stdout and (args.copy or args.primary):
        print('Cannot specify both stdout and copy/primary')

    if args.verbose:
        print('site =', args.site)
        print('hash =', args.hash)
        print('length =', args.length)
        print('min-iterations =', args.min_iterations)
        print('revision =', args.revision)
        print('addition =', args.addition)
        print('stdout =', args.stdout)
        print('linebreak =', args.linebreak)
        print('copy =', args.copy)
        print('primary =', args.primary)
        print('wait =', args.wait)
        print('no-remote =', args.no_remote)
        print('update-remote =', args.update_remote)
        print('wait =', args.wait)
        print('wait-time =', args.wait_time)
        print('strategy =', args.strategy)
        print('username =', args.username)
        print('debug =', args.debug)
        print('debug-lite =', args.debug_lite)

    if not args.site:
        exit(0)

    master = get_master(args)

    if not master:
        exit(0)

    if args.bishop and args.bishop_path is not None:
        bishop(args.bishop_path, master)

    result = get_result(args, master)

    if args.linebreak:
        result += os.linesep

    if args.stdout:
        sys.stdout.write(result)

    if args.copy or args.primary:
        import pyperclip
        pyperclip.copy(result, primary=args.primary)
        if not args.stdout:
            print('Copied the password to clipboard', flush=True)
            if args.wait:
                print('It will expire in %d seconds' % args.wait_time, flush=True)

        if args.wait:
            time.sleep(args.wait_time)
            pyperclip.copy('x', primary=args.primary)
            print('The password has expired', flush=True)
