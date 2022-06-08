module main

import os
import flag
import crypto.sha256
import crypto.sha512
import crypto.md5
import encoding.base64

fn main() {
	mut fp := flag.new_flag_parser(os.args)
	fp.application('paz')
	fp.version('v0.1')
	fp.limit_free_args(0, 1)?
	fp.description('Generate passwords from a master password and a site-specific seed')
	fp.skip_executable()

	hash := fp.string('hash', `H`, '', 'The hash function to use (md5, sha256 or sha512)')
	master := fp.string('master', `m`, '', 'The master password (will be asked for if omitted)')
	length := fp.int('length', `n`, 0, 'Generated password length')
	min_iterations := fp.int('min-iterations', `i`, 0, 'Hash the data at least this many times')
	revision := fp.int('revision', `r`, 0, 'Password revision (bump when changing password)')
	addition := fp.string('addition', `a`, '', 'Password suffix (if special char is needed)')
	stdout := fp.bool('stdout', `s`, false, 'Print password to stdout')
	linebreak := fp.bool('linebreak', `l`, false, 'Add a linebreak to the result')
	verbose := fp.bool('verbose', `v`, false, 'Print additional information to stderr')
	additional_args := fp.finalize() or {
		eprintln(err)
		println(fp.usage())
		return
	}
	site := additional_args[0]
	mut job := Job{
		site: site
		master: master
		hasher: get_hasher(hash)
		length: if length == 0 { 15 } else { length }
		revision: revision
		addition: addition
		min_iterations: if min_iterations == 0 { 10 } else { min_iterations }
		linebreak: linebreak
	}
	job.calculate()
	if verbose {
		eprintln(job)
	}
	if stdout {
		print(job.data)
	}
}


struct Job {
	site string [required]
	master string [required]
	hasher fn(data []u8) []u8
	length int = 15
	revision int
	addition string
	min_iterations int = 10
	linebreak bool
mut:
	iterations int
	data string
}

fn get_hasher(hash string) fn(data []u8) []u8 {
	return match hash {
		'sha256' { sha256.sum }
		'sha512' { sha512.sum512 }
		'md5' { md5.sum }
		//else { none }
		//else { error('Unknown hash: $hashname') }
		else { md5.sum }
	}
}

fn (mut job Job) calculate() {
	for {
		if job.iterate() {
			break
		}
	}
}

fn (mut job Job) iterate() bool {
	if job.iterations == 0 {
		job.data = '$job.master:$job.site'
		if job.revision > 0 {
			job.data = '$job.data$job.revision'
		}
	}

	mut data := job.data.bytes()
	data = job.hasher(data)
	job.data = base64.encode(data)

	job.iterations++
	job.data = job.data
		.replace('+', '9')
		.replace('/', '8')
		.replace('=', 'A')
	if job.iterations < job.min_iterations {
		return false
	}

	chopped := job.data[..job.length]

	if starts_with_lowercase(chopped)
		&& contains_uppercase_letter(chopped)
		&& contains_numeral(chopped) {
			job.data = chopped
			if job.linebreak {
				job.data = job.data + '\n'
			}
			return true
		} else {
			return false
		}

}

fn starts_with_lowercase(s string) bool {
	return match s[0] {
		`a`...`z` { true }
		else { false }
	}
}

fn contains_uppercase_letter(s string) bool {
	mut is_match := false
	for c in s {
		is_match = match c {
			`A`...`Z` { true }
			else { false }
		}
		if is_match {
			return true
		}
	}
	return false
}

fn contains_numeral(s string) bool {
	mut is_match := false
	for c in s {
		is_match = match c {
			`0`...`9` { true }
			else { false }
		}
		if is_match {
			return true
		}
	}
	return false
}
