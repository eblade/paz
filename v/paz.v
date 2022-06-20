module main

import os
import flag
import crypto.sha256
import crypto.sha512
import crypto.md5
import encoding.base64
import ui
//import clipboard

[heap]
struct State {
	mut:
	window &ui.Window = voidptr(0)
	master string
	site string
	revision string
	addition string
	length string
	hash &ui.Radio
	password &ui.Label
}

fn main() {
	mut app := &State{
		hash: ui.radio(
			width: 200
			values: ['md5', 'sha256', 'sha512']
			title: 'Hash function'
		)
		password: ui.label(text: 'n/a')
	}
	window := ui.window(
		width: 700
		height: 500
		title: 'Paz'
		children: [
			ui.row(
				margin: ui.Margin{10, 10, 10, 10}
				widths: [200.0, ui.stretch]
				spacing: 30
				children: [
					ui.column(
						spacing: 13
						children: [
							ui.textbox(
								max_len: 30
								width: 200
								placeholder: 'Master password'
								text: &app.master
								is_focused: true
								is_password: true
							),
							ui.textbox(
								max_len: 30
								width: 200
								placeholder: 'Site'
								text: &app.site
							),
							ui.textbox(
								max_len: 30
								width: 200
								placeholder: 'Revision'
								text: &app.revision
								is_numeric: true
							),
							ui.textbox(
								max_len: 30
								width: 200
								placeholder: 'Addition'
								text: &app.addition
							),
							ui.textbox(
								max_len: 30
								width: 200
								placeholder: 'Length'
								text: &app.length
								is_numeric: true
							),
							app.hash,
							ui.row(
								spacing: 5
								children: [
									ui.button(
										text: 'Generate'
										on_click: app.on_generate
									),
									app.password,
								]
							),
						]
					)
				]
			),
		]
	)
	app.window = window
	ui.run(window)
}

fn (mut app State) on_generate(b &ui.Button) {
	length := app.length.int()
	revision := app.revision.int()
	mut job := Job{
		site: app.site
		master: app.master
		hasher: get_hasher(app.hash.selected_value())
		length: if length == 0 { 15 } else { length }
		revision: revision
		addition: app.addition
		min_iterations: 10
		linebreak: false
	}
	job.calculate()
	app.password.set_text(job.data)
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
