# Guitéliq

Simple TUI (terminal user interface) for accessing your `PDF`, `DjVu` and `LaTeX` files on your own machine, using your preferred editors and viewers.

Written in Haskell 😎.

## `ai-writings` - navigate and search your LaTeX docs

<img src="docs/assets/writings.gif">

- Lists all `.tex` files (except `.inc.tex`) from your *Writings* directory (see [Configuration](Configuration)) and its subdirectories.
- Fetches the titles of `.tex` documents.
- Recently modified documents appear higher on the list.

## `ai-readings` - single access point for your library

<img src="docs/assets/readings.gif">

- Lists all `.pdf` and `.djvu` files from your *Readings* directory (see [Configuration](Configuration)) and its subdirectories.

## Installation

Haskell [stack](https://docs.haskellstack.org/en/stable/) is a prerequisite, installation instructions can be found [here](https://docs.haskellstack.org/en/stable/).

Run `stack install` from the project's root directory to compile the project.

Et voilà, Guitéliq is up and running!
The two command-line tools, `ai-writings` and `ai-readings`, are located at `~/.local/bin`.

## Configuration

The config file `~/.guiteliq/config.json` will be created upon the first start of either app.
Here are the configuration attributes:

<dl>
  <dt><strong>editor.prefix</strong></dt>
  <dd>Set to use your preferred editor, e.g. <tt>vim</tt>, <tt>emacs</tt>, or other. The default value is <tt>code</tt>.</dd>
  <dt><strong>writings.root.directory</strong></dt>
  <dd>The root directory for all your writings (<tt>.tex</tt> documents), relative to the home directory. The default value is <tt>Documents/Writings</tt>, which resolves to <tt>~/Documents/Writings</tt>.</dd>
  <dt><strong>readings.root.directory</strong></dt>
  <dd>The root directory for all your readings (<tt>.pdf</tt> and <tt>.djvu</tt> documents), relative to the home directory. The default value is <tt>Documents/Library</tt>.</dd>
  <dt><strong>djvu.cmd.prefix</strong></dt>
  <dd>The editor for <tt>.djvu</tt> files.</dd>
  <dt><strong>pdf.cmd.prefix</strong></dt>
  <dd>The editor for <tt>.pdf</tt> files.</dd>
</dl>

## How to customize LaTeX templates

The default templates of LaTeX documents are in the `~/.guiteliq/templates` directory.
To create your own template, say `note`, create a direcory `~/.guiteliq/templates/note` and put all boilerplate files inside.

There are several convensions:
- The main `.tex` file should be called `main.tex`.
- Inside `main.tex`, there should be an (empty) command `\title{}`.
- All included files should have an extension `.inc.tex`.
