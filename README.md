# Guitéliq – a toolset for knowledge work

[![GitHub CI](https://github.com/armeninants/guiteliq/workflows/Haskell-CI/badge.svg)](https://github.com/armeninants/guiteliq/actions)
[![stability-experimental](https://img.shields.io/badge/stability-experimental-orange.svg)](https://github.com/mkenney/software-guides/blob/master/STABILITY-BADGES.md#experimental)

<!-- [![armeninants](https://circleci.com/gh/armeninants/guiteliq.svg?style=shield)](https://circleci.com/gh/armeninants/guiteliq) -->

![Haskell](https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white)

Simple TUI (terminal user interface) for accessing your `PDF`, `DjVu` readings, and `LaTeX`, `Markdown` writings, using your preferred editors and viewers.

### Assumptions

- You keep all your writings and readings within your home directory (`~`).
- The writings that you don't want to be listed have an extension prefix `.inc`, i.e. `.inc.tex` or `.inc.md`.

## Structure

- [`ai-writings` - navigate and search your LaTeX docs](#ai-writings---navigate-and-search-your-latex-docs)
- [`ai-readings` - single access point for your library](#ai-readings---single-access-point-for-your-library)
- [Installation](#installation)
  - [Compile from the source](#compile-from-the-source)
  - [Download a binary release](#download-a-binary-release)
- [Configuration](#configuration)
- [How to customize LaTeX templates](#how-to-customize-latex-templates)

## `ai-writings` - navigate and search your LaTeX docs

<img src="docs/assets/writings.gif">

- Lists all `.tex` and `.md` files (except `.inc.tex` and `.inc.md`) from your *Writings* directory (see [Configuration](#configuration)) and its subdirectories.
- Fetches the titles of `.tex` documents.
- Recently modified documents appear higher on the list.

## `ai-readings` - single access point for your library

<img src="docs/assets/readings.gif">

- Lists all `.pdf` and `.djvu` files from your *Readings* directory (see [Configuration](#configuration)) and its subdirectories.

## Installation

### Compile from the source
Haskell [stack](https://docs.haskellstack.org/en/stable/) is a prerequisite, installation instructions can be found [here](https://docs.haskellstack.org/en/stable/).

Run one of the commands below from the project's root directory:
```bash
# To compile the whole project:
stack install
# Or
# To compile a single app:
stack install guiteliq:ai-readings
stack install guiteliq:ai-writings
```

Et voilà, Guitéliq is up and running!
`ai-writings` and/or `ai-readings`, are located at `~/.local/bin`.

### Download a binary release
You can download binary directly [from GitHub releases](https://github.com/armeninants/guiteliq/releases).

After downloading binary, make it executable and copy it under convenient location, for example:

```bash
chmod +x ai-readings-osx ai-writings-osx
mv ai-readings-osx ~/.local/bin/ai-readings
mv ai-writings-osx ~/.local/bin/ai-writings
```

## Configuration

The config file `~/.guiteliq/config.json` will be created upon the first start of either app.
Here are the configuration attributes:

| Attribute                 | Description                                                                                                                                                                       |
| ------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| *writings.root.directory* | The root directory for all your writings (`.tex` documents), relative to the home directory. The default value is `Documents/Writings`, which resolves to `~/Documents/Writings`. |
| *readings.root.directory* | The root directory for all your readings (`.pdf` and `.djvu` documents), relative to the home directory. The default value is `Documents/Library`.                                |
| *md.cmd.prefix*           | The command for opening `.md` files.                                                                                                                                              |
| *latex.cmd.prefix*        | The command for opening `.tex` files.                                                                                                                                             |
| *djvu.cmd.prefix*         | The command for opening `.djvu` files.                                                                                                                                            |
| *pdf.cmd.prefix*          | The command for opening `.pdf` files.                                                                                                                                             |
| *editor.prefix*           | The command for opening all other files. Set to use your preferred editor, e.g. `vim`, `emacs`, or other. The default value is `code`.                                            |


## How to customize LaTeX templates

The default templates of LaTeX documents are in the `~/.guiteliq/templates` directory.
To create your own template, say `note`, create a direcory `~/.guiteliq/templates/note` and put all boilerplate files inside.

There are several conventions:
- The main `.tex` file should be called `main.tex`.
- Inside `main.tex`, there should be an (empty) command `\title{}`.
- All included files should have an extension `.inc.tex`.
