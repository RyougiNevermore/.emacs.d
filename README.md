Ryougi's Emacs
============================

[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

This is an Emacs distribution that aims to enhance the default
Emacs experience. It alters a lot of the default settings,
bundles a plethora of additional packages and adds its own core
library to the mix. The final product offers an easy to use Emacs
configuration for Emacs newcomers and lots of additional power for
Emacs power users.

It's able to run on Windows, GNU Linux and macOS. It is compatible **ONLY with
GNU Emacs 25.0 and above**. In general you're advised to always run with the
latest stable release - currently **25.3**.

# Features

- Out of box.
- Clean and Fast.
- Quick fuzzy search (via `ivy`, `rg`, `ag` and `pt` etc.).
- Better Org support.
- Support multiple programming languages
  - C/C++
  - Python
  - Javascript/Typescript/JSON/YAML
  - HTML/CSS/XML
  - Golang
  - Markdown/Org
  - ...
- Auto completion.
- Fly syntax check.
- Fly spell check.
- Git/SVN integration.
- Projectile integration.
- Youdao dictionary integration.
- Emoji

# Prerequiste

## OS

- GNU Linux
- macOS
- Windows (Cygwin)

## GNU Emacs

Emacs 25.0+. Please refer to [Installing Emacs](http://wikemacs.org/index.php/Installing_Emacs).

# Quick Start

## Installation

Backup `.emacs.d` if need,

``` shell
mv ~/.emacs.d ~/.emacs.d.bak
```

then

``` shell
git clone https://github.com/RyougiNevermore/.emacs.d.git ~/.emacs.d
```

or download the [zip
package](https://codeload.github.com/RyougiNevermore/.emacs.d/zip/master) directly and
extract to `~./emacs.d`.

Then start emacs. Wait for a while to install packages at the first startup.
Oh Yeah!!! Enjoy it!!!

# Customization

Edit`user-init.el` and change the configurations, then
restart Emacs.

