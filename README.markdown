## Cheatsheet.el

Cheatsheet.el is a tool for creating your own Emacs cheatsheet.
Why I've created this plugin:
* I want to start using new plugin without learning keys - cheatsheet.el lets you to defne keys you want to be able to find quickly
* I don't need to see all keys, defined in different keymaps
* I don't need to see all keys, defined in any plugin
* I want to write my own key description
* I want to see commands near the keys in my cheatsheet

All this problems can be solved using cheatsheet.el

## Getting started
* Get cheatsheet.el
  * Hopefully, will be available via MELPA
  * Manually download cheatsheet.el and set-up your load path.
    [Find out more.](http://www.emacswiki.org/emacs/InstallingPackages)
* Load package - (require 'cheatsheet)
* Add your first cheat:
```
(cheatsheet-add :group 'Common
                :key "C-x C-c"
                :description "leave Emacs.")
```
* Run `(cheatsheet-show)` and enjoy :-)

## Plugin API
# cheatsheet-add
Command to add a new cheat to your cheatsheet
```
(cheatsheet-add :group 'Common
                :key "C-x C-c"
                :description "leave Emacs.")
```

# cheatsheet-get
Command to get current cheatsheet as list of groups, keeping defining order.
* Cheat is a plist that looks like this `[:group :key :description]`. `:group`, `:key`, `:description` are symbols or strings
* Group is a plist that looks like this `[:name :cheats]`. `:name` is a symbol or string, `:cheats` is a list of CHEATs
* Cheatsheet is a list of GROUPs - result of `cheatsheet-get` command

# cheatsheet-show
Show buffer with your cheatsheet. Can be closed via `C-q` key.

Enjoy using cheatsheet.el