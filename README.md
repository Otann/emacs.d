# this is my `emacs` config

If you're new to emacs, check out
[this introductory tutorial](http://www.braveclojure.com/basic-emacs/)!

This configuration is heavily inpired by [layron's](https://github.com/lunaryorn/.emacs.d)

## Personal Sruff

This implementation stores all customizations that are done from
ui in `custom.el` file. For instance `waka-time` requires personal key to start
which should be set up like this.

## Organization

I've tried to separate everything logically and document the purpose
of every line. [`init.el`](./init.el) acts as a kind of table of
contents.

All customizations are broke into following modules (which are packages that are located in `molules` folder and loaded through `use-package`):

- `otann-environment-fixup` how emacs interacts with it's host
  + `exec-path-from-shell`
  + `ns-win`
  + MacOs keybindings
  + `saveplace`
  + `osx-trash`

- `otann-look-and-feel` all looks and colors
  + default faces
  + toolbars, menubars, navbars, etc...
  + `solarized` color theme
  + `hl-line`
  + `which-key`
  + `uniquify`

- `otann-modeline`
  + mode line configuration

- `otann-navigation`
  + `projectile`
  + `ibuffer-projectile`
  + `window-numbering`
  + `neotree`
  + `helm`
  + `helm-files`
  + `helm-misc`
  + `helm-command`
  + `helm-projectile`
  + configuration for buffer's windows'

- `otann-ide` all common stuff related to development
  + hook to `whitespace-cleanup`
  + `smartparens`
  + `flycheck`
  + `wakatime`
  + `subword`
  + `hideshowvis`
  + `company`
  + `company-quickhelp`
  + `company-statistics`
  + `company-statistics`
  + `company-math`
  + `helm-company`
  + `diff-hl`
  + `gitconfig-mode`
  + `gitignore-mode`
  + `git-timemachine`
  + `web-mode`
  + `js2-mode`
  + `js2-refactor`
  + `json-mode`
  + `json-reformant`
  + `markdown-mode`
  + `sh-script`
  + `yaml-mode`
  + `clojure-mode`
  + `cider`

- TODO
  - whitespace-cleanup-mode
  - undo-tree
  - tagedit
  - typo
