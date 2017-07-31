# .emacs.d #

## Beware the unorthodox keybindings ##
I got the point of vim people that overusing the modifier keys are incovinient, however switching between insert and commad mode is annoying, because most of the time I want to execute only one command... yes event for navigation (see [avy-mode](https://github.com/abo-abo/avy)).
So what if hit CapsLock and enter a easily memorizable keysequence without holding down the CapsLock or any of the modifier keys?
I tried it and worked pretty well.
Well I had to remap CapsLock to an otherwise expendable key: insert. (Seriously what is the real world use-case of that key, making ASCII art...)

For this you should have an Xmodmap similar to this:

`
clear lock
clear control
keycode 66 = Insert
keycode 118 = Caps_Lock
add control = Control_L Control_R
`

## Mnemonics ##
b for buffers
p for projects
i for insertion
l for line
m for manuals and docs
v for version control (magit)
s for search
x for I/O
ESC for exit

return for newline commands

easter egg: j for major mode bindings, because it is so easy to hit after control.

## Milestones: ##
- [ ] indium js IDE
- [x] unit-testing
- [x] helm everywhere
- [ ] add toggle-truncate-lines to org-mode-hook
- [ ] implement js-import
- [ ] helm-package
- [ ] helm-dash
- [ ] helm-mt
- [ ] helm-cider
- [ ] helm-pydoc
- [ ] helm-clojuredocs
- [ ] helm-commandlinefu
- [ ] Implement use-package to automaticly install packages and decrease load time
- [ ] org mode .docx export
- [ ] paredit-mode
- [ ] Effective Blender-mode usage
- [ ] npm-mode
- [ ] helm-sql


## Discussion ##

- Do I need perspectives?
- Use atomic-chrome package to edit text inside chrome with Emacs
- multitran for in-Emacs translation
- hledger-mode for accounting (Do I have time for this?)


## Issues ##

- [ ] linum mode makes org-mode extremly slow
	- Is it true for other modes? (Possibly the ones with folding)
	- Diable it on larger files
- [x] first helm isearch fails
