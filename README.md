Frédéric Jolliton's dotfiles
============================

Disclaimer
----------

Theses configuration files are provided as is. The aim of publishing
them publicly is to let you get an idea of how I manage them. I do not
plan to provide any support for them, nor accept any pull request.

Installation
------------

First make sure that no existing configuration file with important
content will be overwritten.

If you want the full configuration, install with:

    mkdir -p ~/.local/bin
    stow -t ~ ./*

You can also install a specific part with `stow -t ~ xmonad` for example.

Per-Host Settings
-----------------

For Xorg, put specific settings in `~/.Xdefaults-<hostname>` (such as
`~/.Xdefaults-pluto`). Note: nowadays, it is recommended to use
`~/.Xresources`, but I like the convenience of the settings being
always reloaded without an explicit command. I don't think it causes
performance issues.

Screenshots
-----------

Minimalistic desktop when starting Xorg:

![XOrg Preview](https://github.com/fjolliton/dotfiles/raw/master/.extra/preview.png)
