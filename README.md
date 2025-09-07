# Personal Dotfiles
My dotfiles are based on Luke Smith's dotfiles. I like how he manages dotfiles, so I borrowed some ideas. However, there are many things I don’t fully understand or that don’t work well for me and lot of things are WIP.

I organize common configurations across different programs into a few shared files instead of hardcoding the same settings in multiple places. For example:

- Defining common software I use like `$TERMINAL`, `$BROWSER`, etc. in `~/.config/shell/profile` for my keybindings for multiples WMs.
- All common shared shell setting from `.bashrc` and `.zshrc` into separate `profile` and `aliasrc` files to keep things consistent.
- Keeping `$HOME` directory clean by following the [XDG (freedesktop)](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html) / [XDG (Arch Wiki)](https://wiki.archlinux.org/title/XDG_Base_Directory), moving everything that supports it into `.config`
- Using symbolic links to redirect config files from their default locations to `.config`

I think this way of organization is a little more modular and easy to maintain and add features.

## File Structure
- User script are located in `~/.local/bin`
- Default programs are located in `~/.local/share/applications`
- Docs about some scripts and other are located in `dotfiles docs`.

# Packages Needed

## Common base for x11 and wayland
```
zsh pcmanfm neovim emacs kitty firefox mpv nsxiv-git lf eza zoxide
```

## X11
```
dmenu slock lxsession
```

### BSPWM
```
bspwm sxhkd polybar
```

### I3
```
i3-wm i3status xdotool
```

## Wayland
```
swaylock wmenu-run
```

### Hyprland
```
hyprland xdg-desktop-portal-hyprland waybar socat jq
```

### Sway
```
sway
```

## Small Rice
```
papirus-icon-theme materia-gtk-theme adwaita-qt5-git qt5ct qt6ct qt5-wayland qt6-wayland nwg-look ttf-aporetic
```
