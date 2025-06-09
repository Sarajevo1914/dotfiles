# Personal Dotfiles

My dotfiles are based on Luke Smith's dotfiles. I like how he manages dotfiles, so I borrowed some ideas. However, there are many things I don’t fully understand or that don’t work well for me and lot of things are WIP.

I organize common configurations across different programs into a few shared files instead of hardcoding the same settings in multiple places. For example:

- Defining common settings like `$TERMINAL=kitty` `$FILEMANAGER=pcmanfm` in `~/.config/shell/profile` and using for my keybindings for BSPWM, DWM, Hyprland, etc. **TODO**
- All common shared shell setting from `.bashrc` and `.zshrc` into separate `profile` and `aliasrc` files to keep things consistent.
- Keeping `$HOME` directory clean by following the [XDG (freedesktop)](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html) / [XDG (Arch Wiki)](https://wiki.archlinux.org/title/XDG_Base_Directory), moving everything that supports it into `.config`
- Using symbolic links to redirect config files from their default locations to `.config`

I think this way of organization is a little more modular and easy to maintain and add features.

## File Struct

- User script are located in `~/.local/bin`
- Default programs are located in `~/.local/share/applications` **TODO**
- Both `bash` and `zsh` shell load other config files like `aliasrc` `profile` sharing very similar setting
- Personal docs are located in `~/docs`

# Pkg needed

## Common base for x11 and wayland

```
zsh ly pcmanfm pcmanfm-qt sublime-text-4 neovim emacs kitty firefox brave-bin mpv vlc nsxiv-git lf archiver yazi eza zoxide tealdeer htop btop
```

## X11

```
bspwm sxhkd polybar dmenu slock lxsession
```

## Wayland

```
hyprland xdg-desktop-portal-hyprland wofi waybar swaylock
```

## Small Rice

```
papirus-icon-theme materia-gtk-theme adwaita-qt5-git qt5ct qt6ct qt5-wayland qt6-wayland lxappearance
```
