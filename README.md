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

# Pkg needed

## Common base for x11 and wayland

```
zsh ly pcmanfm pcmanfm-qt sublime-text-4 neovim helix kitty firefox brave-bin nomacs qt5-imageformats imagemagick mpv imv vlc nsxiv-git lf pistol chroma archiver ctpv stpv yazi eza zoxide tldr htop btop
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
papirus-icon-theme epapirus-icon-theme materia-gtk-theme adwaita-qt5-git ttf-jetbrains-mono-nerd ttf-font-awesome qt5ct qt6ct qt5-wayland qt6-wayland lxappearance fontconfig gsettings-desktop-schemas
```

### Theme Switcher
> WIP
> Global Cursor theme might not work

Small script for change between dark and light theme that works for X11 and Waylan session.

The first time you run will create the config file and exit, telling you to edit `~/.config/theme_switcher.conf` and after that run again
The default config file have my settings i still use since i start using linux (Materia theme, Papirus icon, etc.) you can use my setting or change for your preferences

**Syntax**
`theme_switcher -v dark`
`-v` for verbose option
`dark` `light` for switch to dark of light theme

**Required Package for Arch Linux**

```
xorg-xrdb qt5ct qt6ct qt5-base qt6-base fontconfig gsettings-desktop-schemas
````