# Personal Dotfiles

My dotfiles are based on Luke Smith's dotfiles. I like how he manages dotfiles, so I borrowed some ideas. However, there are many things I don’t fully understand or that don’t work well for me and lot of things are WIP.

I organize common configurations across different programs into a few shared files instead of hardcoding the same settings in multiple places. For example:

- Defining common software I use like `$TERMINAL`, `$BROWSER`, etc. in `~/.config/shell/profile` for my keybindings for multiples WMs.
- All common shared shell setting from `.bashrc` and `.zshrc` into separate `profile` and `aliasrc` files to keep things consistent.
- Keeping `$HOME` directory clean by following the [XDG (freedesktop)](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html) / [XDG (Arch Wiki)](https://wiki.archlinux.org/title/XDG_Base_Directory), moving everything that supports it into `.config`
- Using symbolic links to redirect config files from their default locations to `.config`
- Docs about some scripts and other are located in `dotfiles docs`.

I think this way of organization is a little more modular and easy to maintain and add features.

# Packages Needed

## Common base for x11 and wayland

```sh
zsh pcmanfm vis emacs kitty firefox mpv nsxiv-git lf trash-cli chafa ctpv-git eza zoxide wmname nitrogen ly reflector pacman-contrib xclip wl-clipboard xdg-utils
```

Extra:

```sh
gnome-disk-utility bleachbit zathura zathura-cb zathura-djvu zathura-pdf-mupdf font-manager tldr btop hblock bc scrcpy sshfs cpupower corefreq powertop turbostat bmon tmon s-tui
```
For VMs:

```sh
qemu-base libvirt virt-manager virt-viewer edk2-ovmf dnsmasq bridge-utils nftables openbsd-netcat spice-vdagent swtpm gnome-boxes
```

## X11

```sh
dmenu slock lxqt-policykit
```

### BSPWM

```sh
bspwm sxhkd polybar
```

### I3

```sh
i3-wm i3status xdotool
```

## Wayland

```sh
swaylock wmenu xdg-desktop-portal-wlr
```

### Hyprland

```sh
hyprland xdg-desktop-portal-hyprland waybar socat jq
```

### Sway

```sh
sway jq
```

## Small Rice

```sh
papirus-icon-theme materia-gtk-theme adwaita-qt5-git qt5ct qt6ct qt5-wayland qt6-wayland nwg-look ttf-aporetic
```

# Install Dotfiles

From archinstall ISO media, change root using user account:

```sh
chroot-arch /mnt su -- user
```

Or from a booted OS after install on home (~/):

```sh
rm -rf * .*
sudo pacman -Syu git rsync base-devel --needed
git clone https://github.com/sarajevo1914/dotfiles
rsync -av /home/user/dotfiles/ /home/user/
```

If you want to exclude some git-related files:

```sh
rsync -aHv --exclude ".git" --exclude "README*" /home/user/dotfiles/ /home/user/
```

If needed, change all file ownership from root to user.

```sh
chown -R user:user /home/user
```

To use Ly DM:

```sh
systemctl disable getty@tty2.service
systemctl enable ly@tty2.service
```

To start bspwm without any DM, edit `~/.xinitrc`:

```sh
exec bspwm
```

Then run:

```sh
startx
```
