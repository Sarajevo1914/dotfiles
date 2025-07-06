# Theme Switcher
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
