## Theme Not Applying Everywhere
There are two steps involved, you likely did only the first step.

- Select the theme on the _Appearance_ settings.
- Select the theme on the _Window Manager_ settings.

**TIP**: Do not forget to set the icons too from the _Apperance_ settings.

## Theme Not Applying on Login Screen
See [this](https://askubuntu.com/a/1505899) answer on Stack Exchange.

### Resolving Issues with File Permissions
You are likely to have issues with file permissions somewhere in the process. I
resolve them by manually copying the relevant files to these locations:

- `/usr/share/themes`
- `/usr/share/icons`
- `/usr/share/wallpapers`
- `/usr/share/lightdm/avatars`

**NOTE**: The last directory does not exist by default so you will have to manually make it.

**EXAMPLE**: `sudo cp ~/.themes/gruvbox-theme/ /usr/share/themes/gruvbox-theme/ -r`

## Sticky Keys
You might get nagging notifications about Stick Keys getting enabled. This is
how I resolve them:

- Open _Accessibility_ settings then turn on Stick Keys manually, then turn it
  off.
- Uncheck `xfce4-settings-helper` from _Notifications_ settings' _Applications_
  section.

## Using GTK Theme for Qt Apps
**SELF NOTE**: Don't. Not worth it since there are quirks here and there.

- Install the dependencies:
    ```bash
    sudo apt install qt5ct qt5ct-style-plugins
    ```
- Setup your `.profile` (`.zprofile` if using Zsh):
    ```bash
    export QT_QPA_PLATFORMTHEME=qt5ct
    ```
- Log out and log back.
- Open `qt5ct` then select `gtk2` theme with default colorscheme.
- Apply.

## Log out Menu Not Following GTK Theme Under i3 WM
I don't know what exactly the cause is but the fix is tweaking your session startup apps.

![](https://files.catbox.moe/fwkp7h.png)

Configure your startup session as shown and then login. It should work.

**NOTE**: You might also need to switch to "Xfce" from "Xubuntu" in the login manager before doing this.

## Exit TTY
Press `CTRL + ALT + F7`.
