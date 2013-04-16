#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice


# Start Pulse Audio
/esu/bin/start-pulseaudio-x11

# Hide Cursor
hdd &

# Set the cursor
xsetrroot -cursor_name left_ptr

# Set Background
DISPLAY=:0.0 feh --bg-scale "$(find ~/Pictures/Wallpaper/16x10/|shuf -n1)"

# Start parcellite
parcellite -d &

# Source .Xresources
xrdb -merge ~/.Xresources

# Start Xmonad
xmonad
