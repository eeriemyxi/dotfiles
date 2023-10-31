"""
Title: Arrow Key Mode
Description: Temporarily set alternatives to arrow keys.
Author: myxi (eeriemyxi @ GitHub), 2023
Dependencies: Python 3.10 and `pynput` module.
License: MIT
"""

from pynput.keyboard import Controller, Listener, Key


SUPPRESS = False
AL_SET = ("r", "f", "s", "t") + ("n", "u", "e", "i")
RE_SET = ("left", "up", "down", "right")
ASSIGNED_CHARS = {}

for i, char in enumerate(AL_SET):
    ASSIGNED_CHARS[char] = RE_SET[i % 4]

CTRL_CHARS = dict(list(ASSIGNED_CHARS.items())[:4])

controller = Controller()


def handle_backspace():
    if not SUPPRESS:
        controller.tap(Key.backspace)


def on_release(key):
    try:
        if key.char == "c":
            handle_backspace()
            exit(0)

        key_obj = getattr(Key, ASSIGNED_CHARS[key.char])

        handle_backspace()

        try:
            CTRL_CHARS[key.char]
        except KeyError:
            pass
        else:
            with controller.pressed(Key.ctrl):
                controller.tap(key_obj)
                return

        controller.tap(key_obj)
        return

    except (AttributeError, KeyError):
        return


with Listener(on_release=on_release, suppress=SUPPRESS) as listener:
    listener.join()
