# /// script
# requires-python = ">=3.4"
# dependencies = [
#     "i3ipc",
# ]
# ///

# @name i3-smart-titlebar.py
# @description Disable i3WM titlebar when the current workspace at any moment has only one
# window. Enable it again if there are more.
# @author myxi@envs.net
# @license MIT

from i3ipc import Connection, Event

i3 = Connection()
i3.ws_focus = None


def on_workspace_focus(self, e):
    self.ws_focus = e.current


def on_window_focus(self, e):
    if not i3.ws_focus:
        return
    amnt = len(self.ws_focus.leaves())
    if amnt == 1:
        i3.command("border none")
    else:
        i3.command("border normal")


i3.on(Event.WORKSPACE_FOCUS, on_workspace_focus)
i3.on(Event.WINDOW_FOCUS, on_window_focus)

i3.main()
