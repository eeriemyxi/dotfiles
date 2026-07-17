#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.13"
# dependencies = [
#     "platformdirs>=4.10.0",
# ]
# ///

import re
import platformdirs
import json
import subprocess

NIRI_CONFIG_DIR = platformdirs.user_config_path("niri")
NIRI_CONFIG_FILE = NIRI_CONFIG_DIR / "config.kdl"
WS_CONFIG_FILE_PREFIX = "workspace-switch-anim-"
WS_CONFIG_OFF = NIRI_CONFIG_DIR / (WS_CONFIG_FILE_PREFIX + "off.kdl")
WS_CONFIG_ON = NIRI_CONFIG_DIR / (WS_CONFIG_FILE_PREFIX + "on.kdl")

assert WS_CONFIG_OFF.exists()
assert WS_CONFIG_ON.exists()
assert NIRI_CONFIG_FILE.exists()

def stream_niri_events():
    process = subprocess.Popen(
        ["niri", "msg", "--json", "event-stream"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    print("Listening for niri events... Press Ctrl+C to stop.")
    try:
        for line in process.stdout:
            event_data = json.loads(line.strip())
            key, val = next(iter(event_data.items()))
            if not key == "OverviewOpenedOrClosed":
                continue
            config_text = NIRI_CONFIG_FILE.read_text()
            active = (WS_CONFIG_OFF.name, WS_CONFIG_ON.name)[val["is_open"]]
            config_text = re.sub(r'(include ").+(")', f"\\1{active}\\2", config_text)
            NIRI_CONFIG_FILE.write_text(config_text)
            subprocess.run(["niri", "msg", "action", "load-config-file"])
    except KeyboardInterrupt:
        print("\nStopping event stream listener.")
    finally:
        process.terminate()

if __name__ == "__main__":
    stream_niri_events()
