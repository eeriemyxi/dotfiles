"""Opionated automated compilation of Emacs using Python
   CAUTION: Do not run it with Python optimization flags.
"""

import pathlib
import subprocess
import uuid
import pprint
import sys
import urllib.request
import tarfile
import shutil
import logging

TEMP_COMP_PATH = pathlib.Path(f"/tmp/emacs-{uuid.uuid4()}")
EMACS_VERSION = "21.4a"
EMACS_COMP_FLAGS = ""
DL_MIRROR = "https://mirrors.hopbox.net/gnu/emacs"
DL_ZIP_FORMAT = ".tar.gz"
EMACS_ZIP_FILENAME = pathlib.Path(f"emacs-{EMACS_VERSION}{DL_ZIP_FORMAT}")
EMACS_DL_URL = f"{DL_MIRROR}/{EMACS_ZIP_FILENAME}"
EMACS_SV_LOC = TEMP_COMP_PATH / EMACS_ZIP_FILENAME
EMACS_DIR = TEMP_COMP_PATH / f"emacs-{EMACS_VERSION.strip('a')}"

logging.basicConfig(
    level=logging.DEBUG,
    format='%(levelname)s: [%(asctime)s] %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)
log = logging.getLogger(__file__)

assert not TEMP_COMP_PATH.exists(), "Temp directory already exists (somehow)."
TEMP_COMP_PATH.mkdir()

log.debug(f"{TEMP_COMP_PATH=}")
log.debug(f"{EMACS_COMP_FLAGS=}")
log.debug(f"{EMACS_VERSION=}")
log.debug(f"{DL_MIRROR=}")
log.debug(f"{DL_ZIP_FORMAT=}")
log.debug(f"{EMACS_ZIP_FILENAME=}")
log.debug(f"{EMACS_DL_URL=}")
log.debug(f"{EMACS_SV_LOC=}")
log.debug(f"{EMACS_DIR=}")


def print_usage(file) -> None:
    print(f"Usage: {sys.argv[0]} <output_dir>", file=file)


def _get_dir_tree(path):
    return pprint.pformat(list(path.iterdir()))


def main() -> None:
    if len(sys.argv) < 1:
        print("ERROR: not enough arguments")
        print_usage(sys.stderr)
        exit(1)

    log.info("Downloading tar file from %s at %s", EMACS_DL_URL, EMACS_SV_LOC)
    with urllib.request.urlopen(EMACS_DL_URL) as resp, open(EMACS_SV_LOC, "wb") as file:
        log.debug(f"{resp.status=}")
        assert resp.status == 200
        file.write(resp.read())

    assert tarfile.is_tarfile(EMACS_SV_LOC)

    log.info("Extracting %s", EMACS_SV_LOC)

    with tarfile.open(EMACS_SV_LOC, "r:gz") as trfile:
        trfile.extractall(path=TEMP_COMP_PATH)
    assert EMACS_DIR.exists(), "Invalid zip file. Recheck your config."

    log.debug("Tree of %s: %s", TEMP_COMP_PATH, _get_dir_tree(TEMP_COMP_PATH))


def cleanup() -> None:
    log.info("Beginning clean-up process")
    log.debug("Tree of %s: %s", TEMP_COMP_PATH, _get_dir_tree(TEMP_COMP_PATH))
    log.info("Cleaning %s", TEMP_COMP_PATH)
    shutil.rmtree(TEMP_COMP_PATH)
    log.info("... Cleanup complete")


if __name__ == "__main__":
    EXIT_CODE = 0
    try:
        main()
    except Exception as err:
        log.critical("Can't continue due to errors. Exiting.")
        log.error(err, exc_info=True)
        EXIT_CODE = 1
    finally:
        cleanup()
        log.info("Exiting")
        exit(EXIT_CODE)
