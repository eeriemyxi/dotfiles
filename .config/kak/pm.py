#!/bin/python3

import pathlib
import subprocess
import sys

SCRIPT_DIR = pathlib.Path(__file__).parent
PACKAGES: list[list[str, str]] = [
    ["https://github.com/danr/kakoune-easymotion", "easymotion"],
    ["https://github.com/enricozb/tabs.kak", "tabs"],
    ["https://github.com/lePerdu/kakboard", "kakboard"],
    ["https://github.com/Bodhizafa/kak-rainbow", "rainbow-parens"],
    ["https://github.com/h-youhei/kakoune-surround", "surrounder"]
]
PRE_INSTALL = {"tabs": [["cargo", "install", "kak-tabs"]]}
POST_INSTALL = {}
AUTOLOAD_DIR = SCRIPT_DIR / "autoload"

def eprint(*args, **kwargs):
    print(*args, **kwargs, file=sys.stderr)


def git_clone(url: str, name: str, depth: int = 1) -> int:
    res = subprocess.run(["git", "clone", "--depth", str(depth), url, name])
    return res.returncode


def git_pull(dir_name: str) -> int:
    res = subprocess.run(["git", "pull"], cwd=AUTOLOAD_DIR / dir_name)
    return res.returncode


def install_package(package_url: str, name: str) -> None:
    print(f"Installing package {name} from {package_url}")
    return git_clone(package_url, str(AUTOLOAD_DIR / name))


def update_package(name: str) -> None:
    print(f"Updating package {name}")
    return git_pull(str(AUTOLOAD_DIR / name))


def match_package_or_exit(name):
    for package in PACKAGES:
        if name == "all":
            yield package
        elif package[1] == name:
            yield package
            return
    if name != "all":
        eprint(f"ERROR: Package {name!r} does not exist.")
        eprint("Available options:")
        eprint("".join(f"{' ' * 4}- {name}\n" for _, name in PACKAGES))
        exit(1)


def run_shell(index, cmd, *args, **kwargs):
    print(f"({index}) [RUNNING] {cmd!r}")
    res = subprocess.run(cmd, *args, **kwargs)
    print(f"({index}) [FINISHED] {cmd!r}")
    print(f"({index}) [EXIT CODE] {res.returncode}")


def main() -> None:
    args = sys.argv[1:]
    if len(args) == 0:
        print("Subcommands: install, update")
        exit(1)

    if args[0] == "install":
        for package_url, name in match_package_or_exit(args[1]):
            install_ec = install_package(package_url, name)
            if install_ec == 128:
                eprint(f"INFO: Package {name} is already installed. ")
                eprint("    tip: try 'update' subcommand")
            print(
                "Install was",
                ("successful" if install_ec == 0 else "unsuccessful") + ".",
            )
            if name in PRE_INSTALL:
                print("INFO: Running pre-install commands")
                for index, cmd in enumerate(PRE_INSTALL[name], 1):
                    run_shell(index, cmd)
            if name in POST_INSTALL:
                print("INFO: Running pre-install commands")
                for index, cmd in enumerate(POST_INSTALL[name], 1):
                    run_shell(index, cmd)

    if args[0] == "update":
        for _, name in match_package_or_exit(args[1]):
            update_ec = update_package(name)
            print(
                "Update was", ("successful" if update_ec == 0 else "unsuccessful") + "."
            )


if __name__ == "__main__":
    main()
