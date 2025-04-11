# /// script
# requires-python = ">=3.12"
# dependencies = [
#     "kisesi",
# ]
# ///
import subprocess
import kisesi
import pathlib

kisesi.basic_config(level=kisesi.DEBUG)
log = kisesi.get_logger(__name__)

REQUIRED_PROGRAMS = ("git", "stow")
DOTFILES_REPO = "https://github.com/eeriemyxi/dotfiles"
DOTFILES_PATH = pathlib.Path.home() / ".dotfiles/"


def autolog(func):
    from functools import wraps

    log = kisesi.get_logger("autologger")
    name = func.__name__

    @wraps(func)
    def wrapper(*args, **kwargs):
        log.debug(
            f"Call {name}() with {args=} {kisesi.Color.BRIGHT_RED}AND{kisesi.Color.RESET} {kwargs=}"
        )
        result = func(*args, **kwargs)
        log.debug(f"{name}() returned {result=}")
        return result

    return wrapper


@autolog
def is_executable(name: str | tuple) -> bool:
    from shutil import which

    return bool(which(name))


@autolog
def ensure_calls(func, data, log_msg):
    callbacks = []

    for name, args in data:
        cb = func(*args)
        callbacks.append(cb)
        if not cb:
            log.error(log_msg % dict(name=name))

    return all(callbacks)


@autolog
def git_clone(repo_link: str, at: str):
    out = subprocess.run(["git", "clone", repo_link, at], capture_output=True)
    return (out.returncode, result.stdout, result.stderr)


def main() -> None:
    # if DOTFILES_PATH.exists():
    #     log.error("Path to extract the dotfiles at already exists: %s", str(DOTFILES_PATH))
    #     return

    if not ensure_calls(
        is_executable,
        ((name, [name]) for name in REQUIRED_PROGRAMS),
        "Executable '%(name)s' is not in your PATH environment variable.",
    ):
        log.warning(
            "Exiting because you don't have all the necessary executables in your PATH."
        )
        return

    # log.info("
    # git_clone


if __name__ == "__main__":
    main()
