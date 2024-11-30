#!/bin/python3

import json
import pathlib
import shutil
import sys
import tarfile
import tempfile
import urllib.request


def get_latest_tag(author: str, repo: str) -> str:
    resp = urllib.request.urlopen(f"https://api.github.com/repos/{author}/{repo}/tags")
    assert resp.status == 200, f"Repository {author}/{repo} probably doesn't exist"
    resp_json = json.loads(resp.read())
    return resp_json[0]["name"]


def parse_tag(tag: str) -> str:
    return tag.split("/")[1].lstrip("v")


def trans_platform(platform: str) -> str:
    match platform:
        case "linux":
            return "linux"
        case "win32":
            return "windows"
        case "darwin":
            return "macos"


def download_libs(
    author: str, repo: str, tag: str, platform: str, outdir: pathlib.Path
):
    flname = outdir / "release.tar.gz"
    with open(flname, "wb") as file:
        url = (
            f"https://github.com/{author}/{repo}/releases/download/{tag}/"
            f"tree-sitter-grammars-{platform}-{tag}.tar.gz"
        )
        resp = urllib.request.urlopen(url)
        assert (
            resp.status == 200
        ), "tag probably doesn't exist, for various reasons, IDC."
        file.write(resp.read())

    return flname


def main() -> None:
    if (
        len(sys.argv) == 1
        or len(sys.argv) > 2
        or sys.argv[1].startswith(("--help", "-h", "help"))
    ):
        print(f"Usage: {sys.argv[0]} <dest_dir>", file=sys.stderr)
        print(f"Note: <dest_dir> will be created if it doesn't exist.", file=sys.stderr)
        exit(1)

    dest_dir = pathlib.Path(sys.argv[1]).resolve()
    dest_dir.mkdir(exist_ok=True)
    tag = get_latest_tag("emacs-tree-sitter", "tree-sitter-langs")
    parsed_tag = parse_tag(tag)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir = pathlib.Path(tempdir)
        libs_dir = tempdir / "libs"
        tar_flname = download_libs(
            "emacs-tree-sitter",
            "tree-sitter-langs",
            parsed_tag,
            trans_platform(sys.platform),
            tempdir,
        )

        with tarfile.open(tar_flname) as tar:
            tar.extractall(path=libs_dir, filter="data")

        for file in libs_dir.iterdir():
            if not file.suffix:
                continue
            filename = f"libtree-sitter-{file.name}"
            dest = dest_dir / filename
            shutil.copy(file, dest)
            print(f"{file} -> {dest}")


if __name__ == "__main__":
    main()
