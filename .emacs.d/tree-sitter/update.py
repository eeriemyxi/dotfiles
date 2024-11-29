#!/bin/python3

import tempfile
import shutil
import tarfile
import sys
import json
import pathlib
import urllib.request


def get_latest_tag(author, repo):
    resp = urllib.request.urlopen(f"https://api.github.com/repos/{author}/{repo}/tags")
    assert resp.status == 200, f"Repository {author}/{repo} probably doesn't exist"
    resp_json = json.loads(resp.read())
    return resp_json[0]["name"]


def parse_tag(tag):
    return tag.split("/")[1].lstrip("v")


def parse_platform(platform):
    pl = platform
    match platform:
        case "linux":
            pl = "linux"
        case "win32":
            pl = "windows"
        case "darwin":
            pl = "macos"
    return pl


def download_libs(author, repo, tag, platform, outdir):
    flname = f"{outdir}/release.tar.gz"
    with open(flname, "wb") as file:
        url = f"https://github.com/{author}/{repo}/releases/download/{tag}/tree-sitter-grammars-{platform}-{tag}.tar.gz"
        resp = urllib.request.urlopen(url)
        assert resp.status == 200
        file.write(resp.read())

    return flname


def main() -> None:
    if len(sys.argv) == 1:
        print(f"Usage: {sys.argv[0]} <where to extract>", file=sys.stderr)
        exit(1)

    location = pathlib.Path(sys.argv[1]).resolve()
    location.mkdir(exist_ok=True)
    tag = get_latest_tag("emacs-tree-sitter", "tree-sitter-langs")
    parsed_tag = parse_tag(tag)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir = pathlib.Path(tempdir)
        libs_dir = tempdir / "libs"
        tar_flname = download_libs(
            "emacs-tree-sitter",
            "tree-sitter-langs",
            parsed_tag,
            parse_platform(sys.platform),
            tempdir,
        )

        with tarfile.open(tar_flname) as tar:
            tar.extractall(path=libs_dir, filter="data")

        for file in libs_dir.iterdir():
            if not file.suffix:
                continue
            filename = f"libtree-sitter-{file.name}"
            dest = location / filename
            shutil.copy(file, dest)
            print(f"{file} -> {dest}")


if __name__ == "__main__":
    main()
