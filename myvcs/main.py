#!/usr/bin/env python
import os
from os.path import exists, isdir, join
import shutil
import sys

META_DIR = '.myvcs'

def backup(path):
    backup_dir = join(path, META_DIR)
    if exists(backup_dir):
        shutil.rmtree(backup_dir)

    def is_backup_dir(src, names):
        if src == path:
            return [META_DIR]
        return []

    shutil.copytree(path, backup_dir, ignore=is_backup_dir)

def get_next_backup_dir(backup_dir, versions):
    return join(backup_dir, str(len(versions) + 1))

def get_versions():
    backup_dir = join(path, META_DIR)
    if exists(backup_dir):
        versions = [
            name for name in os.listdir(join(path, META_DIR))
            if name != 'stash'
        ]
    else:
        versions = []
    return versions

def snapshot(path, version=None):
    versions = get_versions()

    def is_backup_dir(src, names):
        if src == path:
            return [META_DIR]
        return []

    if version == None:
        backup_dir = get_next_backup_dir(join(path, META_DIR), versions)

    elif version == 'stash':
        backup_dir = join(path, META_DIR, version)
        if exists(backup_dir):
            shutil.rmtree(backup_dir)

    else:
        print 'Unknown version'

    shutil.copytree(path, backup_dir, ignore=is_backup_dir)
    # FIXME: version 21?


def checkout_version(path, version):
    snapshot_dir = join(path, META_DIR, version)

    for name in os.listdir(path):
        if name == META_DIR:
            continue
        full_path = join(path, name)
        if isdir(full_path):
            shutil.rmtree(full_path)
        else:
            os.remove(full_path)

    for name in os.listdir(snapshot_dir):
        full_path = join(snapshot_dir, name)
        if isdir(full_path):
            shutil.copytree(full_path, join(path, name))
        else:
            shutil.copy(full_path, join(path, name))

def checkout(path, version):
    versions = get_versions()
    if version not in versions:
        print 'No such version'
    else:
        snapshot(path, version='stash')
        checkout_version(path, version)


if __name__ == '__main__':
    # FIXME: This is a bad idea.  It should look for a .myvcs somewhere up in
    # the tree, or current directory.
    path = os.getcwd()
    assert len(sys.argv) > 1, 'Need a command to run'

    command = sys.argv[1]
    if command == 'commit':
        snapshot(path)

    elif command == 'checkout':
        checkout(path, sys.argv[2])

    else:
        print 'Unknown command!'
        sys.exit(1)
