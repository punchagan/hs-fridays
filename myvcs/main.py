#!/usr/bin/env python
import os
from os.path import exists, join

import shutil

def backup(path):
    backup_dir = join(path, '.myvcs')
    if exists(backup_dir):
        shutil.rmtree(backup_dir)

    def is_backup_dir(src, names):
        if src == path:
            return ['.myvcs']
        return []

    shutil.copytree(path, backup_dir, ignore=is_backup_dir)

def get_next_backup_dir(backup_dir, versions):
    return join(backup_dir, str(len(versions) + 1))

def snapshot(path):
    backup_dir = join(path, '.myvcs')
    if exists(backup_dir):
        versions = os.listdir(backup_dir)
    else:
        versions = []

    def is_backup_dir(src, names):
        if src == path:
            return ['.myvcs']
        return []

    backup_dir = get_next_backup_dir(backup_dir, versions)
    shutil.copytree(path, backup_dir, ignore=is_backup_dir)


if __name__ == '__main__':
    path = os.getcwd()
    snapshot(path)
