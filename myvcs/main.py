#!/usr/bin/env python
import os
from os.path import basename, exists, isdir, join
import shutil
import string
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

def get_versions(path):
    backup_dir = join(path, META_DIR)
    if exists(backup_dir):
        versions = [
            name for name in os.listdir(join(path, META_DIR))
            if name.startswith(tuple(string.digits))
        ]
    else:
        versions = []
    return versions

def stash_exists(path):
    return exists(join(path, META_DIR, 'stash'))

def commit(path, version=None):
    versions = get_versions(path)

    def is_backup_dir(src, names):
        if src == path:
            return [META_DIR]
        return []

    if version == None:
        backup_dir = get_next_backup_dir(join(path, META_DIR), versions)
        print versions
        track_version(path, basename(backup_dir))

    elif version == 'stash':
        backup_dir = join(path, META_DIR, version)
        if exists(backup_dir):
            shutil.rmtree(backup_dir)

    else:
        print 'Unknown version'

    shutil.copytree(path, backup_dir, ignore=is_backup_dir)
    # FIXME: version 21?


def checkout_version(path, version):
    commit_dir = join(path, META_DIR, version)

    for name in os.listdir(path):
        if name == META_DIR:
            continue
        full_path = join(path, name)
        if isdir(full_path):
            shutil.rmtree(full_path)
        else:
            os.remove(full_path)

    for name in os.listdir(commit_dir):
        full_path = join(commit_dir, name)
        if isdir(full_path):
            shutil.copytree(full_path, join(path, name))
        else:
            shutil.copy(full_path, join(path, name))
    
    if version is not 'stash':
        track_version(path, version)

def checkout(path, version):
    versions = get_versions(path)
    if version == 'stash':
        if not stash_exists(path):
            version = max([int(i) for i in versions])
        checkout_version(path, str(version))
    elif version not in versions:           
        print 'No such version'
    else:
        commit(path, version='stash')
        checkout_version(path, version)

def track_version(path, version):
    with open(join(path, META_DIR, 'head'), 'w') as f:
        f.write(version)

def print_version(path):
    with open(join(path, META_DIR, 'head')) as f:
        print f.read()
    

if __name__ == '__main__':
    # FIXME: This is a bad idea.  It should look for a .myvcs somewhere up in
    # the tree, or current directory.
    path = os.getcwd()
    assert len(sys.argv) > 1, 'Need a command to run'

    command = sys.argv[1]
    if command == 'commit':
        commit(path)

    elif command == 'checkout':
        checkout(path, sys.argv[2])
    
    elif command == 'latest':
        checkout(path, 'stash')
    
    elif command == 'current':
        print_version(path)
        
    else:
        print 'Unknown command!'
        sys.exit(1)
