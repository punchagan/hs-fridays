#!/usr/bin/env python
"""Usage:
  myvcs commit [-m <message>]
  myvcs checkout <version>
  myvcs current
  myvcs diff <version1> <version2>
  myvcs latest
  myvcs log
"""

import difflib
import docopt
import filecmp
import json
import os
from os.path import basename, exists, isdir, join
import shutil
import string
import sys
import time

META_DIR = '.myvcs'
LOG_FILE = '.log.json'


class Repository(object):
    """ A class to represent and manage a repository. """

    META_DIR = '.myvcs'
    LOG_FILE = '.log.json'

    def __init__(self, root):
        """Constructor.

        root: The path to the root of the repository (the directory that
        contains the .myvcs directory)

        """

        self.root = root
        assert exists(self.root)

        self._log = self.read_log()
        self.versions = len(self._log)

        vcs_dir = self.get_vcs_dir()
        if not exists(vcs_dir):
            os.mkdir(vcs_dir)

    def commit(self, message):

        def is_backup_dir(src, names):
            if src == self.root:
                return [self.META_DIR]
            return []

        # track_version(path, version)
        version = self._next_version()
        # backup_dir = self._get_version_path(version)
        # shutil.copytree(self.root, backup_dir, ignore=is_backup_dir)
        self._add_log(version, message)
        self.versions += 1


    def get_log_path(self):
        return join(self.get_vcs_dir(), self.LOG_FILE)

    def get_vcs_dir(self):
        return join(self.root, self.META_DIR)

    def read_log(self):
        log_path = self.get_log_path()

        if exists(log_path):
            try:
                with open(log_path) as f:
                    data = json.load(f)
            except ValueError:
                data = {}

        else:
            data = {}

        return data

    def write_log(self, data):
        with open(self.get_log_path(), 'w') as f:
            json.dump(data, f)

    def _add_log(self, version, message):
        data = self.read_log()
        data[version] = {
            'timestamp': time.time(),
            'message': message
        }
        self.write_log(data)

    def _next_version(self):
        return self.versions + 1

# def add_log(path, version, message):
#     log_file = join(path, META_DIR, LOG_FILE)
#     data = read_log(log_file)
#     data[version] = {
#         'timestamp': time.time(),
#         'message': message
#     }
#     write_log(log_file, data)

# def read_log(log_file):
#     if exists(log_file):
#         try:
#             with open(log_file) as f:
#                 data = json.load(f)
#         except ValueError:
#             data = {}

#     else:
#         data = {}

#     return data

# def write_log(log_file, data):
#     with open(log_file, 'w') as f:
#         json.dump(data, f)

# def get_next_backup_dir(backup_dir, versions):
#     return join(backup_dir, str(len(versions) + 1))

# def get_versions(path):
#     backup_dir = join(path, META_DIR)
#     if exists(backup_dir):
#         versions = [
#             name for name in os.listdir(join(path, META_DIR))
#             if name.startswith(tuple(string.digits))
#         ]
#     else:
#         versions = []
#     return versions

# def get_max_version(versions):
#     version = max([int(i) for i in versions])
#     return str(version)

# def stash_exists(path):
#     return exists(join(path, META_DIR

#         , 'stash'))

# def commit(path, message=None, version=None):
#     versions = get_versions(path)

#     def is_backup_dir(src, names):
#         if src == path:
#             return [META_DIR]
#         return []

#     if version == None:
#         backup_dir = get_next_backup_dir(join(path, META_DIR), versions)
#         version = basename(backup_dir)
#         if message is None:
#             print 'Need message'
#         else:
#             track_version(path, version)

#     elif version == 'stash':
#         backup_dir = join(path, META_DIR, version)
#         if exists(backup_dir):
#             shutil.rmtree(backup_dir)

#     else:
#         print 'Unknown version'

#     shutil.copytree(path, backup_dir, ignore=is_backup_dir)
#     add_log(path, version, message)
#     # FIXME: version 21?


# def checkout_version(path, version):
#     commit_dir = join(path, META_DIR, version)

#     for name in os.listdir(path):
#         if name == META_DIR:
#             continue
#         full_path = join(path, name)
#         if isdir(full_path):
#             shutil.rmtree(full_path)
#         else:
#             os.remove(full_path)

#     for name in os.listdir(commit_dir):
#         full_path = join(commit_dir, name)
#         if isdir(full_path):
#             shutil.copytree(full_path, join(path, name))
#         else:
#             shutil.copy(full_path, join(path, name))

#     if version is 'stash':
#         version = get_max_version(get_versions(path))
#     track_version(path, version)

# def checkout(path, version):
#     versions = get_versions(path)
#     if version == 'stash':
#         if not stash_exists(path):
#             version = get_max_version(versions)
#         checkout_version(path, version)
#     elif version not in versions:
#         print 'No such version'
#     else:
#         commit(path, version='stash')
#         checkout_version(path, version)

# def track_version(path, version):
#     with open(join(path, META_DIR, 'head'), 'w') as f:
#         f.write(version)

# def print_version(path):
#     with open(join(path, META_DIR, 'head')) as f:
#         print f.read()

# def print_log(path):
#     data = read_log(join(path, META_DIR, LOG_FILE))

#     for version in sorted(data):
#         metadata = data[version]
#         print 'Commit: %s' % version
#         print 'Date: %s' % time.ctime(metadata['timestamp'])
#         print '\n    %s' % metadata['message']
#         print '\n'

# def print_diff(path, version1, version2):
#     path1 = join(path, META_DIR, version1)
#     path2 = join(path, META_DIR, version2)
#     print_dir_diff(path1, path2)

# def print_dir_diff(path1, path2):
#     diff_report = filecmp.dircmp(path1, path2)

#     diff_files = diff_report.diff_files
#     for file in diff_files:
#         print_file_diff(join(path1, file), join(path2, file))

#     common_dirs = diff_report.common_dirs
#     for dir in common_dirs:
#         print_dir_diff(join(path1, dir), join(path2, dir))

#     left_only, right_only = diff_report.left_only, diff_report.right_only
#     for left in left_only:
#         path = join(path1, left)
#         if isdir(path):
#             print '--- %s' % path
#         else:
#             print_file_diff(path, '/dev/null')

#     for right in right_only:
#         path = join(path2, right)
#         if isdir(path):
#             print '+++ %s' % path
#         else:
#             print_file_diff('/dev/null', path)

# def get_text(file):
#     if file == '/dev/null':
#         text = ''
#     else:
#         with open(file) as f:
#             text = f.readlines()
#     return text

# def print_file_diff(file1, file2):
#     for line in difflib.unified_diff(get_text(file1), get_text(file2), file1, file2):
#         print line.strip()
#     print

# def parse_arguments(args=None):
#     if args is None:
#         args = sys.argv


# if __name__ == '__main__':
#     # FIXME: This is a bad idea.  It should look for a .myvcs somewhere up in
#     # the tree, or current directory.
#     path = os.getcwd()
#     args = docopt.docopt(__doc__)

#     if args['commit']:
#         message = args['<message>']
#         if message is None:
#             print 'No message specified. Use -m to specify one.'
#             print __doc__
#             # fixme: open editor.
#         else:
#             commit(path, message)

#     elif args['checkout']:
#         checkout(path, sys.argv[2])

#     elif args['latest']:
#         checkout(path, 'stash')

#     elif args['current']:
#         print_version(path)

#     elif args['log']:
#         print_log(path)

#     elif args['diff']:
#         print_diff(path, args['<version1>'], args['<version2>'])

#     else:
#         print 'Unknown command!'
#         sys.exit(1)
