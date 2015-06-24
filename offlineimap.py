#!/usr/bin/python
#
# Copyright (C) 2015 Richard Mortier <mort@cantab.net>.  All Rights
# Reserved.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 2 as
# published by the Free Software Foundation
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.

import re, subprocess

def get_keychain_pass(account=None, server=None):
    ## based off from http://stevelosh.com/blog/2012/10/the-homely-mutt/

    localname = 'mort'
    params = {
        'localname': localname,
        'security': '/usr/bin/security',
        'command': 'find-internet-password',
        'account': account,
        'server': server,
        'keychain': '/Users/%s/Library/Keychains/login.keychain' % localname,
    }
    command = "sudo -u %(localname)s %(security)s -v %(command)s -g -a %(account)s -s %(server)s %(keychain)s" % params
    output = subprocess.check_output(command, shell=True, stderr=subprocess.STDOUT)
    outtext = [l for l in output.splitlines()
               if l.startswith('password: ')][0]

    return re.match(r'password: "(.*)"', outtext).group(1)

LocalRemoteNames = {
    'archive': '[Google Mail]/All Mail',
    'chats': '[Google Mail]/Chats',
    'drafts': '[Google Mail]/Drafts',
    'flagged': '[Google Mail]/Starred',
    'important': '[Google Mail]/Important',
    'sent': '[Google Mail]/Sent Mail',
    'trash': '[Google Mail]/Trash',
}
RemoteLocalNames = { r: l for (l, r) in LocalRemoteNames.items() }

def to_remotefolder(folder): return LocalRemoteNames.get(folder, folder)
def to_localfolder(folder): return RemoteLocalNames.get(folder, folder)

def is_included(folder):
    return folder not in [
        '[Google Mail]/All Mail',
        '[Google Mail]/Bin',
        '[Google Mail]/Important',
        '[Google Mail]/Spam',
        '[Google Mail]/Trash',
    ]
