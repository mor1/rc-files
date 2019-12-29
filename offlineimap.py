#!/usr/bin/python
#
# Copyright (c) 2015 Richard Mortier <mort@cantab.net>
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

import re, subprocess

def get_keychain_pass(account=None, server=None):
    ## based off from http://stevelosh.com/blog/2012/10/the-homely-mutt/
    params = {
        'userid': 'mort',
        'security': '/usr/bin/security',
        'command': 'find-internet-password',
        'account': account,
        'server': server,
    }
    command = "sudo -u %(userid)s %(security)s -v %(command)s -g -a %(account)s -s %(server)s" % params
    output = subprocess.check_output(
        command, shell=True, stderr=subprocess.STDOUT
    )
    outtext = [
        l for l in output.splitlines() if l.startswith('password: ')
    ][0]
    password = re.match(r'password:\s+"(.*)"', outtext)
    if password: return password.group(1)

    password = re.match(
        r'password: 0x[0-9A-Z]+  "(.*)"', outtext
    )
    return password.group(1).split('\\012')[0]

Gmail_LocalRemoteNames = {
    'archive': '[Google Mail]/All Mail',
    'chats': '[Google Mail]/Chats',
    'drafts': '[Google Mail]/Drafts',
    'flagged': '[Google Mail]/Starred',
    'important': '[Google Mail]/Important',
    'sent': '[Google Mail]/Sent Mail',
    'trash': '[Google Mail]/Trash',
    'bin': '[Google Mail]/Bin',
    'spam': '[Google Mail]/Spam',
}
Gmail_RemoteLocalNames = { r: l for (l, r) in Gmail_LocalRemoteNames.items() }

def to_remotefolder(folder):
    return Gmail_LocalRemoteNames.get(folder, folder)
def to_localfolder(folder):
    return Gmail_RemoteLocalNames.get(folder, folder)

def gmail_is_synced(folder):
    return folder in [
        ## uk-based gmail accounts via Google Mail
        '[Google Mail]/All Mail',
        '[Google Mail]/Trash',
        '[Google Mail]/Drafts',
        ## google apps via Gmail
        '[Gmail]/All Mail',
        '[Gmail]/Trash',
        '[Gmail]/Drafts',
    ]

def hermes_is_synced(folder):
    return folder not in [
        'Archive',
    ]
