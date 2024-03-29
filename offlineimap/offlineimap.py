#!/usr/bin/env python3
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

import subprocess

def get_keychain_pass(account=None, server=None):
    params = {
        'user': "mort",
        'exe': "/usr/bin/security",
        'a': account,
        's': server,
        }

    if server:
        params['cmd'] = "find-internet-password"
        command = "sudo -u {user} {exe} {cmd} -a {a} -s {s} -w".format(**params)
    else:
        params['cmd'] = "find-generic-password"
        command = "sudo -u {user} {exe} {cmd} -a {a} -w".format(**params)

    output = subprocess.check_output(command, shell=True, stderr=subprocess.STDOUT)

    return output.strip()

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

if __name__ == '__main__':
    SERVERS = {
        "richard.mortier@gmail.com": "imap.gmail.com",
        "mort@vipadia.com": "imap.gmail.com",
        "mort.vipadia": "imap.gmail.com",
        "richard_mortier@hotmail.com": "imap-mail.outlook.com",
        "mort@live.co.uk": "imap-mail.outlook.com",
        "mort@kvasira.com": "imap.gmail.com",
        "rmm1002@imap.hermes.cam.ac.uk": None
    }

    for a, s in SERVERS.items():
        print(f"ACCOUNT {a} SERVER {s} PASSWORD {get_keychain_pass(a, s)}")
