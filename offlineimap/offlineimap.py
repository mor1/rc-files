#!/usr/bin/env python3
#
# Copyright (c) Richard Mortier <mort@cantab.net>
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

from subprocess import check_output

def pass_get_password(account):
    return check_output(f"pass Offlineimap/{account}", shell=True).splitlines()[0]

def m365_filterfolder(folder):
    return folder not in [
        'Calendar',
        'Calendar/Birthdays',
        'Calendar/Holidays in United Kingdom',
        'Calendar/United Kingdom holidays',
        'Calendar/United States holidays',
        'Contacts',
        'Contacts/Skype for Business Contacts',
        'Deleted Items',
        'Drafts',
        'Journal',
        'Junk Email',
        'Notes',
        'Outbox',
        'Sync Issues',
        'Sync Issues/Conflicts',
        'Sync Issues/Local Failures',
        'Sync Issues/Server Failures',
        'Tasks',
        'Tasks/Sub Folder 1',
        'Tasks/Sub Folder 2'
    ]


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

def gmail_to_remotefolder(folder):
    return Gmail_LocalRemoteNames.get(folder, folder)
def gmail_to_localfolder(folder):
    return Gmail_RemoteLocalNames.get(folder, folder)

def gmail_folderfilter(folder):
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

if __name__ == '__main__':
    SERVERS = {
        "richard.mortier@gmail.com": "imap.gmail.com",
        "richard_mortier@hotmail.com": "imap-mail.outlook.com",
        "mort@live.co.uk": "imap-mail.outlook.com",
        "mort+access-token@ikva.ai": "outlook.office365.com",
    }

    for a, s in SERVERS.items():
        print(f"ACCOUNT {a} SERVER {s} PASSWORD {pass_get_password(a)}")
