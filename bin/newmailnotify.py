#!/usr/bin/env python

import json
import os
import subprocess
import sys
import time

DELAY = 30
INBOXES = ('~/Mail/Personal/INBOX', '~/Mail/Apture/INBOX',)

def notify(subject, message):
    subprocess.call(['notify-send', '-i', 'mail_new', '-t', '5000', subject, message])

def main():
    mail_sets = dict(((box, set()) for box in INBOXES))
    while True:
        for box in INBOXES:
            basepath = os.path.expanduser(box)
            newpath = os.path.join(basepath, 'new')
            if not os.path.exists(newpath) or not os.path.isdir(newpath):
                print (newpath, 'does not appear to be a directory')
                continue

            new_set = set(os.listdir(newpath))
            old_set = mail_sets[box]
            print ('old, new', len(old_set), len(new_set))
            diff = new_set.difference(old_set)

            if diff:
                noun = 'mail'
                if len(diff) > 1:
                    noun += 's'

                title = '%d new %s in %s' % (len(diff), noun, box.replace('~/Mail/', ''))
                message = 'Mail from:'
                senders = set()

                for f in diff:
                    filepath = os.path.join(newpath, f)

                    if not os.path.exists(filepath):
                        print 'Looks like "%s" no longer exists' % f
                        continue

                    with open(filepath, 'r') as fd:
                        for mail_line in fd.xreadlines():
                            if mail_line.startswith('From: '):
                                pieces = mail_line.strip().split('From: ')
                                if not pieces:
                                    continue

                                from_name = ' '.join(pieces[1:])

                                if not from_name.startswith('<'):
                                    from_name = from_name.split('<')[0]

                                senders.add(from_name.strip())
                                break

                for sender in senders:
                    message += '\\n%s' % sender

                print ('Notifying', title, message)
                notify(title, message)

            mail_sets[box] = new_set

        print '>> Finished checking'
        time.sleep(DELAY)
    return 0


if __name__ == '__main__':
    sys.exit(main())

