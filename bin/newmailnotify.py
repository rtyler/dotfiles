#!/usr/bin/env python

import json
import os
import subprocess
import sys
import time

DELAY = 60
INBOXES = ('~/Mail/Personal/INBOX', '~/Mail/Apture/INBOX',)

def notify(message):
    subprocess.call(['notify-send', '-i', 'mail_new', '-t', '5000', message])

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
            diff = old_set.difference(new_set)

            if diff:
                message = '%s has %d new ' % (box, len(diff))
                if len(diff) == 1:
                    message += 'mail'
                else:
                    message += 'mails'

                print ('Notifying', message)
                notify(message)

            mail_sets[box] = new_set

        print '>> Finished checking'
        time.sleep(DELAY)
    return 0


if __name__ == '__main__':
    sys.exit(main())

