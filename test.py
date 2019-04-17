#!/usr/bin/env python3

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

'''
This is a test runner for this project. Just invoke with python3.
This will run tests for the bootstrap.
'''

import sys
import glob
import subprocess

# run bootstrap tests

failed = 0
interp = 'bootstrap/mirth.py'
subprocess.run([interp, '--doctest'])

def test_interp(args, passfn):
    global failed
    with subprocess.Popen([interp] + args,
            stdout = subprocess.PIPE,
            stderr = subprocess.PIPE ) as proc:
        (outs, errs) = proc.communicate()
        outs = outs.decode('utf8')
        errs = errs.decode('utf8')
        if not passfn(proc.returncode, outs, errs):
            print ("\nTEST FAILED [ " + ' '.join(args) + " ]")
            if outs: print ("\nSTDOUT:\n\n" + outs + '\n')
            if errs: print ("\nSTDERR:\n\n" + errs + '\n')
            print ()
            failed += 1

test_interp(['--no-prelude', 'bootstrap/prelude.mth'],
    lambda exit_code, outs, errs: exit_code == 0)

for path in glob.glob('bootstrap/pass/*'):
    test_interp([path], lambda exit_code, outs, errs: exit_code == 0)

for path in glob.glob('bootstrap/fail_types/*'):
    test_interp([path], lambda exit_code, outs, errs: 'TypeError' in errs)

for path in glob.glob('bootstrap/fail_tests/*'):
    test_interp([path], lambda exit_code, outs, errs: 'Assertion failed' in errs)

sys.exit(failed)

