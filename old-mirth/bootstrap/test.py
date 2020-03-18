#!/usr/bin/env python3

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

'''
This is a test runner for the bootstrap. Just invoke with python3
from inside the top-level repository directory.
'''

import sys
import glob
import subprocess

# run bootstrap tests

failed = 0
interp = 'bootstrap/mirth.py'

print(' '.join([interp, '--doctest']))
subprocess.run([interp, '--doctest'])

def test_interp(args, passfn):
    global failed
    cmd = [interp] + args
    print(' '.join(cmd), file=sys.stderr)
    with subprocess.Popen(cmd,
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

def succeeds(exit_code, outs, errs):
    return exit_code == 0

def stderr_contains(msg):
    return lambda exit_code, outs, errs: msg in errs

test_interp(['--no-prelude', 'bootstrap/prelude.mth'], succeeds)

for path in glob.glob('bootstrap/pass/*'):
    test_interp([path], succeeds)

for path in glob.glob('bootstrap/fail_syntax/*'):
    test_interp([path], stderr_contains('Syntax Error'))

for path in glob.glob('bootstrap/fail_types/*'):
    test_interp([path], stderr_contains('Type Error'))

for path in glob.glob('bootstrap/fail_tests/*'):
    test_interp([path], stderr_contains('Assertion failed'))

sys.exit(failed)

