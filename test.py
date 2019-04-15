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

for path in glob.glob('bootstrap/pass/*'):
    with subprocess.Popen([interp, path],
            stdout = subprocess.PIPE,
            stderr = subprocess.PIPE) as proc:

        (outs, errs) = proc.communicate()
        if proc.returncode > 0:
            print ("\nTEST FAILED [", path, "]")
            if outs: print ("\nSTDOUT:\n\n" + outs.decode('utf8') + '\n')
            if errs: print ("\nSTDERR:\n\n" + errs.decode('utf8') + '\n')
            print ()

            failed += 1

for path in glob.glob('bootstrap/fail_types/*'):
    with subprocess.Popen([interp, path],
            stdout = subprocess.PIPE,
            stderr = subprocess.PIPE) as proc:

        (outs, errs) = proc.communicate()
        if 'TypeError' not in errs.decode('utf8'):
            print ("\nTEST FAILED [", path, "]")
            if outs: print ("\nSTDOUT:\n\n" + outs.decode('utf8') + '\n')
            if errs: print ("\nSTDERR:\n\n" + errs.decode('utf8') + '\n')
            print ()

            failed += 1

for path in glob.glob('bootstrap/fail_tests/*'):
    with subprocess.Popen([interp, path],
            stdout = subprocess.PIPE,
            stderr = subprocess.PIPE) as proc:

        (outs, errs) = proc.communicate()
        if 'Assertion failed' not in errs.decode('utf8'):
            print ("\nTEST FAILED [", path, "]")
            if outs: print ("\nSTDOUT:\n\n" + outs.decode('utf8') + '\n')
            if errs: print ("\nSTDERR:\n\n" + errs.decode('utf8') + '\n')
            print ()

            failed += 1

sys.exit(failed)

