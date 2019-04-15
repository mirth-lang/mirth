#!/usr/bin/env python3

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

'''
This is a test runner for this project. Just invoke with python3.
This will run tests for the bootstrap.
'''

import glob
import subprocess

# run bootstrap tests

interp = 'bootstrap/mirth.py'
subprocess.run([interp, '--doctest'])

for path in glob.glob('bootstrap/tests/pass/*.mth'):
    subprocess.run([interp, path])

