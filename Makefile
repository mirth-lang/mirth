# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

typecheck:
	python3 bootstrap/mirth.py --typecheck src

test:
	PYTHONHASHSEED=0 python3 bootstrap/mirth.py src mtest

test-update:
	PYTHONHASHSEED=0 python3 bootstrap/mirth.py src mtest --update

build:
	PYTHONHASHSEED=0 python3 bootstrap/mirth.py src build

bootstrap-test:
	PYTHONHASHSEED=0 python3 bootstrap/test.py

install-vim:
	mkdir -p ~/.vim/bundle
	rm -rf ~/.vim/bundle/mirth-vim
	cp -r tools/mirth-vim ~/.vim/bundle/

.PHONY: typecheck test build bootstrap-test install-vim

