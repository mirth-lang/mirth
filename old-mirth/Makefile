# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

typecheck:
	python3 bootstrap/mirth.py --typecheck src

test:
	python3 bootstrap/mirth.py src mtest

test-update:
	python3 bootstrap/mirth.py src mtest --update

build:
	python3 bootstrap/mirth.py src build

bootstrap-test:
	python3 bootstrap/test.py

install-vim:
	mkdir -p ~/.vim/bundle
	rm -rf ~/.vim/bundle/mirth-vim
	cp -r tools/mirth-vim ~/.vim/bundle/

install-code:
	code --install-extension tools/mirth-code/mirth-*.vsix

.PHONY: mirth typecheck test test-update build bootstrap-test install-vim install-code
