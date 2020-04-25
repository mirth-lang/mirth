C99FLAGS=-std=c99 -Wall -Wextra -Wno-unused-function -Wno-unused-parameter -Werror -pedantic
CC=gcc $(C99FLAGS)

.PHONY: default show build update check update-mirth install-vim install-code profile play-snake

default: show

show: bin/mirth0.c bin/mirth1.c bin/mirth2.c bin/mirth3.c
	diff --strip-trailing-cr bin/mirth0.c bin/mirth1.c | head -n 5
	diff --strip-trailing-cr bin/mirth1.c bin/mirth2.c | head -n 10
	diff --strip-trailing-cr bin/mirth2.c bin/mirth3.c

build: mirth0 mirth1 mirth2 mirth1.c mirth2.c mirth3.c

update: bin/mirth0.c bin/mirth3.c
	cp bin/mirth3.c bin/mirth0.c

check:
	diff --strip-trailing-cr bin/mirth2.c bin/mirth3.c
	diff --strip-trailing-cr bin/mirth1.c bin/mirth3.c
	diff --strip-trailing-cr bin/mirth0.c bin/mirth3.c

clean:
	rm -f bin/mirth1.c bin/mirth2.c bin/mirth3.c bin/mirth0 bin/mirth1 bin/mirth2 bin/mirth_prof bin/mirth_prof.c bin/snake.c bin/snake

install-vim:
	mkdir -p ~/.vim/bundle
	rm -rf ~/.vim/bundle/mirth-vim
	cp -r tools/mirth-vim ~/.vim/bundle/

install-code:
	code --install-extension tools/mirth-code/mirth-*.vsix

profile: bin/mirth_prof
	time bin/mirth_prof
	rm -f bin/mirth.c

play-snake: bin/snake
	bin/snake

#########

bin/mirth0: bin/mirth0.c
	$(CC) -o bin/mirth0 bin/mirth0.c

bin/mirth1: bin/mirth1.c
	$(CC) -o bin/mirth1 bin/mirth1.c

bin/mirth2: bin/mirth2.c
	$(CC) -o bin/mirth2 bin/mirth2.c

bin/mirth1.c: bin/mirth0 src/*.mth
	bin/mirth0
	mv bin/mirth.c bin/mirth1.c

bin/mirth2.c: bin/mirth1 src/*.mth
	bin/mirth1
	mv bin/mirth.c bin/mirth2.c

bin/mirth3.c: bin/mirth2 src/*.mth
	bin/mirth2
	mv bin/mirth.c bin/mirth3.c

bin/mirth_prof.c: bin/mirth3.c

bin/mirth_prof: bin/mirth_prof.c
	$(CC) -g -fprofile-instr-generate -o bin/mirth_prof bin/mirth_prof.c

bin/snake.c: bin/mirth2 src/*.mth
	bin/mirth2
	rm -f bin/mirth.c

bin/snake: bin/snake.c
	$(CC) -o bin/snake bin/snake.c `pkg-config --libs sdl2`
