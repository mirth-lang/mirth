C99FLAGS=-std=c99 -Wall -Wextra -Wno-unused-variable -Wno-unused-function -Wno-unused-parameter \
 -Wno-unused-value -Wno-missing-braces -Wno-overlength-strings -Wno-infinite-recursion \
 -Werror -pedantic -O0
MIRTHFLAGS=-p std:src/std -p args:src/args -p mirth:src/mirth -p posix:src/posix -p examples:src/examples -p ansi:src/ansi

CC=gcc $(C99FLAGS)
CCSAN=$(CC) -fsanitize=undefined -fsanitize=address

SRCS=src/std/* src/args/* src/posix/* src/mirth/* src/ansi/*

.PHONY: default show showsan build buildsan debug update check checksan update-mirth install-vim install-code install profile play-snake test test-update

default: show bin/snake.c

show: bin/mirth0.c bin/mirth1.c bin/mirth2.c bin/mirth3.c
	diff --strip-trailing-cr bin/mirth0.c bin/mirth1.c | head -n 5
	diff --strip-trailing-cr bin/mirth1.c bin/mirth2.c | head -n 10
	diff --strip-trailing-cr bin/mirth2.c bin/mirth3.c
showsan: bin/mirth0.c bin/mirth1.c bin/mirth2.c bin/mirth3san.c
	diff --strip-trailing-cr bin/mirth0.c bin/mirth1.c | head -n 5
	diff --strip-trailing-cr bin/mirth1.c bin/mirth2.c | head -n 10
	diff --strip-trailing-cr bin/mirth2.c bin/mirth3san.c

build: bin/mirth0 bin/mirth1 bin/mirth2 bin/mirth1.c bin/mirth2.c bin/mirth3.c
buildsan: bin/mirth0 bin/mirth1 bin/mirth2san bin/mirth1.c bin/mirth2.c bin/mirth3san.c
debug: bin/mirth3debug

update: bin/mirth0.c bin/mirth3.c
	cp bin/mirth3.c bin/mirth0.c

check: bin/mirth0.c bin/mirth1.c bin/mirth2.c bin/mirth3.c
	diff --strip-trailing-cr bin/mirth2.c bin/mirth3.c
	diff --strip-trailing-cr bin/mirth1.c bin/mirth3.c
	diff --strip-trailing-cr bin/mirth0.c bin/mirth3.c

checksan: bin/mirth0.c bin/mirth1.c bin/mirth2.c bin/mirth3san.c
	diff --strip-trailing-cr bin/mirth0.c bin/mirth3san.c
	diff --strip-trailing-cr bin/mirth1.c bin/mirth3san.c
	diff --strip-trailing-cr bin/mirth2.c bin/mirth3san.c

clean:
	cp bin/mirth0.c mirth0.c
	rm -rf bin/mirth{0,1,2,}{,.c,.dSYM}
	mv mirth0.c bin/

install-vim:
	mkdir -p ~/.vim/bundle
	rm -rf ~/.vim/bundle/mirth-vim
	cp -r tools/mirth-vim ~/.vim/bundle/

install-code:
	code --install-extension tools/mirth-code/mirth-*.vsix

install-atom:
	apm link tools/mirth-atom

install: bin/mirth0
	./install.sh bin/mirth0

profile: bin/mirth_prof
	time bin/mirth_prof
	rm -f bin/mirth.c

play-snake: bin/snake
	bin/snake

test-verify:
	bash mirth-test.sh -v

test-update:
	bash mirth-test.sh -u

#########

bin/%: bin/%.c
	$(CC) -o $@ $^

bin/mirth0san: bin/mirth0.c
	$(CCSAN) -o bin/mirth0san bin/mirth0.c

bin/mirth1san: bin/mirth1.c
	$(CCSAN) -o bin/mirth1san bin/mirth1.c

bin/mirth2san: bin/mirth2.c
	$(CCSAN) -o bin/mirth2san bin/mirth2.c

bin/mirth1debug: bin/mirth1debug.c
	$(CC) -g -o bin/mirth1debug bin/mirth1debug.c

bin/mirth2debug: bin/mirth2debug.c
	$(CC) -g -o bin/mirth2debug bin/mirth2debug.c

bin/mirth3debug: bin/mirth3debug.c
	$(CC) -g -o bin/mirth3debug bin/mirth3debug.c

bin/mirth1.c: bin/mirth0 $(SRCS)
	bin/mirth0 $(MIRTHFLAGS) src/mirth/main.mth -o bin/mirth1.c

bin/mirth2.c: bin/mirth1 $(SRCS)
	bin/mirth1 $(MIRTHFLAGS) src/mirth/main.mth -o bin/mirth2.c

bin/mirth3.c: bin/mirth2 $(SRCS)
	bin/mirth2 $(MIRTHFLAGS) src/mirth/main.mth -o bin/mirth3.c

bin/mirth1debug.c: bin/mirth0 $(SRCS)
	bin/mirth0 $(MIRTHFLAGS) --debug src/mirth/main.mth -o bin/mirth1debug.c

bin/mirth2debug.c: bin/mirth1debug $(SRCS)
	bin/mirth1debug $(MIRTHFLAGS) --debug src/mirth/main.mth -o bin/mirth2debug.c

bin/mirth3debug.c: bin/mirth2debug $(SRCS)
	bin/mirth2debug $(MIRTHFLAGS) --debug mirth/main.mth -o bin/mirth3debug.c

bin/mirth3san.c: bin/mirth2san $(SRCS)
	bin/mirth2san $(MIRTHFLAGS) src/mirth/main.mth -o bin/mirth3san.c

bin/mirth_prof.c: bin/mirth3.c

bin/mirth_prof: bin/mirth_prof.c
	$(CC) -g -fprofile-instr-generate -o bin/mirth_prof bin/mirth_prof.c

bin/snake.c: bin/mirth2 src/std/* src/posix/* src/examples/snake.mth src/examples/sdl2.mth
	bin/mirth2 --debug $(MIRTHFLAGS) src/examples/snake.mth -o bin/snake.c

bin/snake: bin/snake.c
	$(CC) -o bin/snake bin/snake.c `pkg-config --libs sdl2`
