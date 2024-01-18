C99FLAGS=-std=c99 -Wall -Wextra -Wno-unused-variable -Wno-unused-function -Wno-unused-parameter -Wno-unused-value -Wno-missing-braces -Werror -pedantic -O0 -g
CC=gcc $(C99FLAGS)
CCSAN=$(CC) -fsanitize=undefined -fsanitize=address

SRCS=src/prelude.mth src/mirth.mth src/data/*.mth src/platform/*.mth src/mirth/*.mth src/mirth/data/*.mth src/mirth/*.h

.PHONY: default show showsan build buildsan update check checksan update-mirth install-vim install-code install profile play-snake test test-update

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

bin/mirth0: bin/mirth0.c
	$(CC) -o bin/mirth0 bin/mirth0.c

bin/mirth1: bin/mirth1.c
	$(CC) -o bin/mirth1 bin/mirth1.c

bin/mirth2: bin/mirth2.c
	$(CC) -o bin/mirth2 bin/mirth2.c

bin/mirth0san: bin/mirth0.c
	$(CCSAN) -o bin/mirth0san bin/mirth0.c

bin/mirth1san: bin/mirth1.c
	$(CCSAN) -o bin/mirth1san bin/mirth1.c

bin/mirth2san: bin/mirth2.c
	$(CCSAN) -o bin/mirth2san bin/mirth2.c

bin/mirth1.c: bin/mirth0 $(SRCS)
	bin/mirth0 mirth.mth
	mv bin/mirth.c bin/mirth1.c

bin/mirth2.c: bin/mirth1 $(SRCS)
	bin/mirth1 mirth.mth
	mv bin/mirth.c bin/mirth2.c

bin/mirth3.c: bin/mirth2 $(SRCS)
	bin/mirth2 mirth.mth
	mv bin/mirth.c bin/mirth3.c

bin/mirth3san.c: bin/mirth2san $(SRCS)
	bin/mirth2san mirth.mth
	mv bin/mirth.c bin/mirth3san.c

bin/mirth_prof.c: bin/mirth3.c

bin/mirth_prof: bin/mirth_prof.c
	$(CC) -g -fprofile-instr-generate -o bin/mirth_prof bin/mirth_prof.c

bin/snake.c: bin/mirth2 $(SRCS) src/sdl2.mth src/snake.mth
	bin/mirth2 snake.mth

bin/snake: bin/snake.c
	$(CC) -o bin/snake bin/snake.c `pkg-config --libs sdl2`
