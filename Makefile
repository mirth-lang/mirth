
build:
	python3 bootstrap/mirth.py src

test:
	python3 test.py

.PHONY: build test

