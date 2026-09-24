.PHONY: build clean test

build:
	@echo Building the language...
	@opam exec -- dune build

clean:
	@echo Cleaning up build...
	@opam exec -- dune clean

test:
	@echo Running tests$(if $(TEST_PATH), in $(TEST_PATH),)...
	@opam exec -- dune test --force $(TEST_PATH)