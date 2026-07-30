.PHONY: run interpret build clean

run: build
	./_build/default/bin/main.exe

interpret: build
ifndef FILE
	$(error Usage: make interpret FILE=path/to/file.aera)
endif
	./_build/default/bin/main.exe $(FILE)

build:
  @echo "Compiling the language..."
	@opam exec -- dune build

clean:
  @echo "Cleaning up build..."
	@opam exec -- dune clean