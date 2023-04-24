# List the available recipes
default:
	@just --list --justfile {{justfile()}}

# Build and document the project
build-all: build doc

# Build the project.
build: fmt
	cabal build

# Document the project.
doc: fmt
	cabal haddock --haddock-hyperlink-source --haddock-quickjump

# Test the project.
test: fmt
	cabal test

# Format the Haskell source code
fmt:
	fd -e hs -X ormolu -i

# Clean the project
clean:
	cabal clean
