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
	cabal haddock --haddock-hyperlink-source --haddock-quickjump \
	| colout '^(Warning:)(.*)$' "red,yellow"

# Test the project.
test: fmt
	cabal test \
	| colout -T ~/Code/haskell/composer/.colout -t cabal

# Format the Haskell source code
fmt:
	fd -e hs -X ormolu -i

# Clean the project
clean:
	cabal clean
