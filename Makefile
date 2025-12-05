.PHONY: help test build clean run css format-examples

help:
	@echo "quickslice - Makefile Commands"
	@echo ""
	@echo "  make run      - Start server"
	@echo "  make test     - Run all tests"
	@echo "  make build    - Build all projects"
	@echo "  make clean    - Clean build artifacts"
	@echo "  make format-examples - Format example HTML files"
	@echo ""

# Run all tests
test: build
	@echo "Running lexicon_graphql tests..."
	@cd lexicon_graphql && gleam test
	@echo ""
	@echo "Running server tests..."
	@cd server && gleam test
	@echo ""
	@echo "All tests passed"

# Run server
run: build
	@echo "Starting server..."
	@cd server && gleam run

# Clean build artifacts
clean:
	@cd lexicon_graphql && gleam clean
	@cd server && gleam clean
	@echo "Clean complete"

# Format example HTML files
format-examples:
	@prettier --write "examples/**/*.html"
