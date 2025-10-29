.PHONY: help test build clean run

help:
	@echo "QuickSlice - Makefile Commands"
	@echo ""
	@echo "  make run      - Start server"
	@echo "  make test     - Run all tests"
	@echo "  make build    - Build all projects"
	@echo "  make clean    - Clean build artifacts"
	@echo ""

# Build all projects
build:
	@echo "Building graphql package..."
	@cd graphql && gleam build
	@echo ""
	@echo "Building lexicon_graphql package..."
	@cd lexicon_graphql && gleam build
	@echo ""
	@echo "Building server..."
	@cd server && gleam build
	@echo ""
	@echo "Build complete"

# Run all tests
test: build
	@echo "Running graphql tests..."
	@cd graphql && gleam test
	@echo ""
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
	@echo "Cleaning build artifacts..."
	@cd graphql && gleam clean
	@cd lexicon_graphql && gleam clean
	@cd server && gleam clean
	@echo "Clean complete"
