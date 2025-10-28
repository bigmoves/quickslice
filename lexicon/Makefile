.PHONY: all build clean test

# Detect the operating system
UNAME_S := $(shell uname -s)

# Determine the library extension and target directory
ifeq ($(UNAME_S),Linux)
	LIB_EXT = so
	LIB_NAME = liblexicon_nif.so
endif
ifeq ($(UNAME_S),Darwin)
	LIB_EXT = dylib
	LIB_NAME = liblexicon_nif.dylib
endif
ifeq ($(OS),Windows_NT)
	LIB_EXT = dll
	LIB_NAME = lexicon_nif.dll
endif

NATIVE_DIR = native/lexicon_nif
TARGET_DIR = $(NATIVE_DIR)/target/release
PRIV_DIR = priv

all: build

build:
	@echo "Building Rust NIF library..."
	cd $(NATIVE_DIR) && cargo build --release
	@echo "Creating priv directory..."
	mkdir -p $(PRIV_DIR)
	@echo "Copying library to priv/..."
ifeq ($(UNAME_S),Darwin)
	cp $(TARGET_DIR)/$(LIB_NAME) $(PRIV_DIR)/liblexicon_nif.so
else
	cp $(TARGET_DIR)/$(LIB_NAME) $(PRIV_DIR)/
endif
	@echo "Build complete! Library is in $(PRIV_DIR)/"

clean:
	@echo "Cleaning build artifacts..."
	cd $(NATIVE_DIR) && cargo clean
	rm -rf $(PRIV_DIR)
	@echo "Clean complete!"

test: build
	@echo "Running Gleam tests..."
	gleam test
