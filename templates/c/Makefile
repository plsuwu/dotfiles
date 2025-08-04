CC=gcc
CFLAGS=-std=c17 -Wall -Wextra -g
TARGET=main
SOURCE_DIR=src

BUILD_DIR=build
SOURCES=$(wildcard $(SOURCE_DIR)/*.c)
OBJECTS=$(patsubst $(SOURCE_DIR)/%.c,$(BUILD_DIR)/%.o,$(SOURCES))

.PHONY: all clean debug release

all: $(BUILD_DIR)/$(TARGET)

$(BUILD_DIR)/$(TARGET): $(OBJECTS) | $(BUILD_DIR)
	$(CC) $(OBJECTS) -o $@

$(BUILD_DIR)/%.o: $(SOURCE_DIR)/%.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

clean:
	rm -rf build/

debug: BUILD_DIR=build/debug
debug: CFLAGS += -DDEBUG -O0 -ggdb3
debug: $(BUILD_DIR)/$(TARGET)

release: BUILD_DIR=build/release
release: CFLAGS += -DNDEBUG -O2
release: $(BUILD_DIR)/$(TARGET)
