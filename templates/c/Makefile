CC = gcc
CFLAGS = -std=c11 -Wall -Wextra -g
TARGET = main
SRCDIR = src
SOURCES = $(wildcard $(SRCDIR)/*.c)

BUILDDIR = build
DEBUGDIR = $(BUILDDIR)/debug
RELEASEDIR = $(BUILDDIR)/release

DEBUG_OBJECTS = $(SOURCES:$(SRCDIR)/%.c=$(DEBUGDIR)/%.o)
RELEASE_OBJECTS = $(SOURCES:$(SRCDIR)/%.c=$(RELEASEDIR)/%.o)
DEFAULT_OBJECTS = $(SOURCES:$(SRCDIR)/%.c=$(BUILDDIR)/%.o)

.PHONY: all clean debug release dirs

all: dirs $(BUILDDIR)/$(TARGET)
debug: dirs $(DEBUGDIR)/$(TARGET)
release: dirs $(RELEASEDIR)/$(TARGET)
dirs:
	@mkdir -p $(BUILDDIR) $(DEBUGDIR) $(RELEASEDIR)


# general
$(BUILDDIR)/$(TARGET): $(DEFAULT_OBJECTS)
	$(CC) $(DEFAULT_OBJECTS) -o $@
$(BUILDDIR)/%.o: $(SRCDIR)/%.c
	$(CC) $(CFLAGS) -c $< -o $@


# debug
$(DEBUGDIR)/$(TARGET): $(DEBUG_OBJECTS)
	$(CC) $(DEBUG_OBJECTS) -o $@
$(DEBUGDIR)/%.o: $(SRCDIR)/%.c
	$(CC) $(CFLAGS) -DDEBUG -O0 -ggdb3 -c $< -o $@


# release
$(RELEASEDIR)/$(TARGET): $(RELEASE_OBJECTS)
	$(CC) $(RELEASE_OBJECTS) -o $@
$(RELEASEDIR)/%.o: $(SRCDIR)/%.c
	$(CC) $(CFLAGS) -DNDEBUG -O2 -c $< -o $@

run: debug
	@$(DEBUGDIR)/$(TARGET)

clean:
	rm -rf $(BUILDDIR)
