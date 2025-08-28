CC = gcc
CFLAGS = -std=c11 -Wall -Wextra -g -MMD -MP
TARGET = main
SRCDIR = src

SOURCES = $(shell find $(SRCDIR) -name '*.c')

BUILDDIR = build
DEBUGDIR = $(BUILDDIR)/debug
RELEASEDIR = $(BUILDDIR)/release

DEBUG_OBJECTS = $(SOURCES:$(SRCDIR)/%.c=$(DEBUGDIR)/%.o)
RELEASE_OBJECTS = $(SOURCES:$(SRCDIR)/%.c=$(RELEASEDIR)/%.o)
DEFAULT_OBJECTS = $(SOURCES:$(SRCDIR)/%.c=$(BUILDDIR)/%.o)

DEBUG_DEPS = $(DEBUG_OBJECTS:.o=.d)
RELEASE_DEPS = $(RELEASE_OBJECTS:.o=.d)
DEFAULT_DEPS = $(DEFAULT_OBJECTS:.o=.d)

.PHONY: all clean debug release dirs

all: dirs $(BUILDDIR)/$(TARGET)

debug: dirs $(DEBUGDIR)/$(TARGET)

release: dirs $(RELEASEDIR)/$(TARGET)

dirs:
	@mkdir -p $(BUILDDIR) $(DEBUGDIR) $(RELEASEDIR)
	@mkdir -p $(dir $(DEBUG_OBJECTS))
	@mkdir -p $(dir $(RELEASE_OBJECTS))
	@mkdir -p $(dir $(DEFAULT_OBJECTS))

$(BUILDDIR)/$(TARGET): $(DEFAULT_OBJECTS)
	$(CC) $(DEFAULT_OBJECTS) -o $@

$(BUILDDIR)/%.o: $(SRCDIR)/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

$(DEBUGDIR)/$(TARGET): $(DEBUG_OBJECTS)
	$(CC) $(DEBUG_OBJECTS) -o $@

$(DEBUGDIR)/%.o: $(SRCDIR)/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -DDEBUG -O0 -ggdb3 -c $< -o $@

$(RELEASEDIR)/$(TARGET): $(RELEASE_OBJECTS)
	$(CC) $(RELEASE_OBJECTS) -o $@

$(RELEASEDIR)/%.o: $(SRCDIR)/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -DNDEBUG -O2 -c $< -o $@

run: debug
	@$(DEBUGDIR)/$(TARGET)

clean:
	rm -rf $(BUILDDIR)

-include $(DEFAULT_DEPS)
-include $(DEBUG_DEPS)
-include $(RELEASE_DEPS)
