CXX = g++
CXXFLAGS = -std=c++17 -Wall -Wextra -g
TARGET = main
SRCDIR = src
SOURCES = $(wildcard $(SRCDIR)/*.cpp)

BUILDDIR = build
DEBUGDIR = $(BUILDDIR)/debug
RELEASEDIR = $(BUILDDIR)/release

DEBUG_OBJECTS = $(SOURCES:$(SRCDIR)/%.cpp=$(DEBUGDIR)/%.o)
RELEASE_OBJECTS = $(SOURCES:$(SRCDIR)/%.cpp=$(RELEASEDIR)/%.o)
DEFAULT_OBJECTS = $(SOURCES:$(SRCDIR)/%.cpp=$(BUILDDIR)/%.o)

.PHONY: all clean debug release dirs

all: dirs $(BUILDDIR)/$(TARGET)
debug: dirs $(DEBUGDIR)/$(TARGET)
release: dirs $(RELEASEDIR)/$(TARGET)
dirs:
	@mkdir -p $(BUILDDIR) $(DEBUGDIR) $(RELEASEDIR)


# general 
$(BUILDDIR)/$(TARGET): $(DEFAULT_OBJECTS)
	$(CXX) $(DEFAULT_OBJECTS) -o $@
$(BUILDDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@


# debug target
$(DEBUGDIR)/$(TARGET): $(DEBUG_OBJECTS)
	$(CXX) $(DEBUG_OBJECTS) -o $@ -fsanitize=address,undefined
$(DEBUGDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) $(CXXFLAGS) -DDEBUG -O0 -ggdb3 -fsanitize=address,undefined -c $< -o $@


# release target
$(RELEASEDIR)/$(TARGET): $(RELEASE_OBJECTS)
	$(CXX) $(RELEASE_OBJECTS) -o $@
$(RELEASEDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) $(CXXFLAGS) -DNDEBUG -O2 -c $< -o $@

run: debug
	@$(DEBUGDIR)/$(TARGET)

clean:
	rm -rf $(BUILDDIR)
