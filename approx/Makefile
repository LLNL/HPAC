SRCS=$(wildcard runtime/*.cpp)
CXXFLAGS=-fPIC -shared
TARGET=libapprox.so

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(SRCS)
	$(CC) $(CXXFLAGS) $< -o $@

clean:
	rm -rf $(TARGET)

