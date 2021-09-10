COMPILER = g++
LINK_FLAGS = -lGL -lX11 -ldl -pthread
COMPILER_FLAGS = -std=c++20 -O3 -c -o

OUTPUT_PATH = ./bin
OUTPUT_BINARY_NAME = game

SRC_PATH = ./src
INCLUDE_PATH = ./src/include
LIBS_PATH = ./libs
O_FILE_PATH = ./build_

CPP_FILES = $(wildcard $(SRC_PATH)/*.cpp)
O_FILES = $(subst $(SRC_PATH),$(O_FILE_PATH),  $(subst .cpp,.o, $(CPP_FILES) ))
A_FILES = $(wildcard $(LIBS_PATH)/*.a)

run: $(OUTPUT_PATH)/$(OUTPUT_BINARY_NAME)
	$(OUTPUT_PATH)/$(OUTPUT_BINARY_NAME)

build: $(OUTPUT_PATH)/$(OUTPUT_BINARY_NAME)

$(O_FILE_PATH):
	mkdir $(O_FILE_PATH)

$(O_FILES): $(CPP_FILES)
	$(COMPILER) $(CPP_FILES) -I $(INCLUDE_PATH) $(COMPILER_FLAGS) $(O_FILES)

$(OUTPUT_PATH)/$(OUTPUT_BINARY_NAME): $(O_FILE_PATH) $(O_FILES)
	$(COMPILER) $(O_FILES) $(A_FILES) $(LINK_FLAGS) -o $(OUTPUT_PATH)/$(OUTPUT_BINARY_NAME)

clean:
	rm $(O_FILE_PATH)/*.o

clean_build: clean
	rm $(OUTPUT_PATH)/$(OUTPUT_BINARY_NAME)
	make build
	make clean
	