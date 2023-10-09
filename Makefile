# If RACK_DIR is not defined when calling the Makefile, default to two directories above
RACK_DIR ?= ../..

VERILATOR_ROOT=$(shell verilator -getenv VERILATOR_ROOT)
# Where to build the verilated RTL.
VERILATED_PATH=build/rtl-verilated
# Files generated by verilator that we must execute / link to.
VERILATED_MK=Vcore.mk
VERILATED_OBJ=Vcore__ALL.a

# FLAGS will be passed to both the C and C++ compiler
FLAGS += -I$(VERILATOR_ROOT)/include -faligned-new
CFLAGS +=
CXXFLAGS +=

# Careful about linking to shared libraries, since you can't assume much about the user's environment and library search path.
# Static libraries are fine, but they should be added to this plugin's build system.
#LDFLAGS += -L$(VERILATED_PATH) -l:$(VERILATED_OBJ)
OBJECTS += $(VERILATED_PATH)/$(VERILATED_OBJ)

# Add .cpp files to the build
SOURCES += $(wildcard src/*.cpp) $(VERILATOR_ROOT)/include/verilated.cpp

# Add files to the ZIP package when running `make dist`
# The compiled plugin and "plugin.json" are automatically added.
DISTRIBUTABLES += res
DISTRIBUTABLES += $(wildcard LICENSE*)
DISTRIBUTABLES += $(wildcard presets)

rtl/core.v: rtl/Core.hs
	clash --verilog rtl/Core.hs
	cp verilog/Core.topEntity/core.v rtl/core.v

$(VERILATED_PATH)/Vcore.mk: rtl/core.v
	mkdir -p build; \
	verilator --cc $< -Mdir $(VERILATED_PATH) -CFLAGS -fPIC

$(VERILATED_PATH)/$(VERILATED_OBJ): $(VERILATED_PATH)/Vcore.mk
	make -C $(VERILATED_PATH) -f $(VERILATED_MK)

src/eurorack-pmod.cpp: $(VERILATED_PATH)/$(VERILATED_OBJ)

# Include the Rack plugin Makefile framework
include $(RACK_DIR)/plugin.mk
