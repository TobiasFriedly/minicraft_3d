BASE_TARGET := minicraft_3D_by_GameOfTobi
TARGET ?= $(BASE_TARGET)
SRCS := src/main.c src/render/mode4_fast_asm.S
ifndef DEVKITPRO
$(error DEVKITPRO is not set. Define it, e.g. /opt/devkitpro)
endif
ifndef DEVKITARM
$(error DEVKITARM is not set. It should be $(DEVKITPRO)/devkitARM)
endif
CC := $(DEVKITARM)/bin/arm-none-eabi-gcc
OBJCOPY := $(DEVKITARM)/bin/arm-none-eabi-objcopy
GBAFIX := $(DEVKITPRO)/tools/bin/gbafix
GDB := $(DEVKITARM)/bin/arm-none-eabi-gdb
MGBA ?= /Applications/mGBA.app/Contents/MacOS/mGBA
LIBGBA_INC := $(DEVKITPRO)/libgba/include
LIBGBA_LIB := $(DEVKITPRO)/libgba/lib
GBA_SPECS := $(DEVKITARM)/arm-none-eabi/lib/gba.specs
COMMON_CFLAGS := -marm -mthumb-interwork -mcpu=arm7tdmi -mtune=arm7tdmi \
	-fno-strict-aliasing -ffunction-sections -fdata-sections \
	-falign-functions=16 -falign-loops=16 -Wall -Wextra -std=gnu99 \
	-I$(LIBGBA_INC) -Isrc -Isrc/render -Isrc/world -Isrc/data
RELEASE_CFLAGS := -O3 -flto $(COMMON_CFLAGS) \
	-fomit-frame-pointer -ffast-math -fno-strict-aliasing \
	-DNDEBUG
PROFILE_CFLAGS := -O3 -flto -g3 -gdwarf-4 $(COMMON_CFLAGS) \
	-fomit-frame-pointer -ffast-math -DPERF_PROFILE=1
CFLAGS ?= $(RELEASE_CFLAGS)
COMMON_LDFLAGS := -specs=$(GBA_SPECS) -Wl,--gc-sections -L$(LIBGBA_LIB)
RELEASE_LDFLAGS := -flto $(COMMON_LDFLAGS)
PROFILE_LDFLAGS := -flto $(COMMON_LDFLAGS)
LDFLAGS ?= $(RELEASE_LDFLAGS)
LIBS := -lgba -lm
all: $(TARGET).gba
$(TARGET).elf: $(SRCS)
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS) -Wl,-Map=$(TARGET).map $(LIBS)
$(TARGET).gba: $(TARGET).elf
	$(OBJCOPY) -O binary $< $@
	$(GBAFIX) $@
clean:
	rm -f $(BASE_TARGET).elf $(BASE_TARGET).gba $(BASE_TARGET).map
	rm -f $(BASE_TARGET)_profile_ingame.elf $(BASE_TARGET)_profile_ingame.gba $(BASE_TARGET)_profile_ingame.map
	rm -f $(BASE_TARGET)_profile_worldgen.elf $(BASE_TARGET)_profile_worldgen.gba $(BASE_TARGET)_profile_worldgen.map

profile-ingame:
	$(MAKE) TARGET=$(BASE_TARGET)_profile_ingame CFLAGS='$(PROFILE_CFLAGS) -DPERF_AUTOSTART=1 -DPERF_RENDER_SCALE=2 -DPERF_VIS_DIST=9 -DPERF_FACE_LIMIT=1600' LDFLAGS='$(PROFILE_LDFLAGS)' all

profile-worldgen:
	$(MAKE) TARGET=$(BASE_TARGET)_profile_worldgen CFLAGS='$(PROFILE_CFLAGS) -DPERF_AUTOSTART=1 -DPERF_HALT_AFTER_WORLDGEN=1 -DPERF_RENDER_SCALE=2 -DPERF_VIS_DIST=9 -DPERF_FACE_LIMIT=1600' LDFLAGS='$(PROFILE_LDFLAGS)' all

run-profile-ingame: profile-ingame
	$(MGBA) -g $(BASE_TARGET)_profile_ingame.gba

run-profile-worldgen: profile-worldgen
	$(MGBA) -g $(BASE_TARGET)_profile_worldgen.gba

gdb-profile-ingame:
	$(GDB) -q $(BASE_TARGET)_profile_ingame.elf -ex "target remote localhost:2345"

gdb-profile-worldgen:
	$(GDB) -q $(BASE_TARGET)_profile_worldgen.elf -ex "target remote localhost:2345"

.PHONY: all clean profile-ingame profile-worldgen run-profile-ingame run-profile-worldgen gdb-profile-ingame gdb-profile-worldgen
