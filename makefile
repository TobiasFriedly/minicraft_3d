TARGET := minicraft_3D_by_GameOfTobi
SRCS := src/main.c
ifndef DEVKITPRO
$(error DEVKITPRO is not set. Define it, e.g. /opt/devkitpro)
endif
ifndef DEVKITARM
$(error DEVKITARM is not set. It should be $(DEVKITPRO)/devkitARM)
endif
CC := $(DEVKITARM)/bin/arm-none-eabi-gcc
OBJCOPY := $(DEVKITARM)/bin/arm-none-eabi-objcopy
GBAFIX := $(DEVKITPRO)/tools/bin/gbafix
LIBGBA_INC := $(DEVKITPRO)/libgba/include
LIBGBA_LIB := $(DEVKITPRO)/libgba/lib
GBA_SPECS := $(DEVKITARM)/arm-none-eabi/lib/gba.specs
CFLAGS := -O3 -flto -marm -mthumb-interwork -mcpu=arm7tdmi -mtune=arm7tdmi \
	-fomit-frame-pointer -ffast-math -fno-strict-aliasing \
	-ffunction-sections -fdata-sections -falign-functions=16 -falign-loops=16 \
	-Wall -Wextra -std=gnu99 -DNDEBUG \
	-I$(LIBGBA_INC) \
	-DPLAT_GBA
LDFLAGS := -flto -specs=$(GBA_SPECS) -Wl,--gc-sections -Wl,-Map=$(TARGET).map \
	-L$(LIBGBA_LIB)
LIBS := -lgba -lm
all: $(TARGET).gba
$(TARGET).elf: $(SRCS)
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS) $(LIBS)
$(TARGET).gba: $(TARGET).elf
	$(OBJCOPY) -O binary $< $@
	$(GBAFIX) $@
clean:
	rm -f $(TARGET).elf $(TARGET).gba $(TARGET).map
.PHONY: all clean
