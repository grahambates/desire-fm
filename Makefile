name=desirefm
program=out/a

# Emulator options
BUILD=hunk
DEBUG=0
MODEL=A500
FASTMEM=0
CHIPMEM=512
SLOWMEM=512

ITERATIONS=9
EFFORT=2000

BIN_DIR = ~/amiga/bin

# Binaries
CC = m68k-amiga-elf-gcc
ELF2HUNK = elf2hunk
EXE2ADF = $(BIN_DIR)/exe2adf
VASM = $(BIN_DIR)/vasmm68k_mot
VLINK = $(BIN_DIR)/vlink
KINGCON = $(BIN_DIR)/kingcon
AMIGECONV = $(BIN_DIR)/amigeconv
SHRINKLER = $(BIN_DIR)/Shrinkler
AMIGATOOLS = ~/.nvm/versions/node/v16.17.0/bin/amigatools
FSUAE = /Applications/FS-UAE-3.app/Contents/MacOS/fs-uae
VAMIGA = /Applications/vAmiga.app/Contents/MacOS/vAmiga

# Flags:
VASMFLAGS = -m68000 -opt-fconst -nowarn=62 -x -DDEBUG=$(DEBUG)
VLINKFLAGS = -bamigahunk -Bstatic
CCFLAGS = -g -MP -MMD -m68000 -Ofast -nostdlib -Wextra -fomit-frame-pointer -fno-tree-loop-distribution -flto -fwhole-program
LDFLAGS = -Wl,--emit-relocs,-Ttext=0
FSUAEFLAGS = --floppy_drive_0_sounds=off --video_sync=1 --automatic_input_grab=0  --chip_memory=$(CHIPMEM) --fast_memory=$(FASTMEM) --slow_memory=$(SLOWMEM) --amiga_model=$(MODEL)
SHRINKLER_FLAGS = -i $(ITERATIONS) -e $(EFFORT) --length-margin 20 -T decrunch.txt
EXE2ADF_FLAGS = -p 112,dff,569

TEX_SRC=assets/tiles-dither.png
TEX_SHIFT_SRC=assets/tiles-shift.png

exe = out/$(name).$(BUILD).exe
dist_exe = dist/$(name)
dist_adf = dist/$(name).adf
sources := _main.asm $(wildcard */*.asm)
elf_objects := $(sources:.asm=.elf)
hunk_objects := $(sources:.asm=.hunk)
deps := $(sources:.asm=.d)

data = data/bg.BPL \
	data/radio-logo.BPL \
	data/radio-logo.chk \
	data/radio-logo.PAL \
	data/radio-numbers.BPL \
	data/marker-1.SPR \
	data/marker-2.SPR \
	data/marker-3.SPR \
	data/marker-4.SPR \
	data/marker-5.SPR \
	data/scope-bg.BPL \
	data/scope-text.SPR \
	data/scope-l.SPR \
	data/scope-r.SPR \
	data/scope-controls.SPR \
	data/scope-light.SPR \
	data/scope-text.BPL \
	data/city-text-1.BPL \
	data/city-text-2.BPL \
	data/cred-1.SPR \
	data/cred-2.SPR \
	data/cred-3.SPR \
	data/graff-font.BPL \
	data/desire3.chk \
	data/boombox-small.SPR \
	data/boombox-small-alt.SPR \
	data/dipole.BPL

all: $(exe)

dist: $(dist_adf)

run: $(exe)
	$(info Cleaning...)
	cp $< $(program).exe
	$(FSUAE) $(FSUAEFLAGS) --hard_drive_0=./out

run-dist: $(dist_exe)
	cp $< $(program).exe
	$(FSUAE) $(FSUAEFLAGS) --hard_drive_0=./out

run-adf: $(dist_adf)
	$(FSUAE) $(FSUAEFLAGS) $<

run-vamiga: $(exe)
	$(VAMIGA) $<

run-vamiga-dist: $(dist_exe)
	$(VAMIGA) $<

run-vamiga-adf: $(dist_adf)
	$(VAMIGA) $<

clean:
	$(info Cleaning...)
	@$(RM) $(elf_objects) $(hunk_objects) $(deps) $(dist_exe) $(dist_adf) out/*.*

.PHONY: all clean dist run run-dist run-adf run-vamiga run-vamiga-dist run-vamiga-adf


$(dist_exe): $(exe) decrunch.txt
	$(info Shrinkling exe)
	$(SHRINKLER) $(SHRINKLER_FLAGS) $< $@

$(dist_adf): $(dist_exe) Makefile
	$(info )
	$(info Creating ADF)
	$(EXE2ADF) -i $(dist_exe) -a $@ $(EXE2ADF_FLAGS)

# BUILD=hunk (vasm/vlink)
out/$(name).hunk.exe: $(hunk_objects) out/$(name).hunk-debug.exe
	$(info )
	$(info Linking (stripped) $@)
	@$(VLINK) $(VLINKFLAGS) -S $(hunk_objects) -o $@
	cp $@ $(program).exe
out/$(name).hunk-debug.exe: $(hunk_objects)
	$(info )
	$(info Linking $@)
	@$(VLINK) $(VLINKFLAGS) $(hunk_objects) -o $@
%.hunk : %.asm $(data)
	$(info )
	$(info Assembling $@)
	@$(VASM) $(VASMFLAGS) -Fhunk -linedebug -o $@ $(CURDIR)/$<

# BUILD=elf (GCC/Bartman)
out/$(name).elf.exe: $(program).elf
	$(info )
	$(info Elf2Hunk $@)
	@$(ELF2HUNK) $< $@ -s
	cp $@ $(program).exe
$(program).elf: $(elf_objects)
	$(info )
	$(info Linking $@)
	$(CC) $(CCFLAGS) $(LDFLAGS) $(elf_objects) -o $@
%.elf : %.asm $(data)
	$(info )
	$(info Assembling $<)
	@$(VASM) $(VASMFLAGS) -Felf -dwarf=3 -o $@ $(CURDIR)/$<

-include $(deps)

%.d : %.asm
	$(info Building dependencies for $<)
	$(VASM) $(VASMFLAGS) -quiet -dependall=make -o "$(patsubst %.d,%.\$$(BUILD),$@)" $(CURDIR)/$< > $@


#-------------------------------------------------------------------------------
# Data:
#-------------------------------------------------------------------------------

data/P61.tune: p61conv/assets/P61.tune
	cp $< $@

data/table.i: scripts/uvtbls/table-data-2.js
	node $< > $@

data/gen.i: scripts/uvtbls/table.js
	node $< > $@

data/head.i: assets/head.obj scripts/obj2asm/index.mjs
	scripts/obj2asm/index.mjs $< > $@ 110 1

data/hand.i: assets/hand.obj scripts/obj2asm/index.mjs
	scripts/obj2asm/index.mjs $< > $@ 120

data/checkertbls.i: scripts/checkertbls.js
	node $< > $@


#-------------------------------------------------------------------------------
# Images:
#-------------------------------------------------------------------------------

# Intro

data/radio-logo.BPL: assets/radio-logo-2.png
	$(KINGCON) $< data/radio-logo -F=3 -RP
data/radio-logo.chk: assets/radio-logo-2.png
	$(AMIGECONV) -f chunky $< $@

data/radio-numbers.BPL: assets/radio-numbers.png
	$(KINGCON) $< data/radio-numbers -F=1

data/marker-1.SPR: assets/marker-1.png
	$(KINGCON) $< data/marker-1 -F=s16
data/marker-2.SPR: assets/marker-2.png
	$(KINGCON) $< data/marker-2 -F=s16
data/marker-3.SPR: assets/marker-3.png
	$(KINGCON) $< data/marker-3 -F=s16
data/marker-4.SPR: assets/marker-4.png
	$(KINGCON) $< data/marker-4 -F=s16
data/marker-5.SPR: assets/marker-5.png
	$(KINGCON) $< data/marker-5 -F=s16

# UV Table

data/texture.chk: $(TEX_SRC)
	$(AMIGECONV) -f chunky $< $@
data/texture.pal: $(TEX_SRC)
	$(AMIGECONV) -f palette -p pal4 $< $@
data/texture-shift.pal: $(TEX_SHIFT_SRC)
	$(AMIGECONV) -f palette -p pal4 $< $@
data/circle.bpl: assets/circle.png
	$(AMIGECONV) -f bitplane -d 1 $< $@

data/cred-1.SPR: assets/cred-1.png
	$(KINGCON) $< data/cred-1 -F=s16
data/cred-2.SPR: assets/cred-2.png
	$(KINGCON) $< data/cred-2 -F=s16
data/cred-3.SPR: assets/cred-3.png
	$(KINGCON) $< data/cred-3 -F=s16
data/font-8.BPL: assets/font-8.png
	$(KINGCON) $< data/font-8 -F=1
data/font-16x16.BPL: assets/font-16x16.png
	$(KINGCON) $< data/font-16x16 -F=1

# Dots:
data/scope-bg.BPL: assets/scope-bg.png
	$(KINGCON) $< data/scope-bg -F=1
data/scope-text.SPR: assets/scope-text.png
	$(KINGCON) $< data/scope-text  -F=s16 -SX=240 -SY=58
data/scope-l.SPR: assets/scope-l.png
	$(KINGCON) $< data/scope-l -F=s16 -SX=144 -SY=44
data/scope-r.SPR: assets/scope-r.png
	$(KINGCON) $< data/scope-r -F=s16 -SX=400 -SY=44
data/scope-controls.SPR: assets/scope-controls.png
	$(KINGCON) $< data/scope-controls -F=s16 -SX=416 -SY=44
data/scope-light.SPR: assets/scope-light.png
	$(KINGCON) $< data/scope-light -F=s16 -SX=416 -SY=69

# City

data/skyline.BPL: assets/skyline.png
	$(KINGCON) $< data/skyline -F=1
data/landscape.BPL: assets/landscape.png
	$(KINGCON) $< data/landscape -F=1
data/city-text-1.BPL: assets/city-text-1.png
	$(KINGCON) $< data/city-text-1 -F=1
data/city-text-2.BPL: assets/city-text-2.png
	$(KINGCON) $< data/city-text-2 -F=1

# Rotozoom

data/tapes.chk: assets/tapes3.png
	$(AMIGECONV) -f chunky $< $@
data/tapes.pal: assets/tapes3.png
	$(AMIGECONV) -f palette -p pal4 $< $@

# Logo

data/desire3.chk: assets/desire3.png
	$(AMIGECONV) -f chunky $< $@
data/dipole.BPL: assets/dipole.png
	$(KINGCON) $< data/dipole -F=2 -C


# graff

data/graff-font.BPL: assets/graff-font.png
	$(KINGCON) $< data/graff-font -F=2 -I
data/boombox-small.SPR: assets/boombox-small3.png
	$(KINGCON) $< data/boombox-small -F=s16 -SX=352 -SY=244
data/boombox-small-alt.SPR: assets/boombox-small3-alt.png
	$(KINGCON) $< data/boombox-small-alt -F=s16 -SX=352 -SY=244
