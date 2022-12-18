TARGET = cptester
AC = dasm
EMULOAD = x64 -autoload
EMURUN = x64 -autostart

default: $(TARGET)

all: default

$(TARGET):
	$(AC) $@.asm -o$@.prg

clean:
	-rm -f $(TARGET).prg
load: default
	$(EMULOAD) $(TARGET).prg
run: default
	$(EMURUN) $(TARGET).prg
