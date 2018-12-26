# cptester
Control Port tester for Commodore C64 (6502 assembler)

Only for digital pins - you can check the ports or joystick :)

Sources in dasm format, compile command:
dasm cptester.asm -ocptester.prg

Starting the program in the VICE emulator:
x64 -autoload cptester.prg
