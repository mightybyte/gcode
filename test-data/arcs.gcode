; Arc test file
G21 ; Millimeters
G90 ; Absolute
G28 ; Home
G1 X50 Y50 Z0.2 F3000
G2 X60 Y60 I10 J0 F1000 ; Clockwise arc
G3 X50 Y50 I-10 J0 F1000 ; Counter-clockwise arc
G2 X70 Y50 R10 F1000 ; Arc with radius
M2
