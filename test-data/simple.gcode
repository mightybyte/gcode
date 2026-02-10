; Simple test file
G28 ; Home all axes
G90 ; Absolute positioning
M82 ; Extruder absolute
M140 S60 ; Set bed temp
M104 S200 ; Set hotend temp
M190 S60 ; Wait for bed
M109 S200 ; Wait for hotend
G1 Z5 F5000 ; Lift nozzle
G1 X0.1 Y20 Z0.3 F5000 ; Move to start
G1 X0.1 Y200 Z0.3 F1500 E15 ; Draw first line
G1 X0.4 Y200 Z0.3 F5000 ; Move to side
G1 X0.4 Y20 Z0.3 F1500 E30 ; Draw second line
G92 E0 ; Reset extruder
G1 Z2 F3000 ; Lift Z
M107 ; Fan off
M2 ; Program end
