
GHCI version 8.8.4 
___________________
required 
stack-2.5.1-windows
VSCodeUserSetup
___________________
Windows
If you get this error on startup:

user error (unknown GLUT entry glutInit)

Then this means you need the freeglut MSVC binaries which you can get from here https://www.transmissionzero.co.uk/software/freeglut-devel/ .

Extract freeglut\bin\x64\freeglut.dll to the same location as the executable you wish to run, or place it in a folder that can be discovered by your %PATH% variable. 
(Here are some steps on how to add a new folder to your %PATH%.)
https://docs.alfresco.com/4.2/tasks/fot-addpath.html
