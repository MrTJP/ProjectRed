ProjectRed
==========

A RP2 replacement


Big thanks to immibis, who allows people to freely use his code.  About 80% of the wiring code
in this mod is from Immibis's RedLogic.


Setting up the dev environment:
1) Set up your folder structure like this:
***
  projred
	\-ProjectRed
		\build.xml
    \common
    \resources
    \dummy
    \etc.
***
2) CD to ProjectRed folder.
3) Run this command:
***
 ant fullfilesetup
***
4) After its done, everything should be set up. 
5) Open eclipse and link the following sources:
***
  projred
  \-ProjectRed
    \common
    \resources
    \dummy
***
6) To create a test release, run:
***
 ant testbuild
***
This will use your dev environment to reobfuscate, then restore the state back to development.

7) The finished jar will be in:
***
  projred
  \-ProjectRed
    \build
      \dist
***
