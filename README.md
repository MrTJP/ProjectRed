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
`ant fullfilesetup`
***
4) After its done, download the following files to the MCP lib directory, then link the libraries in eclipse (Rightclick on `Minecraft` project, select `buildpath`, and under `libraries` tab click `add external jar`.
***
     projred
     \-build
       \-forge
         \-mcp
           \-lib
             \CodeChickenCore-dev x.x.x.x.jar
             \NotEnoughItems-dev x.x.x.x.jar
             \scala-compiler.jar
             \CodeChickenLib-dev-1.6.2-x.x.x.x.jar
             \ForgeMultipart-dev-1.6.2-x.x.x.x.jar
***

5) Also copy all those files to the mods directory in mcp:
***
     projred
     \-build
       \-forge
         \-mcp
           \-jars
             \-mods
               \CodeChickenCore-dev x.x.x.x.jar
               \NotEnoughItems-dev x.x.x.x.jar
               \scala-compiler.jar
               \CodeChickenLib-dev-1.6.2-x.x.x.x.jar
               \ForgeMultipart-dev-1.6.2-x.x.x.x.jar
***

6) Open eclipse and link the following sources:
***
     projred
     \-ProjectRed
       \common
       \resources
       \dummy
***

7) To create a test release, run:
***
 `ant testbuild`
***

This will use your dev environment to reobfuscate, then restore the state back to development.

8) The finished jar will be in:
***
     projred
     \-build
       \dist
***
