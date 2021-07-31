# ProjectRed

A Minecraft Forge mod all about Redstone circuity.

| Release Branch         | MC Version | Status      |
| ---------------------- |:----------:| ----------- |
| `publish/1.16/release` | 1.16.5     | [![badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.16-release.json)](https://www.curseforge.com/minecraft/mc-mods/project-red-core) |
| `publish/1.16/beta`    | 1.16.5     | [![badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.16-beta.json)](https://www.curseforge.com/minecraft/mc-mods/project-red-core)    |
| `publish/1.15/beta`    | 1.15.2     | [![badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.15-beta.json)](https://www.curseforge.com/minecraft/mc-mods/project-red-core)    |

## Development Environment Setup

Project Red uses a standard Forge Gradle environment. Setup steps should be the same as most other mods:

1. Create a new folder and check out the repository:
   ```
   mkdir ~/projectred && cd ~/projectred
   git checkout https://github.com/MrTJP/ProjectRed.git .
   ```
2. Setup a new workspace with a decompiled copy of Minecraft:
   ```
   ./gradlew setupDecompWorkspace
   ```
3. You can now either set up Eclipse or IntelliJ Idea as your IDE.

   For Eclipse:
   * Generate an Eclipse project with `./gradlew eclipse`
   * Import the generated project with `File > Import > General > Existing Projects into Workspace` 
    
   For Intellij Idea:
   * Open IntelliJ and from the splash screen, select `Import Project`
   * Point IntelliJ to the `./build.gradle` file in the git repo
   * Close IntelliJ and run `./gradlew genIntellijRuns` to complete setup
   
4. If you'd like to contribute changes, you'll need to create your own fork of ProjectRed, push your changes, and then open a Pull Request for review.

### Building Locally
You can build a locally checked out copy of the repository by simply running:
```
./gradlew build
```
The built jars will be found in `./build/libs`.
