# ProjectRed

A Minecraft Forge mod all about Redstone circuity.

| Release Branch           | MC Version | Status                                                                                                                                                                                                                            |
|--------------------------|:----------:|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `publish/1.18.2/release` |   1.18.2   | [![badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.18.2-release.json)](https://www.curseforge.com/minecraft/mc-mods/project-red-core) |
| `publish/1.18.2/beta`    |   1.18.2   | [![badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.18.2-beta.json)](https://www.curseforge.com/minecraft/mc-mods/project-red-core)    |
| `publish/1.16/release`   |   1.16.5   | [![badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.16-release.json)](https://www.curseforge.com/minecraft/mc-mods/project-red-core)   |
| `publish/1.16/beta`      |   1.16.5   | [![badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.16-beta.json)](https://www.curseforge.com/minecraft/mc-mods/project-red-core)      |
| `publish/1.15/beta`      |   1.15.2   | [![badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.15-beta.json)](https://www.curseforge.com/minecraft/mc-mods/project-red-core)      |

## Development Environment Setup

Project Red uses a standard Forge Gradle environment. Setup steps should be the same as most other mods. Instructions below are for Intellij IDEA, but the process is very similar for both Eclipse and VSCode. See [Forge Gradle](https://docs.minecraftforge.net/en/fg-5.x/gettingstarted/#setting-up-forgegradle) docs for more info.

1. Create a new folder and check out the repository:
   ```
   mkdir ~/projectred && cd ~/projectred
   git checkout https://github.com/MrTJP/ProjectRed.git .
   ```

2. Build the project: 
   ```
   ./gradlew build
   ```   

3. Generate run configurations:
   ```
   ./gradlew genIntellijRuns
   ```
   
4. Open the project directory in Intellij IDEA and give it a few minutes to set up and index the Gradle project.

### Building Locally
You can re-build jar files locally by running `./gradlew build`. The jars can be found in `./<module>/build/libs`.
