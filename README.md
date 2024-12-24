# ProjectRed

A Minecraft Forge mod all about Redstone circuity.


## Latest Versions
| MC Version | Branch   | Latest Release                                                                                                                                                          | Latest Beta                                                                                                                                                          | Status      |
|:----------:|----------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------|
|   1.20.4   | `main`   | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.20.4-release.json) | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.20.4-beta.json) | Active      |
|   1.20.1   | `1.20.1` | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.20.1-release.json) | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.20.1-beta.json) | Active      |
|   1.19.2   | `1.19.x` | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.19-release.json)   | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.19-beta.json)   | Maintenance |
|   1.18.2   | `1.18.x` | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.18-release.json)   | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.18-beta.json)   | Maintenance |
|   1.16.5   | `1.16.x` | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.16-release.json)   | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.16-beta.json)   | End-of-Life |
|   1.15.2   | `1.15.x` | N/A                                                                                                                                                                     | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.15-beta.json)   | End-of-Life |

## Links
| Module       | Modrinth                                                                                                                                        | CurseForge                                                                                                                                                             |
|--------------|-------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Core         | [![Static Badge](https://img.shields.io/badge/Project_Red-Core-green?logo=modrinth)](https://modrinth.com/mod/project-red-core)                 | [![Static Badge](https://img.shields.io/badge/Project_Red-Core-orange?logo=curseforge)](https://www.curseforge.com/minecraft/mc-mods/project-red-core)                 |
| Expansion    | [![Static Badge](https://img.shields.io/badge/Project_Red-Expansion-green?logo=modrinth)](https://modrinth.com/mod/project-red-expansion)       | [![Static Badge](https://img.shields.io/badge/Project_Red-Expansion-orange?logo=curseforge)](https://www.curseforge.com/minecraft/mc-mods/project-red-expansion)       |
| Exploration  | [![Static Badge](https://img.shields.io/badge/Project_Red-Exploration-green?logo=modrinth)](https://modrinth.com/mod/project-red-exploration)   | [![Static Badge](https://img.shields.io/badge/Project_Red-Exploration-orange?logo=curseforge)](https://www.curseforge.com/minecraft/mc-mods/project-red-exploration)   |
| Fabrication  | [![Static Badge](https://img.shields.io/badge/Project_Red-Fabrication-green?logo=modrinth)](https://modrinth.com/mod/project-red-fabrication)   | [![Static Badge](https://img.shields.io/badge/Project_Red-Fabrication-orange?logo=curseforge)](https://www.curseforge.com/minecraft/mc-mods/project-red-fabrication)   |
| Illumination | [![Static Badge](https://img.shields.io/badge/Project_Red-Illumination-green?logo=modrinth)](https://modrinth.com/mod/project-red-illumination) | [![Static Badge](https://img.shields.io/badge/Project_Red-Illumination-orange?logo=curseforge)](https://www.curseforge.com/minecraft/mc-mods/project-red-illumination) |
| Integration  | [![Static Badge](https://img.shields.io/badge/Project_Red-Integration-green?logo=modrinth)](https://modrinth.com/mod/project-red-integration)   | [![Static Badge](https://img.shields.io/badge/Project_Red-Integration-orange?logo=curseforge)](https://www.curseforge.com/minecraft/mc-mods/project-red-integration)   |
| Transmission | [![Static Badge](https://img.shields.io/badge/Project_Red-Transmission-green?logo=modrinth)](https://modrinth.com/mod/project-red-transmission) | [![Static Badge](https://img.shields.io/badge/Project_Red-Transmission-orange?logo=curseforge)](https://www.curseforge.com/minecraft/mc-mods/project-red-transmission) |

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

## ProjectRed API

### Setup
ProjectRed API can be consumed by your ForgeGradle project by adding the following to your `build.gradle`:
```groovy
// Add repository
repositories {
    maven { url = "https://proxy-maven.covers1624.net/" }
}

// Add dependency (replace ${mc_version} and ${pr_version} with the desired versions)
dependencies {
    /* minecraft dependency is here */
    
    // compile against the API but do not include it at runtime
    compileOnly fg.deobf("mrtjp:ProjectRed-${mc_version}:${pr_version}:api")
    
    // at runtime, use the full mod jars of the required modules
    runtimeOnly fg.deobf("mrtjp:ProjectRed-${mc_version}:${pr_version}:core")
    runtimeOnly fg.deobf("mrtjp:ProjectRed-${mc_version}:${pr_version}:integration")
    runtimeOnly fg.deobf("mrtjp:ProjectRed-${mc_version}:${pr_version}:transmission")
    // etc...
}
```

### Usage
The primary entrypoint into the API is `mrtjp.projectred.api.ProjectRedAPI` where you will find static fields for each module of ProjectRed. If a module is installed, its corresponding field will be a non-null implementor of that module's API. See the javadocs for more information.