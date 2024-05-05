# ProjectRed

A Minecraft Forge mod all about Redstone circuity.

| MC Version | Branch      | Latest Release                                                                                                                                                                                                                  | Latest Beta                                                                                                                                                                                                                  | Status      |
|:----------:|-------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------|
|   1.19.2   | `main`      | [![badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.19-release.json)](https://www.curseforge.com/minecraft/mc-mods/project-red-core) | [![badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.19-beta.json)](https://www.curseforge.com/minecraft/mc-mods/project-red-core) | Active      |
|   1.18.2   | `main/1.18` | [![badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.18-release.json)](https://www.curseforge.com/minecraft/mc-mods/project-red-core) | [![badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.18-beta.json)](https://www.curseforge.com/minecraft/mc-mods/project-red-core) | Maintenance |
|   1.16.5   | `main/1.16` | [![badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.16-release.json)](https://www.curseforge.com/minecraft/mc-mods/project-red-core) | [![badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.16-beta.json)](https://www.curseforge.com/minecraft/mc-mods/project-red-core) | End-of-Life |
|   1.15.2   | `main/1.15` | N/A                                                                                                                                                                                                                             | [![badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.15-beta.json)](https://www.curseforge.com/minecraft/mc-mods/project-red-core) | End-of-Life |

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