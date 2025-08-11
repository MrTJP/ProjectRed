# ProjectRed

A Minecraft Forge mod all about Redstone circuity.


## Latest Versions
| MC Version | Branch   | Latest Release                                                                                                                                                          | Latest Beta                                                                                                                                                          | Status      |
|:----------:|----------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------|
|   1.21.1   | `1.21.1` | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/638175d28f3bf15f4e5c0b964153a3ae/raw/projectred-badge-1.21.1-release.json) | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/638175d28f3bf15f4e5c0b964153a3ae/raw/projectred-badge-1.21.1-beta.json) | Active      |
|   1.20.4   | `1.20.4` | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.20.4-release.json) | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.20.4-beta.json) | Active      |
|   1.20.1   | `1.20.1` | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.20.1-release.json) | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.20.1-beta.json) | Active      |
|   1.19.2   | `1.19.x` | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.19.2-release.json) | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.19.2-beta.json) | End-of-Life |
|   1.18.2   | `1.18.x` | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.18-release.json)   | ![badge](https://img.shields.io/endpoint?logo=.&url=https://gist.githubusercontent.com/MrTJP/3ef501bc64c896a86fd706dfea8ba367/raw/projectred-badge-1.18-beta.json)   | End-of-Life |
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

## Contributing Translations

We welcome community translations to make ProjectRed accessible to players worldwide! This section provides guidance for translators who want to contribute language files to the project.

### Language File Structure
The English language files are auto-generated. They are located in each module's `generated` directory:
```
<module>/src/main/generated/assets/projectred_<module>/lang/en_us.json
```
Translations should be placed in the `resources` directory:
```
<module>/src/main/resources/assets/projectred_<module>/lang/
```

### Getting Started

1. **Find the English reference files**: English translations are auto-generated and located in:
   ```
   <module>/src/main/generated/assets/projectred_<module>/lang/en_us.json
   ```  

2. **Create your translation file**: Add your language file in the `resources` directory:
   ```
   <module>/src/main/resources/assets/projectred_<module>/lang/<your_language>.json
   ```
   Use standard Minecraft language codes (e.g., `de_de.json` for German, `fr_fr.json` for French, `es_es.json` for Spanish).

3. **Copy the structure**: Start by copying the English file and translating the values while keeping the keys unchanged.

### Translation Verification Tool

We provide a Python utility to help ensure your translations are complete and correctly formatted. It is recommended that
you use it as it is automatically run on every pull request to ensure translations are correct.

#### Requirements
- Python 3.10 or higher
- No additional dependencies required

#### Basic Usage

**Check all language files:**
```bash
python3 .github/verify_lang_files.py
```

**Check a specific module:**
```bash
python3 .github/verify_lang_files.py --submodule core
```

**Check a specific language:**
```bash
python3 .github/verify_lang_files.py --language de_de
```

**Get detailed output:**
```bash
python3 .github/verify_lang_files.py --verbose
```

#### Auto-Fix Mode

The tool can automatically fix common issues in your translation files:

```bash
python3 .github/verify_lang_files.py --fix
```

This will:
- Add missing translation keys (using English text as placeholder)
- Fix key ordering to match the English reference
- Correct JSON formatting issues

#### What the Tool Checks

✅ **Completeness**: Ensures all keys from English files are present  
✅ **Key Ordering**: Verifies keys are in the same order as English files  
✅ **JSON Format**: Validates proper JSON structure and formatting  
✅ **Data Quality**: Identifies empty values and potential duplicate translations

#### Understanding the Output

The tool provides categorized feedback:

- **❌ ERRORS**: Issues that must be fixed (missing keys, formatting problems)
- **⚠️ WARNINGS**: Potential issues to review (extra keys, empty values, duplicates)
- **🔧 FIXES APPLIED**: Automatic corrections made in fix mode

#### Example Workflow

1. Create or update your translation file
2. Run the verification tool:
   ```bash
   python3 .github/verify_lang_files.py --language your_lang --verbose
   ```
3. Fix any reported issues
4. Use auto-fix for common problems:
   ```bash
   python3 .github/verify_lang_files.py --language your_lang --fix
   ```
5. Verify everything is correct:
   ```bash
   python3 .github/verify_lang_files.py --language your_lang
   ```
6. Commit your changes and submit a pull request

### Translation Guidelines
- **Maintain formatting**: Don't change indentation, etc.
- **Be consistent**: Use consistent terminology throughout all modules
- **No minimum!**: Feel free to submit partial translations. Any contribution is appreciated, and you can always update it later.

Thank you for helping make ProjectRed accessible to the global Minecraft community! 🌍