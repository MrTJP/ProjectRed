#!/usr/bin/env python3
"""
Language File Verification Script

This script verifies language files in submodules for:
1. Completeness - all keys from English files are present
2. Ordering - keys are in the same order as English files
3. Format validation - proper JSON structure and formatting
"""

import argparse
import json
import os
import sys
from pathlib import Path
from typing import Dict, List


class LanguageVerifier:
    def __init__(self, project_root: str, fix_mode: bool = False, verbose: bool = False):
        self.project_root = Path(project_root)
        self.fix_mode = fix_mode
        self.verbose = verbose
        self.errors = []
        self.warnings = []
        self.fixes_applied = []

        # Categorized issue tracking
        self.issue_stats = {
            'missing_keys': 0,
            'extra_keys': 0,
            'key_ordering': 0,
            'json_structure': 0,
            'empty_values': 0,
            'duplicate_values': 0,
            'file_formatting': 0
        }

    def discover_submodules(self) -> List[str]:
        """Automatically discover submodules by looking for English language files in assets directories."""
        submodules = set()

        # Search more efficiently by targeting likely locations first
        search_patterns = [
            "*/src/main/generated/assets/*/lang/en_us.json",  # Generated files
            "*/src/main/resources/assets/*/lang/en_us.json",  # Resource files
            "src/*/src/main/generated/assets/*/lang/en_us.json",  # Nested projects
            "src/*/src/main/resources/assets/*/lang/en_us.json"   # Nested projects
        ]

        for pattern in search_patterns:
            for english_file in self.project_root.glob(pattern):
                # Extract the asset directory name (parent of lang directory)
                asset_dir_name = english_file.parent.parent.name
                submodules.add(asset_dir_name)

        return sorted(list(submodules))

    def find_english_file(self, submodule_asset_name: str) -> Path:
        """Find the English reference file for a submodule asset directory."""
        # Search in order of preference: generated first, then resources
        search_patterns = [
            f"*/src/main/generated/assets/{submodule_asset_name}/lang/en_us.json",
            f"*/src/main/resources/assets/{submodule_asset_name}/lang/en_us.json",
            f"src/*/src/main/generated/assets/{submodule_asset_name}/lang/en_us.json",
            f"src/*/src/main/resources/assets/{submodule_asset_name}/lang/en_us.json"
        ]

        for pattern in search_patterns:
            for english_file in self.project_root.glob(pattern):
                return english_file

        # Fallback - shouldn't happen if discovery worked correctly
        raise FileNotFoundError(f"No English file found for {submodule_asset_name}")

    def find_resources_lang_dir(self, submodule_asset_name: str) -> Path:
        """Find the resources lang directory for manual translations."""
        # Search specifically for resources directories (not generated)
        search_patterns = [
            f"*/src/main/resources/assets/{submodule_asset_name}/lang",
            f"src/*/src/main/resources/assets/{submodule_asset_name}/lang"
        ]

        for pattern in search_patterns:
            for lang_dir in self.project_root.glob(pattern):
                return lang_dir

        # If not found, find any resources directory and construct the path
        for resources_path in self.project_root.glob("*/src/main/resources"):
            return resources_path / "assets" / submodule_asset_name / "lang"

        for resources_path in self.project_root.glob("src/*/src/main/resources"):
            return resources_path / "assets" / submodule_asset_name / "lang"

        # Ultimate fallback
        raise FileNotFoundError(f"No resources directory found for {submodule_asset_name}")

    def log_error(self, message: str, category: str = 'general'):
        """Log an error message with category."""
        self.errors.append(message)
        if category in self.issue_stats:
            self.issue_stats[category] += 1
        if self.verbose:
            print(f"    ERROR: {message}")

    def log_warning(self, message: str, category: str = 'general'):
        """Log a warning message with category."""
        self.warnings.append(message)
        if category in self.issue_stats:
            self.issue_stats[category] += 1
        if self.verbose:
            print(f"    WARNING: {message}")

    def log_fix(self, message: str):
        """Log a fix that was applied."""
        self.fixes_applied.append(message)
        if self.verbose:
            print(f"    FIXED: {message}")

    def load_json_file(self, file_path: Path) -> Dict[str, str]:
        """Load a JSON file and return its contents."""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read().strip()
                if not content:
                    return {}
                return json.loads(content)
        except json.JSONDecodeError as e:
            self.log_error(f"Invalid JSON in {file_path}: {e}")
            return {}
        except FileNotFoundError:
            return {}
        except Exception as e:
            self.log_error(f"Error loading {file_path}: {e}")
            return {}

    def save_json_file(self, file_path: Path, data: Dict[str, str]):
        """Save data to a JSON file with proper formatting."""
        try:
            os.makedirs(file_path.parent, exist_ok=True)
            with open(file_path, 'w', encoding='utf-8') as f:
                if data:
                    json.dump(data, f, ensure_ascii=False, indent=4, separators=(',', ': '))
                else:
                    f.write('{}')
                f.write('\n')  # Ensure file ends with newline
        except Exception as e:
            self.log_error(f"Error saving {file_path}: {e}")

    def get_english_file_path(self, submodule: str) -> Path:
        """Get the path to the English language file for a submodule."""
        return self.find_english_file(submodule)

    def get_language_file_path(self, submodule: str, language: str) -> Path:
        """Get the path to a specific language file for a submodule."""
        lang_dir = self.find_resources_lang_dir(submodule)
        return lang_dir / f"{language}.json"

    def get_all_language_files(self, submodule: str) -> List[Path]:
        """Get all language files for a submodule (excluding English)."""
        try:
            lang_dir = self.find_resources_lang_dir(submodule)
            if not lang_dir.exists():
                return []

            files = []
            for file_path in lang_dir.glob("*.json"):
                if file_path.name != "en_us.json":  # Skip English files
                    files.append(file_path)
            return files
        except FileNotFoundError:
            return []

    def format_key_list(self, keys: List[str], max_display: int = 10) -> str:
        """Format a list of keys for clean display with proper indentation."""
        if not keys:
            return ""

        if len(keys) <= max_display:
            # Show all keys, one per line
            formatted_keys = '\n'.join(f"      - {key}" for key in keys)
            return f"\n{formatted_keys}"
        else:
            # Show first few keys, then indicate how many more
            displayed_keys = keys[:max_display]
            formatted_keys = '\n'.join(f"      - {key}" for key in displayed_keys)
            remaining = len(keys) - max_display
            return f"\n{formatted_keys}\n      ... and {remaining} more"

    def format_file_reference(self, lang_file: Path, submodule: str) -> str:
        """Format a file reference to show just module and filename."""
        return f"{submodule}/{lang_file.name}"

    def verify_submodule_languages(self, submodule: str, target_language: str = None) -> bool:
        """Verify all language files for a specific submodule."""
        if self.verbose:
            print(f"\nVerifying submodule: {submodule}")

        # Load the English reference file
        english_path = self.get_english_file_path(submodule)
        if not english_path.exists():
            self.log_warning(f"No English file found for {submodule}: {english_path}")
            return True  # Not an error if submodule has no generated English file

        english_data = self.load_json_file(english_path)
        if not english_data:
            self.log_warning(f"Empty or invalid English file for {submodule}")
            return True

        english_keys = list(english_data.keys())
        all_passed = True

        # Get language files to check
        if target_language:
            lang_files = [self.get_language_file_path(submodule, target_language)]
        else:
            lang_files = self.get_all_language_files(submodule)

        for lang_file in lang_files:
            if target_language and not lang_file.exists():
                continue  # Skip non-existent specific language file

            if self.verbose:
                print(f"  Checking: {lang_file.name}")

            passed = self.verify_language_file(lang_file, english_keys, english_data, submodule)
            all_passed = all_passed and passed

        return all_passed

    def verify_language_file(self, lang_file: Path, english_keys: List[str], english_data: Dict[str, str], submodule: str) -> bool:
        """Verify a single language file against the English reference."""
        lang_data = self.load_json_file(lang_file)
        original_data = lang_data.copy()

        # Format file reference for cleaner output
        file_ref = self.format_file_reference(lang_file, submodule)

        file_modified = False
        all_passed = True
        initial_error_count = len(self.errors)  # Track errors before processing this file

        # Check for missing keys
        missing_keys = []
        for key in english_keys:
            if key not in lang_data:
                missing_keys.append(key)

        if missing_keys:
            all_passed = False
            if self.fix_mode:
                # In fix mode, don't log as error if we can fix it - just apply the fix
                for key in missing_keys:
                    lang_data[key] = english_data[key]
                    self.log_fix(f"Added missing key to {file_ref}: {key}")
                file_modified = True
                # Track stats but don't add to errors since we fixed it
                self.issue_stats['missing_keys'] += len(missing_keys)
            else:
                # In read-only mode, log as error
                self.log_error(f"Missing {len(missing_keys)} keys in {file_ref}:{self.format_key_list(missing_keys)}", category='missing_keys')

        # Check for extra keys (keys not in English file)
        extra_keys = []
        for key in lang_data:
            if key not in english_keys:
                extra_keys.append(key)

        if extra_keys:
            self.log_warning(f"Extra keys in {file_ref} (not in English):{self.format_key_list(extra_keys)}", category='extra_keys')

        # Check key ordering
        current_keys = list(lang_data.keys())
        # Filter current keys to only include those that exist in English (for ordering comparison)
        existing_english_keys = [key for key in english_keys if key in lang_data]
        current_english_keys = [key for key in current_keys if key in english_keys]

        if existing_english_keys != current_english_keys:
            all_passed = False
            if self.fix_mode:
                # In fix mode, don't log as error if we can fix it - just apply the fix
                # Reorder the data to match English file ordering
                ordered_data = {}
                # First add all keys in English order
                for key in english_keys:
                    if key in lang_data:
                        ordered_data[key] = lang_data[key]
                # Then add any extra keys at the end
                for key in extra_keys:
                    ordered_data[key] = lang_data[key]

                lang_data = ordered_data
                self.log_fix(f"Fixed key ordering in {file_ref}")
                file_modified = True
                # Track stats but don't add to errors since we fixed it
                self.issue_stats['key_ordering'] += 1
            else:
                # In read-only mode, log as error
                self.log_error(f"Key ordering mismatch in {file_ref}", category='key_ordering')

        # Check for empty values
        empty_values = [key for key, value in lang_data.items() if not value.strip()]
        if empty_values:
            self.log_warning(f"Empty values in {file_ref}:{self.format_key_list(empty_values)}", category='empty_values')

        # Check for duplicate values (potential copy-paste errors)
        value_counts = {}
        for key, value in lang_data.items():
            if value in value_counts:
                value_counts[value].append(key)
            else:
                value_counts[value] = [key]

        duplicates = {value: keys for value, keys in value_counts.items() if len(keys) > 1 and value.strip()}
        if duplicates:
            for value, keys in list(duplicates.items())[:3]:  # Show only first 3
                self.log_warning(f"Duplicate value '{value}' in {file_ref}: {keys}", category='duplicate_values')

        # Save file if modified
        if file_modified or lang_data != original_data:
            if self.fix_mode:
                self.save_json_file(lang_file, lang_data)
                # In fix mode, if we successfully saved, we've resolved all fixable issues for this file
                if file_modified:
                    all_passed = True  # Override since we fixed the issues
            else:
                all_passed = False
                if not file_modified:  # Only log if not already logged above
                    self.log_error(f"File needs formatting fixes: {file_ref}", category='file_formatting')

        return all_passed

    def validate_json_structure(self, file_path: Path) -> bool:
        """Validate that a file has proper JSON structure."""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()

            # Check for common JSON issues
            if not content.strip():
                return True  # Empty file is okay

            if not content.strip().startswith('{'):
                self.log_error(f"JSON file should start with '{{': {file_path}", category='json_structure')
                return False

            if not content.strip().endswith('}'):
                self.log_error(f"JSON file should end with '}}': {file_path}", category='json_structure')
                return False

            # Try to parse
            json.loads(content)
            return True

        except json.JSONDecodeError as e:
            self.log_error(f"Invalid JSON structure in {file_path}: {e}", category='json_structure')
            return False
        except Exception as e:
            self.log_error(f"Error validating {file_path}: {e}")
            return False

    def run_verification(self, target_submodule: str = None, target_language: str = None) -> bool:
        """Run the full verification process."""
        print("Language File Verification")
        print("=" * 40)

        if self.fix_mode:
            print("Running in EDIT mode - issues will be automatically fixed")
        else:
            print("Running in READ-ONLY mode - will report issues only")

        if target_submodule:
            submodules_to_check = [target_submodule]
        else:
            # Discover submodules if not specified
            submodules_to_check = self.discover_submodules()
            if not submodules_to_check:
                print("Warning: No submodules with generated language files found")
                return False
            if self.verbose:
                print(f"Discovered submodules: {', '.join(submodules_to_check)}")

        all_passed = True
        for submodule in submodules_to_check:
            passed = self.verify_submodule_languages(submodule, target_language)
            all_passed = all_passed and passed

        # Print summary
        print("\n" + "=" * 40)
        print("VERIFICATION SUMMARY")
        print("=" * 40)

        if self.errors:
            print(f"❌ ERRORS: {len(self.errors)}")
            if not self.verbose:
                for error in self.errors[:10]:  # Show first 10 errors
                    print(f"   {error}")
                if len(self.errors) > 10:
                    print(f"   ... and {len(self.errors) - 10} more errors")

        if self.warnings:
            print(f"⚠️  WARNINGS: {len(self.warnings)}")
            if not self.verbose:
                for warning in self.warnings[:5]:  # Show first 5 warnings
                    print(f"   {warning}")
                if len(self.warnings) > 5:
                    print(f"   ... and {len(self.warnings) - 5} more warnings")

        if self.fixes_applied:
            print(f"🔧 FIXES APPLIED: {len(self.fixes_applied)}")
            if not self.verbose:
                for fix in self.fixes_applied[:5]:  # Show first 5 fixes
                    print(f"   {fix}")
                if len(self.fixes_applied) > 5:
                    print(f"   ... and {len(self.fixes_applied) - 5} more fixes")

        # Print categorized issue statistics
        print("\n" + "=" * 40)
        print("ISSUE STATISTICS")
        print("=" * 40)
        for category, count in self.issue_stats.items():
            if count > 0:
                print(f"  {category.replace('_', ' ').title()}: {count}")

        # In fix mode, if we applied fixes, consider it successful regardless of initial errors
        # In read-only mode, success depends on having no errors
        if self.fix_mode:
            # Fix mode: success if we either had no issues or successfully applied fixes
            if all_passed and not self.errors:
                print("✅ All language files are valid!")
                return True
            elif self.fixes_applied and not self.errors:
                print("✅ All issues have been automatically fixed!")
                return True
            elif self.fixes_applied:
                print("🔧 Some issues were fixed, but errors remain that require manual attention.")
                return False
            else:
                print("❌ Language file verification failed!")
                return False
        else:
            # Read-only mode: success only if no errors were found
            if all_passed and not self.errors:
                print("✅ All language files are valid!")
                return True
            else:
                print("❌ Language file verification failed!")
                return False

def main():
    # Create a temporary verifier to discover available submodules for argument validation
    temp_project_root = Path('.').resolve()
    if temp_project_root.name != "ProjectRed":
        temp_project_root = temp_project_root / "src" / "ProjectRed"

    temp_verifier = LanguageVerifier(temp_project_root)
    available_submodules = temp_verifier.discover_submodules()

    parser = argparse.ArgumentParser(
        description="Verify language files for completeness and consistency",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python3 verify_lang_files.py
  python3 verify_lang_files.py --fix
  python3 verify_lang_files.py --submodule core --language de_de
  python3 verify_lang_files.py --fix --verbose
        """
    )

    parser.add_argument('--fix', action='store_true',
                        help='Enable fix mode to automatically fix issues')
    parser.add_argument('--submodule', choices=available_submodules if available_submodules else None,
                        help=f'Only verify specific submodule. Available: {", ".join(available_submodules) if available_submodules else "auto-detected"}')
    parser.add_argument('--language',
                        help='Only verify specific language (e.g., de_de, fr_fr)')
    parser.add_argument('--verbose', action='store_true',
                        help='Show detailed output')
    parser.add_argument('--project-root', default='.',
                        help='Path to project root directory (default: current directory)')

    args = parser.parse_args()

    # Validate project root
    project_root = Path(args.project_root).resolve()
    if not project_root.exists():
        print(f"Error: Project root does not exist: {project_root}")
        sys.exit(1)

    # Run verification
    verifier = LanguageVerifier(project_root, args.fix, args.verbose)
    success = verifier.run_verification(args.submodule, args.language)

    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()
