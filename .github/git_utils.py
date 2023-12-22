import fire
import subprocess
import re
import logging

# Logging
fmt_str = '[%(asctime)s] %(levelname)s %(lineno)d: %(message)s'
logging.basicConfig(level=logging.INFO, format=fmt_str)
log = logging.getLogger(__name__)

# Regex patterns

# Extracts commit info from lines obtained from git log --oneline --no-decorate
# Epected to be in format: "<hash> <message>", where message is a Conventional Commit compliant format
# https://regex101.com/r/AXhPCV/1
commit_header_extractor = re.compile(r"^(?P<hash>[0-9a-fA-f]+)( )+(?P<type>[a-z]+)(\((?P<scope>[a-z]+)\))?(?P<breaking>!)?:( )+(?P<message>.*)$")

# Extracts version info from tags
# TODO this makes assumption about tag format but no utility is provided here to create tags
# https://regex101.com/r/urHgGo/3
tag_version_extractor = re.compile(r"^v(?:(?P<mc_ver>[0-9]+(?:\.[0-9]+)*)-)?(?P<tag_ver>[0-9]+(?:\.[0-9]+)*)$")

# Extracts semantic version info from string
# Provided by semver.org
# https://regex101.com/r/Ly7O1x/3/
sem_ver_extractor = re.compile(r"^(?P<major>0|[1-9]\d*)\.(?P<minor>0|[1-9]\d*)\.(?P<patch>0|[1-9]\d*)(?:-(?P<prerelease>(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+(?P<buildmetadata>[0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$")

# Extracts semantic version from shorthand string
# https://regex101.com/r/K6R7PH/1
short_sem_ver_extractor = re.compile(r"^(?P<major>0|[1-9]\d*)(?:\.(?P<minor>0|[1-9]\d*))?$")


def sh(cmd):
    result = subprocess.run(args=cmd, capture_output=True, shell=True)
    stdout = result.stdout.decode('utf-8')
    stdout_list = stdout.split('\n')
    stdout_filtered = list(filter(None, stdout_list))
    return result.returncode, stdout_filtered


class Commit:
    """
    Represents a particular commit and its Conventional Commit info
    """

    def __init__(self, commit_log: str):
        """
        Initialize commit based on a line from `git log --oneline --no-decorate`.

        :param commit_log: Line from log, formatted as `<hash> <message>`. Message must match pattern `commit_header_extractor`
        """
        self.commit_log = commit_log

        # Parse commit info
        match = commit_header_extractor.match(commit_log)

        if match is None:
            log.warning("commit message has invalid format: %s" % commit_log)
            self.is_valid = False
            return

        self.hash = match.group('hash')
        self.type = match.group('type')
        self.scope = match.group('scope')
        self.message = match.group('message')
        self.is_breaking = match.group('breaking') is not None
        self.is_valid = True

    def __str__(self):
        return """ { "hash": "%s", "type": "%s", "scope": "%s", "message": "%s", "is_breaking": "%s", "is_major": "%s", "is_minor": "%s", "is_patch": "%s" } """ % (
            self.hash, self.type, self.scope, self.message, self.is_breaking, self.is_major(), self.is_minor(), self.is_patch())

    def is_major(self) -> bool:
        return self.is_breaking

    def is_minor(self) -> bool:
        return self.type == 'feat'

    # kinda redundant, but whatever
    def is_patch(self) -> bool:
        return not self.is_major() and not self.is_minor()


class SemVer:
    def __init__(self, major: int, minor: int, patch: int, increment: int):
        self.major = major
        self.minor = minor
        self.patch = patch
        self.increment = increment

    @classmethod
    def from_version_str(cls, version_str: str):
        match = sem_ver_extractor.match(version_str)
        if match is not None:
            major = int(match.group('major'))
            minor = int(match.group('minor'))
            patch = int(match.group('patch'))
            return cls(major, minor, patch, 0)
        else:
            log.warning("full semver match failed for %s. Trying shorthand matching" % version_str)
            match = short_sem_ver_extractor.match(version_str)
            if match is None:
                raise Exception("Invalid semantic version: %s" % version_str)

            major = int(match.group('major'))
            minor = int(match.group('minor') or 0)
            patch = 0
            return cls(major, minor, patch, 0)

    @classmethod
    def from_tag(cls, tag_str: str):
        match = tag_version_extractor.match(tag_str)
        if match is None:
            raise Exception("Could not extract version string from tag %s" % tag_str)

        return cls.from_version_str(match.group('tag_ver'))

    def __str__(self):
        return "{major}.{minor}.{patch}+{increment}".format(major=self.major, minor=self.minor, patch=self.patch, increment=self.increment)

    def copy(self):
        return SemVer(self.major, self.minor, self.patch, self.increment)

    def bump_major(self):
        self.major += 1
        self.minor = 0
        self.patch = 0

    def bump_minor(self):
        self.minor += 1
        self.patch = 0

    def bump_patch(self):
        self.patch += 1

    def bump_increment(self, n=1):
        self.increment += n

    def formatted(self, format_str):
        return format_str.format(major=self.major, minor=self.minor, patch=self.patch, increment=self.increment)


def get_versioned_tag_range():
    # TODO this may not be very robust against multi-tag commits. Perhaps iterate every commit and check tags?
    # TODO this assumes tags are prefixed with 'v'
    prev_tag = sh('git describe --tags HEAD^ --abbrev=0 --match \'v*\'')[1][0]
    current_tag = sh('git describe --tags HEAD --abbrev=0 --match \'v*\'')[1][0]

    # Replace the current tag with the short commit hash if the current tag is the same as the previous tag
    if current_tag == prev_tag:
        current_tag = sh('git rev-parse --short HEAD')[1][0]

    log.debug("versioned tag range: %s..%s" % (prev_tag, current_tag))
    return prev_tag, current_tag


def parse_commits_in_tag_range(prev_tag, current_tag, additional_args=""):
    (err, commit_logs) = sh("git log %s..%s --oneline --no-decorate %s" % (prev_tag, current_tag, additional_args))

    commit_list = []
    for commit_log in commit_logs:
        commit_list.append(Commit(commit_log))
        if not commit_list[-1].is_valid:
            log.warning("discarding invalid commit: %s" % commit_log)
            commit_list.pop()
        else:
            log.debug("found commit: %s" % commit_list[-1])

    return commit_list


def calc_version_bump_since_tag(commit_list: list[Commit], start_tag: str) -> SemVer:
    # Try to get the initial semver from tag string (e.g. v1.2.3, vMC1.18.2-1.2.3)
    version = SemVer.from_tag(start_tag)

    # Commit index simply counts number of commits since last tag, regardless of change types in between
    version.bump_increment(len(commit_list))

    # If any major change since last tag, bump major version
    if any(map(lambda c: c.is_major(), commit_list)):
        version.bump_major()
    # If any minor change since last tag, bump minor version
    elif any(map(lambda c: c.is_minor(), commit_list)):
        version.bump_minor()
    # Otherwise, bump patch version
    else:
        version.bump_patch()

    return version


def generate_build_version(format_str: str = "{major}.{minor}.{patch}+{increment}") -> str:
    """
    Generates a build version based on the most recent tag, and the types of commits added since.

    :param format_str: A python format string optionally containing strings {major}, {minor}, {patch}, and {increment}.
    :return: The major, minor, patch, and commit numbers injected into format_str
    """

    (prev_tag, current_tag) = get_versioned_tag_range()
    commit_list = parse_commits_in_tag_range(prev_tag, current_tag)
    version = calc_version_bump_since_tag(commit_list, prev_tag)
    return version.formatted(format_str)


# noinspection PyDefaultArgument
def generate_changelog_md(output_file: str = './CHANGELOG.md',
                          next_tag: str | None = None,
                          header_format: str | None = r'# Changelog ({previous_tag} -> {current_tag})',
                          categories: list[list[str | None]] = [[r'\bfeat\b', "Features", r'## {category}', r'- {type}: {message} ({hash})'],
                                                                [r'\bfix\b', "Bug fixes", r'## {category}', r'- {type}: {message} ({hash})'],
                                                                [r'\bperf\b', "Performance improvements", r'## {category}', r'- {type}: {message} ({hash})'],
                                                                [r'(\bbuild\b)|(\bci\b)|(\btest\b)', "Pipeline", r'## {category}', r'- {type}: {message} ({hash})'],
                                                                [r'(\bdocs\b)|(\brefactor\b)|(\bstyle\b)|(\binternal\b)', "Maintenance", r'## {category}', r'- {type}: {message} ({hash})']]):
    """
    Generates a changelog from commit messages between the last tag and now.

    If the current commit (HEAD) is tagged, then the previous tag is selected as the base.

    Parameters:
    -----------
    outputFile: The output file to write to, overwriting if one exists
    nextTag: Optional version of current HEAD to title the changelog. Defaults to tag of HEAD (or hash if not tagged)
    headerFormat: A Python format string to be used as the header containing tokens {previous_tag} and {current_tag}
    categories: A list of categories, where each category is a list of 4 strings: [<commit type regex>, <title>, <title format>, <line format>]
    """

    (prev_tag, current_tag) = get_versioned_tag_range()
    # Don't include commits from merged branches
    commits = parse_commits_in_tag_range(prev_tag, current_tag, "--first-parent")

    log.info("Generating changelog for %d commits between %s and %s" % (len(commits), prev_tag, current_tag))

    if next_tag:
        current_tag = next_tag

    # Open the file and write the header
    file_io = open(output_file, 'w')
    if header_format:
        file_io.write(header_format.format(previous_tag=prev_tag, current_tag=current_tag) + '\n')

    # for each category
    for category_def in categories:
        pattern = re.compile(category_def[0])
        name = category_def[1]
        category_format = category_def[2]
        line_format = category_def[3]
        category_empty = True

        # find commits than match regex
        for commit in commits:
            if pattern.match(commit.type) is not None:
                if category_empty:  # if this is the first entry in the category, print the header
                    if category_format is not None:
                        file_io.write(category_format.format(category=name) + '\n')
                    category_empty = False

                # write line
                file_io.write(line_format.format(type=commit.type, message=commit.message, hash=commit.hash) + '\n')

    file_io.close()
    log.info("Done. Changelog generated at %s" % output_file)


# noinspection PyDefaultArgument
def generate_changelog_raw(output_file='./CHANGELOG.txt',
                           categories: list[list[str | None]] = [[r'\bfeat\b', None, None, r'{type}: {message} ({hash})'],
                                                                 [r'\bfix\b', None, None, r'{type}: {message} ({hash})'],
                                                                 [r'\bperf\b', None, None, r'{type}: {message} ({hash})']]):
    generate_changelog_md(output_file=output_file, header_format=None, categories=categories)


if __name__ == '__main__':
    fire.Fire({
        "genVersion": generate_build_version,
        "genChangelogMd": generate_changelog_md,
        "genChangelogRaw": generate_changelog_raw
    })
