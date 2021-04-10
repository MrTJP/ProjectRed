import fire
import subprocess
import re
import logging

fmt_str = '[%(asctime)s] %(levelname)s %(lineno)d: %(message)s'
logging.basicConfig(level=logging.INFO, format=fmt_str)
log = logging.getLogger(__name__)

def shellCmd(cmd):
    result = subprocess.run(args=cmd, capture_output=True, shell=True)
    stdout = result.stdout.decode('utf-8')
    stdout_list = stdout.split('\n')
    stdout_filtered = list(filter(None, stdout_list))
    return (result.returncode, stdout_filtered)

class Commit:
    def __init__(self, commitHash, commitType, commitScope, isBreaking, commitMessage):
        self.commitHash = commitHash
        self.commitType = commitType
        self.commitScope = commitScope
        self.isBreaking = isBreaking
        self.commitMessage = commitMessage

def getTags():
    prevTag = shellCmd('git describe --tags HEAD^ --abbrev=0')[1][0]
    currentTag = shellCmd('git describe --tags HEAD --abbrev=0')[1][0]
    if currentTag == prevTag:
        currentTag = shellCmd('git rev-parse --short HEAD')[1][0]
    return (prevTag, currentTag)

def getCommits(prevTag, currentTag):
    (err, commits) = shellCmd("git log %s..%s --oneline --no-decorate" % (prevTag, currentTag))
    return commits

def createCommits(prevTag, currentTag):
    (err, commits) = shellCmd("git log %s..%s --oneline --no-decorate" % (prevTag, currentTag))

    pattern = re.compile('^(?P<hash>[0-9a-fA-f]+)( )+(?P<type>[a-z]+!?)(\((?P<scope>[a-z]+)\))?(?P<breaking>!)?:( )+(?P<message>.*)$')

    commitObjects = []

    for i, commit in enumerate(commits):
        match = pattern.match(commit)

        if (match is None):
            log.warning("commit %d message has invalid format: %s" % (i, commit))
            continue

        cHash = match.group('hash')
        cType = match.group('type')
        cScope = match.groups('scope')
        cMessage = match.group('message')
        cIsBreaking = match.group('breaking')

        commit = Commit(cHash, cType, cScope, cIsBreaking, cMessage)
        commitObjects.append(commit)

    return commitObjects

def generateRaw(outputFile = './CHANGELOG.txt',
    categories = [ [r'\bfeat\b', None, None, r'{type}: {message} ({hash})'],
                   [r'\bfix\b', None, None, r'{type}: {message} ({hash})'],
                   [r'\bperf\b', None, None, r'{type}: {message} ({hash})'] ]):
    generateMarkdown(outputFile=outputFile, headerFormat=None, categories=categories)

def generateMarkdown(outputFile = './CHANGELOG.md',
    nextTag = None,
    headerFormat = r'# Changelog ({previousTag} -> {currentTag})',
    categories = [ [r'\bfeat\b', "Features", r'## {category}', r'- {type}: {message} ({hash})'],
                   [r'\bfix\b', "Bug fixes", r'## {category}', r'- {type}: {message} ({hash})'],
                   [r'\bperf\b', "Performance improvements", r'## {category}', r'- {type}: {message} ({hash})'],
                   [r'(\bbuild\b)|(\bci\b)|(\btest\b)', "Pipeline", r'## {category}', r'- {type}: {message} ({hash})'],
                   [r'(\bdocs\b)|(\brefactor\b)|(\bstyle\b)', "Maintenence", r'## {category}', r'- {type}: {message} ({hash})'] ]):

    """
    Generates a changelog from commit messages between the last tag and now.

    If the current commit (HEAD) is tagged, then the previous tag is selected as the base.

    Parameters:
    -----------
    outputFile: The output file to write to, overwriting if one exists
    nextTag: Optional version of current HEAD to title the changelog. Defaults to tag of HEAD (or hash if not tagged)
    headerFormat: A Python format string to be used as the header containing tokens {previousTag} and {currentTag}
    categories: A list of categories, where each category is a list of 4 strings: [<commit type regex>, <title>, <title format>, <line format>]
    """

    log.setLevel(logging.INFO)
    log.info("Calculating tag range...")
    (prevTag, currentTag) = getTags()
    log.info("Tag range: %s..%s" % (prevTag, currentTag))
    commits = createCommits(prevTag, currentTag)
    log.info("Parsing %d commits within range..." % len(commits))

    if nextTag is not None:
        currentTag = nextTag

    # Open the file and write the header
    fileObj = open(outputFile, 'w')
    if headerFormat is not None:
        fileObj.write(headerFormat.format(previousTag = prevTag, currentTag = currentTag) + '\n')

    # for each category
    for categoryDef in categories:
        pattern = re.compile(categoryDef[0])
        name = categoryDef[1]
        categoryFormat = categoryDef[2]
        lineFormat = categoryDef[3]
        categoryEmpty = True

        # find commits than match regex
        for commit in commits:
            if pattern.match(commit.commitType) is not None:
                if categoryEmpty: # if this is the first entry in the category, print the header
                    if categoryFormat is not None:
                        fileObj.write(categoryFormat.format(category = name) + '\n')
                    categoryEmpty = False

                # write line
                fileObj.write(lineFormat.format(type = commit.commitType, message = commit.commitMessage, hash = commit.commitHash) + '\n')

    fileObj.close
    log.info("Done. Changelog generated at %s" % outputFile)


if __name__ == "__main__":
    fire.Fire({
        "generateRaw": generateRaw,
        "generateMarkdown": generateMarkdown
    })