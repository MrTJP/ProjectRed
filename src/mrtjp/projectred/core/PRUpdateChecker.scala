package mrtjp.projectred.core

import mrtjp.core.data.UpdateChecker

class PRUpdateChecker extends UpdateChecker
{
    override def mavenRootURL = "http://projectredwiki.com/maven"
    override def changelogURL = "https://raw.githubusercontent.com/MrTJP/ProjectRed/master/resources/Changelog"

    override def group = "mrtjp"
    override def project = "ProjectRed"

    override def shouldRun = Configurator.versionChecking && !Configurator.version.contains("@")
    override def checkUnstable = Configurator.versionCheckDevBuilds
    override def currentVersion = Configurator.version+"."+Configurator.buildnumber
}