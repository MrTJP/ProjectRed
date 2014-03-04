package mrtjp.projectred.core

trait IProxy
{
    def preinit()

    def init()

    def postinit()

    final def versionCheck()
    {
        def crash() = throw new Exception(
            "Version for a module of ProjectRed ("+version+"."+build+") "+
                "does not match the Core version "+
                "("+Configurator.version+"."+Configurator.buildnumber+"). " +
                "Please make sure all ProjectRed jars are of the same version.")

        if (version != Configurator.version) crash()
        if (build != Configurator.buildnumber) crash()
    }

    def version:String
    def build:String
}