package mrtjp.projectred.compatibility

trait IPRPlugin
{
    def getModID:String

    def preInit()
    def init()
    def postInit()
}
