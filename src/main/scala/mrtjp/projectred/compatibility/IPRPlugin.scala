package mrtjp.projectred.compatibility

trait IPRPlugin
{
    def getModIDs:Array[String]

    def isEnabled:Boolean

    def preInit()
    def init()
    def postInit()

    def loadFailedDesc():String = "Failed to load PR Plugin: "+desc()
    def loadCompleteDesc():String = "Loaded PR Plugin: "+desc()

    def desc():String
}
