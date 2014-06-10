package mrtjp.projectred.compatibility

import codechicken.core.ClassDiscoverer
import cpw.mods.fml.common.Loader
import mrtjp.projectred.core.PRLogger
import scala.collection.immutable.HashMap

object Services
{
    var plugins = HashMap[String, IPRPlugin]()

    def servicesReflect()
    {
        val finder = new ClassDiscoverer(classOf[IPRPlugin])
        finder.findClasses()

        import scala.collection.JavaConversions._
        for (c <- finder.classes) try
        {
            val plugin = c.newInstance().asInstanceOf[IPRPlugin]
            if (Loader.isModLoaded(plugin.getModID))
            {
                plugins += plugin.getModID -> plugin
                PRLogger.info("Loaded ProjectRed compat plugin for '"+plugin.getModID+"'")
            }
        }
        catch
            {
                case e:Exception =>
                    System.out.println("Failed to load " + c.getName)
                    e.printStackTrace()
            }

        PRLogger.info("Finished loading ProjectRed compat plugins")
    }

    def doPreInit()
    {
        for ((id, p) <- plugins) p.preInit()
    }

    def doInit()
    {
        for ((id, p) <- plugins) p.init()
    }

    def doPostInit()
    {
        for ((id, p) <- plugins) p.postInit()
    }
}