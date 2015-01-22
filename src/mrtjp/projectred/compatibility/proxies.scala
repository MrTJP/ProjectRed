package mrtjp.projectred.compatibility

import mrtjp.projectred.core.{Configurator, IProxy}
import sun.security.krb5.Config

class CompatibilityProxy_server extends IProxy
{
    def preinit()
    {
        Services.servicesLoad()
        Services.doPreInit()
    }

    def init()
    {
        Services.doInit()
    }

    def postinit()
    {
        Services.doPostInit()
    }

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"
}

class CompatibilityProxy_client extends CompatibilityProxy_server

object CompatibilityProxy extends CompatibilityProxy_client