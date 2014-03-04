package mrtjp.projectred.compatibility

import mrtjp.projectred.core.{Configurator, IProxy}
import sun.security.krb5.Config

class CompatibilityProxy_server extends IProxy
{
    def preinit()
    {
        Services.loadServices()
    }

    def init()
    {
        if (Services.loadTConstruct) Services.tcProxy.init()
        if (Services.loadTExpansion) Services.teProxy.init()
        if (Services.loadTreecapitator) Services.treecapProxy.init()
    }

    def postinit() {}

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"
}

class CompatibilityProxy_client extends CompatibilityProxy_server
{

}

object CompatibilityProxy extends CompatibilityProxy_client
