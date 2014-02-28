package mrtjp.projectred.compatibility

import mrtjp.projectred.core.IProxy

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
}

class CompatibilityProxy_client extends CompatibilityProxy_server
{

}

object CompatibilityProxy extends CompatibilityProxy_client
