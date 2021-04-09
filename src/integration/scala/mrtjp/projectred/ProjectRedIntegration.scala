package mrtjp.projectred

import mrtjp.projectred.integration.{IntegrationProxy, IntegrationProxyClient}
import net.minecraftforge.fml.DistExecutor

object ProjectRedIntegration {
    final var MOD_ID = "projectred-integration"
    final var proxy:IntegrationProxy = DistExecutor.safeRunForDist(
        () => () => new IntegrationProxyClient().asInstanceOf[IntegrationProxy],
        () => () => new IntegrationProxy())
}

class ProjectRedIntegration
{
    ProjectRedIntegration.proxy.construct()
}
