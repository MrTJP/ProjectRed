package mrtjp.projectred.relocation;

import mrtjp.projectred.api.IFrame;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.CapabilityInject;

/**
 * Capability instance holder class.
 */
class FrameCapability
{
    @CapabilityInject(IFrame.class)
    public static Capability<IFrame> CAPABILITY;
}
