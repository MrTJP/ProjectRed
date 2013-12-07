package mrtjp.projectred.core;

import mrtjp.projectred.core.fx.ParticleIconRegistry;
import mrtjp.projectred.core.fx.ParticleManagement;
import net.minecraftforge.common.MinecraftForge;
import codechicken.lib.packet.PacketCustom;
import cpw.mods.fml.client.registry.RenderingRegistry;
import cpw.mods.fml.common.registry.TickRegistry;
import cpw.mods.fml.relauncher.Side;

public class CoreClientProxy extends CoreProxy
{    
    @Override
    public void preinit() {
        super.preinit();
        
        MinecraftForge.EVENT_BUS.register(ParticleManagement.instance);
        MinecraftForge.EVENT_BUS.register(ParticleIconRegistry.instance);
        TickRegistry.registerTickHandler(ParticleManagement.instance, Side.CLIENT);
    }
    
    @Override
    public void init() {
        super.init();
        MinecraftForge.EVENT_BUS.register(new Messenger());
        PacketCustom.assignHandler(CoreCPH.channel, new CoreCPH());

        BasicRenderUtils.coreRenderHandlerID = RenderingRegistry.getNextAvailableRenderId();
        RenderingRegistry.registerBlockHandler(BasicRenderUtils.MultiRenderHandler.instance);
    }
}
