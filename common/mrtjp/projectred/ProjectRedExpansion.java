package mrtjp.projectred;

import mrtjp.projectred.core.IProxy;
import mrtjp.projectred.expansion.BlockMachine;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraftforge.common.MinecraftForge;
import codechicken.lib.packet.PacketCustom.CustomTinyPacketHandler;
import cpw.mods.fml.common.Mod;
import cpw.mods.fml.common.Mod.Instance;
import cpw.mods.fml.common.SidedProxy;
import cpw.mods.fml.common.event.FMLInitializationEvent;
import cpw.mods.fml.common.event.FMLPostInitializationEvent;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;
import cpw.mods.fml.common.network.NetworkMod;

@Mod(modid = "ProjRed|Expansion", useMetadata = true)
@NetworkMod(clientSideRequired = true, serverSideRequired = true, tinyPacketHandler = CustomTinyPacketHandler.class)
public class ProjectRedExpansion
{
    /** Blocks **/
    public static BlockMachine machine1;

    @Instance("ProjRed|Expansion")
    public static ProjectRedExpansion instance;

    @SidedProxy(clientSide = "mrtjp.projectred.expansion.ExpansionClientProxy", serverSide = "mrtjp.projectred.expansion.ExpansionProxy")
    public static IProxy proxy;

    public static CreativeTabs tabExpansion;

//    = new CreativeTabs("expansion") {
//        @Override
//        public ItemStack getIconItemStack()
//        {
//            // TODO Change this to one of the machines.
//            return EnumPart.COPPERCOIL.getItemStack();
//        }
//    };

    @Mod.EventHandler
    public void preInit(FMLPreInitializationEvent event)
    {
        proxy.preinit();
    }

    @Mod.EventHandler
    public void init(FMLInitializationEvent event)
    {
        MinecraftForge.EVENT_BUS.register(instance);
        proxy.init();
    }

    @Mod.EventHandler
    public void postInit(FMLPostInitializationEvent event)
    {
        proxy.postinit();
    }
}
