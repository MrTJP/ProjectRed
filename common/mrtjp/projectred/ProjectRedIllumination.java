package mrtjp.projectred;

import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import mrtjp.projectred.illumination.BlockLamp;
import mrtjp.projectred.illumination.ItemPartCageLamp;
import mrtjp.projectred.illumination.ItemPartFixture;
import mrtjp.projectred.illumination.ItemPartIllumarButton;
import mrtjp.projectred.illumination.ItemPartLantern;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.ItemStack;
import net.minecraftforge.common.MinecraftForge;
import codechicken.lib.packet.PacketCustom.CustomTinyPacketHandler;
import cpw.mods.fml.common.Mod;
import cpw.mods.fml.common.Mod.Instance;
import cpw.mods.fml.common.SidedProxy;
import cpw.mods.fml.common.event.FMLInitializationEvent;
import cpw.mods.fml.common.event.FMLPostInitializationEvent;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;
import cpw.mods.fml.common.network.NetworkMod;

@Mod(modid = "ProjRed|Illumination", useMetadata = true)
@NetworkMod(clientSideRequired = true, serverSideRequired = true, tinyPacketHandler = CustomTinyPacketHandler.class)
public class ProjectRedIllumination
{
    /** Blocks **/
    public static BlockLamp blockLamp;

    /** Multipart items **/
    public static ItemPartLantern itemPartLantern;
    public static ItemPartLantern itemPartInvLantern;
    public static ItemPartIllumarButton itemPartIllumarButton;
    public static ItemPartCageLamp itemPartCageLamp;
    public static ItemPartCageLamp itemPartInvCageLamp;
    public static ItemPartFixture itemPartFixture;
    public static ItemPartFixture itemPartInvFixture;

    @Instance("ProjRed|Illumination")
    public static ProjectRedIllumination instance;

    @SidedProxy(clientSide = "mrtjp.projectred.illumination.IlluminationClientProxy", serverSide = "mrtjp.projectred.illumination.IlluminationProxy")
    public static IProxy proxy;

    public static CreativeTabs tabLighting = new CreativeTabs("ill") {
        @Override
        public ItemStack getIconItemStack()
        {
            return new ItemStack(ProjectRedIllumination.itemPartInvLantern, 1, 14);
        }
    };

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