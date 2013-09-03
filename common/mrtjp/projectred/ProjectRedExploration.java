package mrtjp.projectred;

import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import mrtjp.projectred.exploration.BlockOre;
import mrtjp.projectred.exploration.BlockSpecialStone;
import mrtjp.projectred.exploration.ExplorationGuiHandler;
import mrtjp.projectred.exploration.ExplorationLocalizationHandler;
import mrtjp.projectred.exploration.ItemBackpack;
import mrtjp.projectred.exploration.ItemGemAxe;
import mrtjp.projectred.exploration.ItemGemHoe;
import mrtjp.projectred.exploration.ItemGemPickaxe;
import mrtjp.projectred.exploration.ItemGemSaw;
import mrtjp.projectred.exploration.ItemGemShovel;
import mrtjp.projectred.exploration.ItemGemSickle;
import mrtjp.projectred.exploration.ItemGemSword;
import mrtjp.projectred.exploration.ItemWoolGin;
import net.minecraft.block.Block;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.EnumToolMaterial;
import net.minecraft.item.ItemStack;
import net.minecraftforge.common.MinecraftForge;
import cpw.mods.fml.common.Mod;
import cpw.mods.fml.common.Mod.Instance;
import cpw.mods.fml.common.SidedProxy;
import cpw.mods.fml.common.event.FMLInitializationEvent;
import cpw.mods.fml.common.event.FMLPostInitializationEvent;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;
import cpw.mods.fml.common.network.NetworkMod;
import cpw.mods.fml.common.network.NetworkRegistry;

@Mod(modid = "ProjRed|Exploration", name = "ProjectRed-Exploration", version = Configurator.version + "." + Configurator.buildnumber, acceptedMinecraftVersions = "[1.6.2]", dependencies = "required-after:ProjRed|Core;")
@NetworkMod(clientSideRequired = true, serverSideRequired = true)
public class ProjectRedExploration {

    /** Blocks **/
    public static BlockOre blockOres;
    public static BlockSpecialStone blockStones;

    /** Items **/
    public static ItemWoolGin itemWoolGin;
    public static ItemBackpack itemBackpack;

    public static EnumToolMaterial toolMaterialRuby;
    public static EnumToolMaterial toolMaterialSapphire;
    public static EnumToolMaterial toolMaterialPeridot;

    public static ItemGemAxe itemRubyAxe;
    public static ItemGemAxe itemSapphireAxe;
    public static ItemGemAxe itemPeridotAxe;

    public static ItemGemHoe itemRubyHoe;
    public static ItemGemHoe itemSapphireHoe;
    public static ItemGemHoe itemPeridotHoe;

    public static ItemGemPickaxe itemRubyPickaxe;
    public static ItemGemPickaxe itemSapphirePickaxe;
    public static ItemGemPickaxe itemPeridotPickaxe;

    public static ItemGemShovel itemRubyShovel;
    public static ItemGemShovel itemSapphireShovel;
    public static ItemGemShovel itemPeridotShovel;

    public static ItemGemSword itemRubySword;
    public static ItemGemSword itemSapphireSword;
    public static ItemGemSword itemPeridotSword;

    public static ItemGemSaw itemWoodSaw;
    public static ItemGemSaw itemStoneSaw;
    public static ItemGemSaw itemIronSaw;
    public static ItemGemSaw itemGoldSaw;
    public static ItemGemSaw itemRubySaw;
    public static ItemGemSaw itemSapphireSaw;
    public static ItemGemSaw itemPeridotSaw;
    public static ItemGemSaw itemDiamondSaw;

    public static ItemGemSickle itemWoodSickle;
    public static ItemGemSickle itemStoneSickle;
    public static ItemGemSickle itemIronSickle;
    public static ItemGemSickle itemGoldSickle;
    public static ItemGemSickle itemRubySickle;
    public static ItemGemSickle itemSapphireSickle;
    public static ItemGemSickle itemPeridotSickle;
    public static ItemGemSickle itemDiamondSickle;

    @Instance("ProjRed|Exploration")
    public static ProjectRedExploration instance;

    @SidedProxy(clientSide = "mrtjp.projectred.exploration.ExplorationClientProxy", serverSide = "mrtjp.projectred.exploration.ExplorationProxy")
    public static IProxy proxy;

    public static CreativeTabs tabExploration = new CreativeTabs("exploration") {
        @Override
        public ItemStack getIconItemStack() {
            return new ItemStack(Block.grass);
        }
    };

    @Mod.EventHandler
    public void preInit(FMLPreInitializationEvent event) {
    	ExplorationLocalizationHandler.loadLanguages();
        if (Configurator.module_Exploration.getBoolean(true))
            proxy.preinit();
    }

    @Mod.EventHandler
    public void init(FMLInitializationEvent event) {
        if (Configurator.module_Exploration.getBoolean(true)) {
            MinecraftForge.EVENT_BUS.register(instance);
            NetworkRegistry.instance().registerGuiHandler(instance, new ExplorationGuiHandler());
            proxy.init();
        }
    }

    @Mod.EventHandler
    public void postInit(FMLPostInitializationEvent event) {        if (Configurator.module_Exploration.getBoolean(true))
            if (Configurator.module_Exploration.getBoolean(true))
                proxy.postinit();
    }

}
