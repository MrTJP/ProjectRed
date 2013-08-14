package mrtjp.projectred.exploration;

import static mrtjp.projectred.ProjectRed.itemDiamondSaw;
import static mrtjp.projectred.ProjectRed.itemDiamondSickle;
import static mrtjp.projectred.ProjectRed.itemGoldSaw;
import static mrtjp.projectred.ProjectRed.itemGoldSickle;
import static mrtjp.projectred.ProjectRed.itemIronSaw;
import static mrtjp.projectred.ProjectRed.itemIronSickle;
import static mrtjp.projectred.ProjectRed.itemPeridotAxe;
import static mrtjp.projectred.ProjectRed.itemPeridotHoe;
import static mrtjp.projectred.ProjectRed.itemPeridotPickaxe;
import static mrtjp.projectred.ProjectRed.itemPeridotSaw;
import static mrtjp.projectred.ProjectRed.itemPeridotShovel;
import static mrtjp.projectred.ProjectRed.itemPeridotSickle;
import static mrtjp.projectred.ProjectRed.itemPeridotSword;
import static mrtjp.projectred.ProjectRed.itemRubyAxe;
import static mrtjp.projectred.ProjectRed.itemRubyHoe;
import static mrtjp.projectred.ProjectRed.itemRubyPickaxe;
import static mrtjp.projectred.ProjectRed.itemRubySaw;
import static mrtjp.projectred.ProjectRed.itemRubyShovel;
import static mrtjp.projectred.ProjectRed.itemRubySickle;
import static mrtjp.projectred.ProjectRed.itemRubySword;
import static mrtjp.projectred.ProjectRed.itemSapphireAxe;
import static mrtjp.projectred.ProjectRed.itemSapphireHoe;
import static mrtjp.projectred.ProjectRed.itemSapphirePickaxe;
import static mrtjp.projectred.ProjectRed.itemSapphireSaw;
import static mrtjp.projectred.ProjectRed.itemSapphireShovel;
import static mrtjp.projectred.ProjectRed.itemSapphireSickle;
import static mrtjp.projectred.ProjectRed.itemSapphireSword;
import static mrtjp.projectred.ProjectRed.itemStoneSaw;
import static mrtjp.projectred.ProjectRed.itemStoneSickle;
import static mrtjp.projectred.ProjectRed.itemWoodSaw;
import static mrtjp.projectred.ProjectRed.itemWoodSickle;
import static mrtjp.projectred.ProjectRed.itemWoolGin;
import mrtjp.projectred.core.IProxy;
import mrtjp.projectred.exploration.BlockOre.EnumOre;
import mrtjp.projectred.exploration.BlockSpecialStone.EnumSpecialStone;
import mrtjp.projectred.exploration.ItemGemSaw.GemSawItemRenderer;
import net.minecraftforge.client.MinecraftForgeClient;
import cpw.mods.fml.common.registry.LanguageRegistry;

public class ExplorationClientProxy implements IProxy {

    @Override
    public void preinit(){}
    
    @Override
    public void init() {
        LanguageRegistry.addName(itemWoolGin, "Wool Gin");
        
        for (EnumOre o : EnumOre.VALID_ORES) {
            LanguageRegistry.addName(o.getItemStack(1), o.name);
        }

        for (EnumSpecialStone s : EnumSpecialStone.VALID_STONE) {
            LanguageRegistry.addName(s.getItemStack(), s.name);
        }
        
        LanguageRegistry.addName(itemRubyAxe, itemRubyAxe.tool.name);
        LanguageRegistry.addName(itemSapphireAxe, itemSapphireAxe.tool.name);
        LanguageRegistry.addName(itemPeridotAxe, itemPeridotAxe.tool.name);
        LanguageRegistry.addName(itemRubyHoe, itemRubyHoe.tool.name);
        LanguageRegistry.addName(itemSapphireHoe, itemSapphireHoe.tool.name);
        LanguageRegistry.addName(itemPeridotHoe, itemPeridotHoe.tool.name);
        LanguageRegistry.addName(itemRubyPickaxe, itemRubyPickaxe.tool.name);
        LanguageRegistry.addName(itemSapphirePickaxe, itemSapphirePickaxe.tool.name);
        LanguageRegistry.addName(itemPeridotPickaxe, itemPeridotPickaxe.tool.name);
        LanguageRegistry.addName(itemRubyShovel, itemRubyShovel.tool.name);
        LanguageRegistry.addName(itemSapphireShovel, itemSapphireShovel.tool.name);
        LanguageRegistry.addName(itemPeridotShovel, itemPeridotShovel.tool.name);
        LanguageRegistry.addName(itemRubySword, itemRubySword.tool.name);
        LanguageRegistry.addName(itemSapphireSword, itemSapphireSword.tool.name);
        LanguageRegistry.addName(itemPeridotSword, itemPeridotSword.tool.name);
        LanguageRegistry.addName(itemWoodSaw, itemWoodSaw.tool.name);
        LanguageRegistry.addName(itemStoneSaw, itemStoneSaw.tool.name);
        LanguageRegistry.addName(itemIronSaw, itemIronSaw.tool.name);
        LanguageRegistry.addName(itemGoldSaw, itemGoldSaw.tool.name);
        LanguageRegistry.addName(itemRubySaw, itemRubySaw.tool.name);
        LanguageRegistry.addName(itemSapphireSaw, itemSapphireSaw.tool.name);
        LanguageRegistry.addName(itemPeridotSaw, itemPeridotSaw.tool.name);
        LanguageRegistry.addName(itemDiamondSaw, itemDiamondSaw.tool.name);
        LanguageRegistry.addName(itemWoodSickle, itemWoodSickle.tool.name);
        LanguageRegistry.addName(itemStoneSickle, itemStoneSickle.tool.name);
        LanguageRegistry.addName(itemIronSickle, itemIronSickle.tool.name);
        LanguageRegistry.addName(itemGoldSickle, itemGoldSickle.tool.name);
        LanguageRegistry.addName(itemRubySickle, itemRubySickle.tool.name);
        LanguageRegistry.addName(itemSapphireSickle, itemSapphireSickle.tool.name);
        LanguageRegistry.addName(itemPeridotSickle, itemPeridotSickle.tool.name);
        LanguageRegistry.addName(itemDiamondSickle, itemDiamondSickle.tool.name);
        
        MinecraftForgeClient.registerItemRenderer(itemWoodSaw.itemID, GemSawItemRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(itemStoneSaw.itemID, GemSawItemRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(itemIronSaw.itemID, GemSawItemRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(itemGoldSaw.itemID, GemSawItemRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(itemRubySaw.itemID, GemSawItemRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(itemSapphireSaw.itemID, GemSawItemRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(itemPeridotSaw.itemID, GemSawItemRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(itemDiamondSaw.itemID, GemSawItemRenderer.instance);
    }
    
    @Override
    public void postinit() {}
}
