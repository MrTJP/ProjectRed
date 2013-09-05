package mrtjp.projectred.exploration;

import static mrtjp.projectred.ProjectRedExploration.blockStores;
import static mrtjp.projectred.ProjectRedExploration.itemDiamondSaw;
import static mrtjp.projectred.ProjectRedExploration.itemDiamondSickle;
import static mrtjp.projectred.ProjectRedExploration.itemGoldSaw;
import static mrtjp.projectred.ProjectRedExploration.itemGoldSickle;
import static mrtjp.projectred.ProjectRedExploration.itemIronSaw;
import static mrtjp.projectred.ProjectRedExploration.itemIronSickle;
import static mrtjp.projectred.ProjectRedExploration.itemPeridotAxe;
import static mrtjp.projectred.ProjectRedExploration.itemPeridotHoe;
import static mrtjp.projectred.ProjectRedExploration.itemPeridotPickaxe;
import static mrtjp.projectred.ProjectRedExploration.itemPeridotSaw;
import static mrtjp.projectred.ProjectRedExploration.itemPeridotShovel;
import static mrtjp.projectred.ProjectRedExploration.itemPeridotSickle;
import static mrtjp.projectred.ProjectRedExploration.itemPeridotSword;
import static mrtjp.projectred.ProjectRedExploration.itemRubyAxe;
import static mrtjp.projectred.ProjectRedExploration.itemRubyHoe;
import static mrtjp.projectred.ProjectRedExploration.itemRubyPickaxe;
import static mrtjp.projectred.ProjectRedExploration.itemRubySaw;
import static mrtjp.projectred.ProjectRedExploration.itemRubyShovel;
import static mrtjp.projectred.ProjectRedExploration.itemRubySickle;
import static mrtjp.projectred.ProjectRedExploration.itemRubySword;
import static mrtjp.projectred.ProjectRedExploration.itemSapphireAxe;
import static mrtjp.projectred.ProjectRedExploration.itemSapphireHoe;
import static mrtjp.projectred.ProjectRedExploration.itemSapphirePickaxe;
import static mrtjp.projectred.ProjectRedExploration.itemSapphireSaw;
import static mrtjp.projectred.ProjectRedExploration.itemSapphireShovel;
import static mrtjp.projectred.ProjectRedExploration.itemSapphireSickle;
import static mrtjp.projectred.ProjectRedExploration.itemSapphireSword;
import static mrtjp.projectred.ProjectRedExploration.itemStoneSaw;
import static mrtjp.projectred.ProjectRedExploration.itemStoneSickle;
import static mrtjp.projectred.ProjectRedExploration.itemWoodSaw;
import static mrtjp.projectred.ProjectRedExploration.itemWoodSickle;
import static mrtjp.projectred.ProjectRedExploration.itemWoolGin;
import mrtjp.projectred.exploration.BlockOre.EnumOre;
import mrtjp.projectred.exploration.BlockSpecialStone.EnumSpecialStone;
import mrtjp.projectred.exploration.ItemBackpack.EnumBackpack;
import mrtjp.projectred.exploration.ItemGemSaw.GemSawItemRenderer;
import net.minecraft.item.ItemStack;
import net.minecraftforge.client.MinecraftForgeClient;
import cpw.mods.fml.common.registry.LanguageRegistry;

public class ExplorationClientProxy extends ExplorationProxy {

    @Override
    public void init() {
        super.init();
        LanguageRegistry.addName(itemWoolGin, "Wool Gin");

        for (EnumOre o : EnumOre.VALID_ORES) {
            LanguageRegistry.addName(o.getItemStack(1), o.name);
        }

        for (EnumSpecialStone s : EnumSpecialStone.VALID_STONE) {
            LanguageRegistry.addName(s.getItemStack(), s.name);
        }

        for (EnumBackpack b : EnumBackpack.VALID_BP) {
            LanguageRegistry.addName(b.getItemStack(), b.fullname);
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

        LanguageRegistry.addName(new ItemStack(blockStores, 1, 0), "Ruby block");
        LanguageRegistry.addName(new ItemStack(blockStores, 1, 1), "Sapphire block");
        LanguageRegistry.addName(new ItemStack(blockStores, 1, 2), "Peridot block");

        MinecraftForgeClient.registerItemRenderer(itemWoodSaw.itemID, GemSawItemRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(itemStoneSaw.itemID, GemSawItemRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(itemIronSaw.itemID, GemSawItemRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(itemGoldSaw.itemID, GemSawItemRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(itemRubySaw.itemID, GemSawItemRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(itemSapphireSaw.itemID, GemSawItemRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(itemPeridotSaw.itemID, GemSawItemRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(itemDiamondSaw.itemID, GemSawItemRenderer.instance);
    }
}
