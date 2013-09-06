package mrtjp.projectred.exploration;

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
import net.minecraftforge.client.MinecraftForgeClient;
import cpw.mods.fml.common.registry.LanguageRegistry;

public class ExplorationClientProxy extends ExplorationProxy {

    @Override
    public void init() {
        super.init();
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
