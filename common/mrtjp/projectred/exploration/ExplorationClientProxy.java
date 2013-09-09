package mrtjp.projectred.exploration;

import static mrtjp.projectred.ProjectRedExploration.itemDiamondSaw;
import static mrtjp.projectred.ProjectRedExploration.itemGoldSaw;
import static mrtjp.projectred.ProjectRedExploration.itemIronSaw;
import static mrtjp.projectred.ProjectRedExploration.itemPeridotSaw;
import static mrtjp.projectred.ProjectRedExploration.itemRubySaw;
import static mrtjp.projectred.ProjectRedExploration.itemSapphireSaw;
import static mrtjp.projectred.ProjectRedExploration.itemStoneSaw;
import static mrtjp.projectred.ProjectRedExploration.itemWoodSaw;
import mrtjp.projectred.exploration.ItemGemSaw.GemSawItemRenderer;
import net.minecraftforge.client.MinecraftForgeClient;

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
