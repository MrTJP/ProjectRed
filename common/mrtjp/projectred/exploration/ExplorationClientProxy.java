package mrtjp.projectred.exploration;

import static mrtjp.projectred.ProjectRedExploration.itemGoldSaw;
import static mrtjp.projectred.ProjectRedExploration.itemPeridotSaw;
import static mrtjp.projectred.ProjectRedExploration.itemRubySaw;
import static mrtjp.projectred.ProjectRedExploration.itemSapphireSaw;
import mrtjp.projectred.exploration.ItemGemSaw.GemSawItemRenderer;
import net.minecraftforge.client.MinecraftForgeClient;

public class ExplorationClientProxy extends ExplorationProxy {

    @Override
    public void init() {
        super.init();
        MinecraftForgeClient.registerItemRenderer(itemGoldSaw.itemID, GemSawItemRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(itemRubySaw.itemID, GemSawItemRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(itemSapphireSaw.itemID, GemSawItemRenderer.instance);
        MinecraftForgeClient.registerItemRenderer(itemPeridotSaw.itemID, GemSawItemRenderer.instance);
    }
}
