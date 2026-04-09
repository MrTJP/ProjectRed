package mrtjp.projectred.expansion.compatibility;

import org.embeddedt.embeddium.api.BlockRendererRegistry;

import static mrtjp.projectred.expansion.ProjectRedExpansion.LOGGER;

public class EmbeddiumCompatibility {

    public static void initClient(Object embeddiumModObject) {
        LOGGER.info("Loading Project Red Embeddium Compatibility Module");

        // Register renderer for handling Frame-moved blocks
        BlockRendererRegistry.instance().registerRenderPopulator(
                BlockRendererRegistry.RenderPopulator.forRenderer(new EmbeddiumMovingBlockRenderer()));
    }
}
