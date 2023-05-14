package mrtjp.projectred.illumination.init;

import mrtjp.projectred.illumination.BlockLightType;

import static mrtjp.projectred.ProjectRedIllumination.*;

public class IlluminationBlocks {

    public static void register() {

        // Block lights
        for (BlockLightType lampType : BlockLightType.values()) {
            lampType.registerBlocks(BLOCKS, ITEMS, TILE_ENTITIES);
        }
    }
}
