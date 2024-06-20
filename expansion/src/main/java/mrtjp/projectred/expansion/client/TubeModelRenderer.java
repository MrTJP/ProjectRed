package mrtjp.projectred.expansion.client;

import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.pipeline.ColourMultiplier;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.uv.IconTransformation;
import codechicken.microblock.api.MicroMaterial;
import mrtjp.projectred.expansion.part.BaseTubePart;
import mrtjp.projectred.expansion.part.RedstoneTubePart;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;

import javax.annotation.Nullable;

public class TubeModelRenderer {

    private static final CCModel[] pipeModels = new CCModel[64];
    private static final CCModel[] redstoneWireModels = new CCModel[64];

    private static int modelKey(int connMap) {
        return connMap;
    }

    private static int modelKey(BaseTubePart pipe) {
        return modelKey(pipe.getConnMap());
    }

    public static CCModel getOrGeneratePipeModel(int key) {
        CCModel model = pipeModels[key];
        if (model == null) {
            model = new TubeModelBuilder().setModelKey(key).build();
            pipeModels[key] = model;
        }
        return model;
    }

    public static CCModel getOrGenerateWireModel(int key) {
        CCModel model = redstoneWireModels[key];
        if (model == null) {
            model = new TubeModelBuilder().setWireMode(true).setModelKey(key).build();
            redstoneWireModels[key] = model;
        }
        return model;
    }

    public static void render(CCRenderState ccrs, BaseTubePart pipe) {
        boolean hasRs = (pipe instanceof RedstoneTubePart rpp) && rpp.hasRedstone();
        int signal = (pipe instanceof RedstoneTubePart rpp) ? rpp.getSignal() : 0;
        render(ccrs, modelKey(pipe), pipe.getIcon(), pipe.getMaterial(), hasRs, signal);
    }

    public static void render(CCRenderState ccrs, int modelKey, TextureAtlasSprite icon, @Nullable MicroMaterial material, boolean hasRedstone, int signal) {
        IconTransformation uvt = new IconTransformation(icon);

        if (material != null && false) {//TODO
//            FramedJacketedWireModel model = getOrGenerateJacketModel(modelKey);
//            model.renderWire(ccrs, uvt, c);
//            model.renderMaterial(ccrs, material, false);
        } else {
            getOrGeneratePipeModel(modelKey).render(ccrs, uvt);
            if (hasRedstone) {
                int c = (signal & 0xFF) / 2 + 60 << 24 | 0xFF;
                getOrGenerateWireModel(modelKey).render(ccrs, uvt, ColourMultiplier.instance(c));
            }
        }
    }

    public static void renderInventory(CCRenderState ccrs, TextureAtlasSprite icon, Transformation transformation) {
        IconTransformation uvt = new IconTransformation(icon);
        getOrGeneratePipeModel(modelKey(0x03)).render(ccrs, uvt, transformation);
    }
}
