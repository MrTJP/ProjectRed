package mrtjp.projectred.transmission.client;

import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.pipeline.ColourMultiplier;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.uv.IconTransformation;
import mrtjp.projectred.transmission.part.BaseFaceWirePart;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;

public class WireModelRenderer {

    private static final CCModel[] wireModels = new CCModel[3 * 6 * 256];
    private static final CCModel[] inventoryWireModels = new CCModel[3];

    /**
     * Returns a tightly packed unique index for the specific model represented
     * by this wire. The mask is split into 3 sections the combination of
     * corresponding bits from the two lowest nybbles gives the connection type
     * in that direction.
     * 00 = none
     * 01 = corner
     * 10 = straight
     * 11 = internal The
     * second byte contains the thickness*6+side
     *
     * @param side      The side the wire is attached to
     * @param thickness The thickness of the wire -1 in 1/8th blocks. Supported
     *                  values 0, 1, 2
     * @param connMap   The connection mask of the wire
     */
    public static int modelKey(int side, int thickness, int connMap) {
        int key = connMap & 0xFF; //take the straight and corner connections

        int renderCorner = connMap >> 20 & 0xF;
        key |= (renderCorner ^ key & 0xF) << 4; //any corner conns that arent rendered convert to straight
        key &= ~0xF | renderCorner; //set corners to renderCorers

        int internal = (connMap & 0xF00) >> 8; //internal cons
        key |= internal << 4 | internal; //if internal is set, set both straight and corner to 1

        key |= side + thickness * 6 << 8; //add side and thickness
        return key;
    }

    private static int modelKey(BaseFaceWirePart wire) {
        return modelKey(wire.getSide(), wire.getWireType().getThickness(), wire.getConnMap());
    }

    private static CCModel getOrGenerateModel(int key) {
        CCModel model = wireModels[key];
        if (model == null) {
            model = new WireModelBuilder().setModelKey(key).build();
            wireModels[key] = model;
        }
        return model;
    }

    private static CCModel getOrGenerateInventoryModel(int thickness) {
        CCModel model = inventoryWireModels[thickness];
        if (model == null) {
            int key = modelKey(0, thickness, 0xF0);
            model = new WireModelBuilder().setModelKey(key).setInventory(true).build();
            inventoryWireModels[thickness] = model;
        }
        return model;
    }

    public static void render(CCRenderState ccrs, BaseFaceWirePart wire) {
        getOrGenerateModel(modelKey(wire)).render(
                ccrs,
                new IconTransformation(wire.getIcon()),
                ColourMultiplier.instance(wire.getRenderHue()));
    }

    public static void render(CCRenderState ccrs, int modelKey, int hue, TextureAtlasSprite icon, Transformation transformation) {
        getOrGenerateModel(modelKey).render(
                ccrs,
                new IconTransformation(icon),
                ColourMultiplier.instance(hue),
                transformation);
    }

    public static void renderInventory(CCRenderState ccrs, int thickness, int hue, TextureAtlasSprite icon, Transformation transformation) {
        getOrGenerateInventoryModel(thickness).render(
                ccrs,
                new IconTransformation(icon),
                ColourMultiplier.instance(hue),
                transformation);
    }
}
