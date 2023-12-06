package mrtjp.projectred.transmission.client;

import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.pipeline.ColourMultiplier;
import codechicken.lib.render.pipeline.IVertexOperation;
import codechicken.lib.vec.Matrix4;
import codechicken.lib.vec.Scale;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Vector3;
import codechicken.lib.vec.uv.IconTransformation;
import codechicken.microblock.api.MicroMaterial;
import codechicken.microblock.client.MicroblockRender;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.transmission.part.BaseCenterWirePart;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.core.BlockPos;

import javax.annotation.Nullable;

public class FramedWireModelRenderer {

    private static final CCModel[] scaffoldModels = new FramedWireScaffoldingModelBuilder().build();
    private static final CCModel[] wireModels = new CCModel[64 * 3];
    private static final FramedJacketedWireModel[] jacketModels = new FramedJacketedWireModel[64 * 3];

    private static int modelKey(int thickness, int connMap) {
        return thickness << 6 | connMap;
    }

    private static int modelKey(BaseCenterWirePart wire) {
        return modelKey(wire.getWireType().getThickness(), wire.getConnMap());
    }

    public static CCModel getOrGenerateWireModel(int key) {
        CCModel model = wireModels[key];
        if (model == null) {
            model = new FramedWireModelBuilder().setModelKey(key).build();
            wireModels[key] = model;
        }
        return model;
    }

    public static FramedJacketedWireModel getOrGenerateJacketModel(int key) {
        FramedJacketedWireModel model = jacketModels[key];
        if (model == null) {
            model = new FramedJacketedWireModelBuilder().setModelKey(key).build();
            jacketModels[key] = model;
        }
        return model;
    }

    public static void render(CCRenderState ccrs, BaseCenterWirePart wire) {
        render(ccrs, modelKey(wire), wire.getRenderHue(), wire.getIcon(), wire.getMaterial());
    }

    public static void render(CCRenderState ccrs, int modelKey, int hue, TextureAtlasSprite icon, @Nullable MicroMaterial material) {
        IconTransformation uvt = new IconTransformation(icon);
        ColourMultiplier c = ColourMultiplier.instance(hue);

        if (material != null) {
            FramedJacketedWireModel model = getOrGenerateJacketModel(modelKey);
            model.renderWire(ccrs, uvt, c);
            model.renderMaterial(ccrs, material, false);
        } else {
            getOrGenerateWireModel(modelKey).render(ccrs, uvt, c);
            renderWireScaffold(modelKey, ccrs, uvt);
        }
    }

    public static void renderWireScaffold(int key, CCRenderState ccrs, IVertexOperation... ops) {
        scaffoldModels[6].render(ccrs, ops);
        for (int s = 0; s < 6; s++) {
            if ((key & 1 << s) != 0) {
                scaffoldModels[s].render(ccrs, ops);
            }
        }
    }

    public static void renderInventory(CCRenderState ccrs, int thickness, int hue, TextureAtlasSprite icon, Transformation transformation) {

        IconTransformation uvt = new IconTransformation(icon);
        ColourMultiplier c = ColourMultiplier.instance(hue);

        getOrGenerateWireModel(modelKey(thickness, 0x3C)).render(ccrs, uvt, c, transformation);
        renderWireScaffold(modelKey(thickness, 0), ccrs, uvt, transformation);
    }

    public static void renderCoverHighlight(BaseCenterWirePart wire, MicroMaterial material, CCRenderState ccrs, PoseStack mStack, MultiBufferSource getter) {
        BlockPos pos = wire.pos();

        Matrix4 mat = new Matrix4(mStack);
        mat.translate(pos);
        mat.apply(new Scale(1.002, 1.002, 1.002).at(Vector3.CENTER));

        ccrs.reset();
        ccrs.bind(MicroblockRender.HIGHLIGHT_RENDER_TYPE, getter, mat);
        ccrs.alphaOverride = 127;
        getOrGenerateJacketModel(modelKey(wire)).renderHighlight(ccrs, material, true);
    }
}
