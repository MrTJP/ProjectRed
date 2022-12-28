package mrtjp.projectred.illumination;

import codechicken.lib.colour.Colour;
import codechicken.lib.colour.EnumColour;
import codechicken.lib.raytracer.VoxelShapeCache;
import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.OBJParser;
import codechicken.lib.render.item.IItemRenderer;
import codechicken.lib.render.lighting.LightModel;
import codechicken.lib.render.pipeline.ColourMultiplier;
import codechicken.lib.texture.AtlasRegistrar;
import codechicken.lib.util.TransformUtils;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.Vector3;
import codechicken.lib.vec.uv.IconTransformation;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.projectred.core.RenderHalo;
import mrtjp.projectred.illumination.item.MultipartLightPartItem;
import mrtjp.projectred.illumination.part.MultipartLightPart;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.model.IModelTransform;
import net.minecraft.client.renderer.model.ItemCameraTransforms;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.shapes.VoxelShape;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import static mrtjp.projectred.ProjectRedIllumination.MOD_ID;

public abstract class MultipartLightProperties {

    //region Properties
    public boolean canFloat() {
        return false;
    }

    public abstract ItemStack makeStack(int color, boolean inverted);

    public abstract VoxelShape getShape(int side);
    public abstract Cuboid6 getGlowBounds(int side);
    //endregion

    //region Registration
    public abstract MultipartLightPart partFactory(int color, boolean inverted);
    //endregion

    //region Client Registration
    public abstract void registerIcons(AtlasRegistrar registrar);
    //endregion

    //region Rendering
    public abstract TextureAtlasSprite getIcon(int color);
    public abstract CCModel getBulbModel(int side);
    public abstract CCModel getChasisModel(int side);

    public CCModel getInventoryBulbModel() {
        return getBulbModel(0);
    }
    public CCModel getInventoryChassisModel() {
        return getChasisModel(0);
    }
    public Cuboid6 getInventoryGlowBounds() {
        return getGlowBounds(0);
    }

    public void render(MultipartLightPart part, Vector3 pos, CCRenderState ccrs) {
        IconTransformation icon = new IconTransformation(getIcon(part.getColor()));
        Translation t = pos.translation();
        getChasisModel(part.getSide()).render(ccrs, t, icon);
        getBulbModel(part.getSide()).render(ccrs, t, icon, getColourMultiplier(part.getColor(), part.isLightOn()));
    }

    public void renderInventory(int colour, boolean inverted, Vector3 pos, CCRenderState ccrs) {
        IconTransformation icon = new IconTransformation(getIcon(colour));
        Translation t = pos.translation();

        getInventoryChassisModel().render(ccrs, t, icon);
        getInventoryBulbModel().render(ccrs, t, icon, getColourMultiplier(colour, inverted));
    }

    public ColourMultiplier getColourMultiplier(int color, boolean isOn) {
        Colour c = EnumColour.values()[color].getColour();
        if (!isOn) {
            c.multiply(EnumColour.GRAY.getColour());
        }
        return new ColourMultiplier(c.rgba());
    }

    public IItemRenderer getItemRenderer() {
        return new IItemRenderer() {
            @Override
            public void renderItem(ItemStack stack, ItemCameraTransforms.TransformType transformType, MatrixStack mStack, IRenderTypeBuffer getter, int packedLight, int packedOverlay) {
                if (!(stack.getItem() instanceof MultipartLightPartItem)) return;

                MultipartLightPartItem lightItem = (MultipartLightPartItem) stack.getItem();
                CCRenderState ccrs = CCRenderState.instance();
                ccrs.reset();
                ccrs.brightness = packedLight;
                ccrs.overlay = packedOverlay;
                ccrs.bind(RenderType.cutout(), getter, mStack);
                renderInventory(lightItem.getColor(), lightItem.isInverted(), Vector3.ZERO, ccrs);

                if (lightItem.isInverted()) {
                    RenderHalo.renderHalo(ccrs, mStack, getter, getInventoryGlowBounds(), lightItem.getColor(), Vector3.ZERO);
                }
            }

            //@formatter:off
            @Override public IModelTransform getModelTransform() { return TransformUtils.DEFAULT_BLOCK; }
            @Override public boolean useAmbientOcclusion() { return true; }
            @Override public boolean isGui3d() { return true; }
            @Override public boolean usesBlockLight() { return true; }
            //@formatter:on
        };
    }
    //endregion

    //region Utils
    public static VoxelShape[] boxesToShapes(Cuboid6[] bounds) {
        return Arrays.stream(bounds).map(VoxelShapeCache::getShape).toArray(VoxelShape[]::new);
    }

    public static Map<String, CCModel> parseCorrectedModel(String name) {
        Map<String, CCModel> models = OBJParser.parseModels(new ResourceLocation(MOD_ID, "obj/" + name + ".obj"),  7, null);

        Map<String, CCModel> bfModels = new HashMap<>();
        models.forEach((key, model) -> bfModels.put(key, model.backfacedCopy()));

        Translation t = new Translation(0.5, 0, 0.5);
        for (CCModel model : bfModels.values()) {
            model.apply(t);
        }

        return bfModels;
    }

    public static CCModel bakeCopy(int s, CCModel model) {
        CCModel m = model.copy();
        m.apply(Rotation.sideOrientation(s, 0).at(Vector3.CENTER));
        finishModel(m);
        return m;
    }

    public static CCModel finishModel(CCModel model) {
        model.computeNormals();
        model.computeLighting(LightModel.standardLightModel);
        model.shrinkUVs(0.0005);
        return model;
    }

    public static Cuboid6[] sidedBoxes(Cuboid6 box) {
        Cuboid6[] boxes = new Cuboid6[6];
        boxes[0] = box.copy();
        for (int s = 1; s < 6; s++)
            boxes[s] = box.copy().apply(Rotation.sideRotations[s].at(Vector3.CENTER));
        return boxes;
    }
    //endregion
}
