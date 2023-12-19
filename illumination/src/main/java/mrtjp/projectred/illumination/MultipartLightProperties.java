package mrtjp.projectred.illumination;

import codechicken.lib.colour.Colour;
import codechicken.lib.colour.EnumColour;
import codechicken.lib.raytracer.VoxelShapeCache;
import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.item.IItemRenderer;
import codechicken.lib.render.lighting.LightModel;
import codechicken.lib.render.model.OBJParser;
import codechicken.lib.render.pipeline.ColourMultiplier;
import codechicken.lib.texture.AtlasRegistrar;
import codechicken.lib.util.TransformUtils;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.Vector3;
import codechicken.lib.vec.uv.IconTransformation;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.core.client.HaloRenderer;
import mrtjp.projectred.illumination.item.MultipartLightPartItem;
import mrtjp.projectred.illumination.part.MultipartLightPart;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.model.ItemTransforms;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.ModelState;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import static mrtjp.projectred.illumination.ProjectRedIllumination.MOD_ID;

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
    @OnlyIn(Dist.CLIENT)
    public abstract void registerIcons(AtlasRegistrar registrar);
    //endregion

    //region Rendering
    @OnlyIn(Dist.CLIENT)
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

    @OnlyIn(Dist.CLIENT)
    public void render(MultipartLightPart part, Vector3 pos, CCRenderState ccrs) {
        IconTransformation icon = new IconTransformation(getIcon(part.getColor()));
        Translation t = pos.translation();
        getChasisModel(part.getSide()).render(ccrs, t, icon);
        getBulbModel(part.getSide()).render(ccrs, t, icon, getColourMultiplier(part.getColor(), part.isLightOn()));
    }

    @OnlyIn(Dist.CLIENT)
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

    @OnlyIn(Dist.CLIENT)
    public IItemRenderer getItemRenderer() {
        return new IItemRenderer() {
            @Override
            public void renderItem(ItemStack stack, ItemTransforms.TransformType transformType, PoseStack mStack, MultiBufferSource getter, int packedLight, int packedOverlay) {
                if (!(stack.getItem() instanceof MultipartLightPartItem lightItem)) return;

                CCRenderState ccrs = CCRenderState.instance();
                ccrs.reset();
                ccrs.brightness = packedLight;
                ccrs.overlay = packedOverlay;
                ccrs.bind(RenderType.cutout(), getter, mStack);
                renderInventory(lightItem.getColor(), lightItem.isInverted(), Vector3.ZERO, ccrs);

                if (lightItem.isInverted()) {
                    HaloRenderer.renderInventoryHalo(ccrs, mStack, getter, getInventoryGlowBounds(), lightItem.getColor(), Vector3.ZERO);
                }
            }

            //@formatter:off
            @Override public ModelState getModelTransform() { return TransformUtils.DEFAULT_BLOCK; }
            @Override public boolean useAmbientOcclusion() { return false; }
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
        Map<String, CCModel> models = new OBJParser(new ResourceLocation(MOD_ID, "obj/" + name + ".obj"))
                .ignoreMtl()
                .quads()
                .parse();

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
