package mrtjp.projectred.illumination.part;

import codechicken.lib.raytracer.VoxelShapeCache;
import codechicken.lib.render.CCModel;
import codechicken.lib.texture.AtlasRegistrar;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Vector3;
import mrtjp.projectred.illumination.MultipartLightProperties;
import mrtjp.projectred.illumination.MultipartLightType;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.Map;

import static mrtjp.projectred.illumination.ProjectRedIllumination.MOD_ID;

public class LanternLightProperties extends MultipartLightProperties {

    private static final Cuboid6 BOUNDS = new Cuboid6(0.35D, 0.25D, 0.35D, 0.65D, 0.75D, 0.65D);
    private static final Cuboid6 GLOW_BOUNDS = BOUNDS.copy().expand(-1/64D);
    private static final VoxelShape SHAPE =  VoxelShapeCache.getShape(BOUNDS);

    private @Nullable TextureAtlasSprite icon;

    @Override
    public VoxelShape getShape(int side) {
        return SHAPE;
    }

    @Override
    public ItemStack makeStack(int color, boolean inverted) {
        return MultipartLightType.LANTERN.makeStack(color, inverted);
    }

    @Override
    public MultipartLightPart partFactory(int color, boolean inverted) {
        return new MultipartLightFacePart(MultipartLightType.LANTERN.getPartType(color, inverted), this, color, inverted);
    }

    //region Rendering
    @Override
    @OnlyIn(Dist.CLIENT)
    public void registerIcons(AtlasRegistrar registrar) {
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/lantern"), i -> icon = i);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public TextureAtlasSprite getIcon(int color) {
        assert icon != null;
        return icon;
    }

    @Override
    public Cuboid6 getGlowBounds(int side) {
        return GLOW_BOUNDS;
    }

    @Override
    public CCModel getBulbModel(int side) {
        return LanternLightModels.BULB_MODEL;
    }

    @Override
    public CCModel getChasisModel(int side) {
        return LanternLightModels.CHASSIS_MODELS[side];
    }
    //endregion

    private static class LanternLightModels {

        private static final CCModel BULB_MODEL;
        private static final CCModel[] CHASSIS_MODELS = new CCModel[6];

        static {
            Map<String, CCModel> models = parseCorrectedModel("lantern");

            CCModel bulb = models.get("bulb");
            CCModel body = models.get("body");
            CCModel top = models.get("standtop");
            CCModel topRing = models.get("goldringtop");
            CCModel bottom = models.get("standbottom");
            CCModel bottomRing = models.get("goldringbottom");
            CCModel side = models.get("standside");

            BULB_MODEL = bulb.copy();

            // Ceiling/ground models
            CHASSIS_MODELS[0] = CCModel.combine(Arrays.asList(body, bottom, bottomRing));
            CHASSIS_MODELS[1] = CCModel.combine(Arrays.asList(body, top, topRing));

            // Wall models
            for (int s = 2; s < 6; s++) {
                CCModel sideModel = side.copy().apply(Rotation.sideOrientation(0, Rotation.rotationTo(0, s)).at(Vector3.CENTER));
                CCModel ringModel = topRing.copy().apply(Rotation.sideOrientation(0, Rotation.rotationTo(0, s)).at(Vector3.CENTER));
                CHASSIS_MODELS[s] = CCModel.combine(Arrays.asList(body, sideModel, ringModel));
            }

            finishModel(BULB_MODEL);
            for (CCModel m : CHASSIS_MODELS) {
                finishModel(m);
            }
        }
    }
}
