package mrtjp.projectred.illumination.part;

import codechicken.lib.render.CCModel;
import codechicken.lib.texture.AtlasRegistrar;
import codechicken.lib.vec.Cuboid6;
import mrtjp.projectred.illumination.MultipartLightProperties;
import mrtjp.projectred.illumination.MultipartLightType;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import java.util.Map;

import static mrtjp.projectred.illumination.ProjectRedIllumination.MOD_ID;

public class CageLightProperties extends MultipartLightProperties {

    private static final Cuboid6[] BOUNDS = sidedBoxes(new Cuboid6(3.5/16D, 0, 3.5/16D, 12.5/16D, 12/16D, 12.5/16D));
    private static final Cuboid6[] GLOW_BOUNDS = sidedBoxes(new Cuboid6(4.5/16D, 1.5/16, 4.5/16D, 11.5/16D, 11.5/16D, 11.5/16D));
    private static final VoxelShape[] SHAPES = boxesToShapes(BOUNDS);

    private TextureAtlasSprite icon;

    @Override
    public VoxelShape getShape(int side) {
        return SHAPES[side];
    }

    @Override
    public ItemStack makeStack(int color, boolean inverted) {
        return MultipartLightType.CAGE.makeStack(color, inverted);
    }

    @Override
    public MultipartLightPart partFactory(int color, boolean inverted) {
        return new MultipartLightFacePart(MultipartLightType.CAGE.getPartType(color, inverted), this, color, inverted);
    }

    //region Rendering
    @Override
    @OnlyIn(Dist.CLIENT)
    public void registerIcons(AtlasRegistrar registrar) {
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/cage_lamp"), i -> icon = i);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public TextureAtlasSprite getIcon(int color) {
        return icon;
    }

    @Override
    public Cuboid6 getGlowBounds(int side) {
        return GLOW_BOUNDS[side];
    }

    @Override
    public CCModel getBulbModel(int side) {
        return CageLightModels.BULB_MODELS[side];
    }

    @Override
    public CCModel getChasisModel(int side) {
        return CageLightModels.CHASSIS_MODELS[side];
    }
    //endregion

    private static class CageLightModels {
        private static final CCModel[] BULB_MODELS = new CCModel[6];
        private static final CCModel[] CHASSIS_MODELS = new CCModel[6];

        static {
            Map<String, CCModel> models = parseCorrectedModel("cagelamp");
            CCModel chassis = models.get("chassi");
            CCModel bulb = models.get("bulb");

            for (int s = 0; s < 6; s++) {
                BULB_MODELS[s] = bakeCopy(s, bulb);
                CHASSIS_MODELS[s] = bakeCopy(s, chassis);
            }
        }
    }
}
