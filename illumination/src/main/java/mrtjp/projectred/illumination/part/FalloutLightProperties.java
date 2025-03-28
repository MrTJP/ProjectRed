package mrtjp.projectred.illumination.part;

import codechicken.lib.render.CCModel;
import codechicken.lib.vec.Cuboid6;
import mrtjp.projectred.illumination.MultipartLightProperties;
import mrtjp.projectred.illumination.MultipartLightType;
import net.minecraft.client.renderer.texture.TextureAtlas;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.api.distmarker.OnlyIn;
import net.neoforged.neoforge.client.event.TextureAtlasStitchedEvent;

import javax.annotation.Nullable;
import java.util.Map;

import static mrtjp.projectred.illumination.ProjectRedIllumination.MOD_ID;

public class FalloutLightProperties extends MultipartLightProperties {

    private static final Cuboid6[] BOUNDS = sidedBoxes(new Cuboid6(2/16D, 0, 2/16D, 14/16D, 11/16D, 14/16D));
    private static final Cuboid6[] GLOW_BOUNDS = sidedBoxes(new Cuboid6(4/16D, 1.5/16, 4/16D, 12/16D, 10/16D, 12/16D).expand(-0.002));
    private static final VoxelShape[] SHAPES = boxesToShapes(BOUNDS);

    private @Nullable TextureAtlasSprite icon;

    @Override
    public VoxelShape getShape(int side) {
        return SHAPES[side];
    }

    @Override
    public Cuboid6 getBounds(int side) {
        return BOUNDS[side];
    }

    @Override
    public ItemStack makeStack(int color, boolean inverted) {
        return MultipartLightType.FALLOUT.makeStack(color, inverted);
    }

    @Override
    public MultipartLightPart partFactory(int color, boolean inverted) {
        return new MultipartLightFacePart(MultipartLightType.FALLOUT.getPartType(color, inverted), this, color, inverted);
    }

    //region Rendering
    @Override
    @OnlyIn(Dist.CLIENT)
    public void onTextureStitchEvent(TextureAtlasStitchedEvent event) {
        if (!event.getAtlas().location().equals(TextureAtlas.LOCATION_BLOCKS)) return;
        icon = event.getAtlas().getSprite(new ResourceLocation(MOD_ID, "block/fallout"));
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public TextureAtlasSprite getIcon(int color) {
        assert icon != null;
        return icon;
    }

    @Override
    public Cuboid6 getGlowBounds(int side) {
        return GLOW_BOUNDS[side];
    }

    @Override
    public CCModel getBulbModel(int side) {
        return FalloutLightModels.BULB_MODELS[side];
    }

    @Override
    public CCModel getChasisModel(int side) {
        return FalloutLightModels.CHASSIS_MODELS[side];
    }
    //endregion

    private static class FalloutLightModels {
        private static final CCModel[] BULB_MODELS = new CCModel[6];
        private static final CCModel[] CHASSIS_MODELS = new CCModel[6];

        static {
            Map<String, CCModel> models = parseCorrectedModel("fallout");
            CCModel chassis = models.get("chassi");
            CCModel bulb = models.get("bulb");

            for (int s = 0; s < 6; s++) {
                BULB_MODELS[s] = bakeCopy(s, bulb);
                CHASSIS_MODELS[s] = bakeCopy(s, chassis);
            }
        }
    }
}
