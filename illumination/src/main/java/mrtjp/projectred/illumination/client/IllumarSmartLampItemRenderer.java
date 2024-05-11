package mrtjp.projectred.illumination.client;

import codechicken.lib.model.PerspectiveModelState;
import codechicken.lib.model.bakedmodels.WrappedItemModel;
import codechicken.lib.render.BlockRenderer;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.item.IItemRenderer;
import codechicken.lib.util.TransformUtils;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Vector3;
import codechicken.lib.vec.uv.MultiIconTransformation;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.core.client.HaloRenderer;
import mrtjp.projectred.illumination.block.IllumarSmartLampBlock;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.model.ItemTransforms;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.core.Direction;
import net.minecraft.util.RandomSource;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.Nullable;

public class IllumarSmartLampItemRenderer extends WrappedItemModel implements IItemRenderer {

    private static final Cuboid6 BLOCK_BOUNDS = Cuboid6.full.copy().expand(-0.02D);
    private static final Cuboid6 GLOW_BOUNDS = Cuboid6.full.copy().expand(0.02D);
    private static final RandomSource random = RandomSource.create();

    private final byte[] signal = new byte[16];
    private long lastSignalAnimateTime = -1L;

    public IllumarSmartLampItemRenderer(BakedModel wrapped) {
        super(wrapped);
    }

    @Override
    public void renderItem(ItemStack stack, ItemTransforms.TransformType transformType, PoseStack mStack, MultiBufferSource getter, int packedLight, int packedOverlay) {
        Item item = stack.getItem();
        if (!(item instanceof BlockItem blockItem)) return;

        if (!(blockItem.getBlock() instanceof IllumarSmartLampBlock block)) return;

        // Render actual block. Required because renderWrapped does not play nice with
        // halo rendering. Halo completely obscures wrapped render.

        // Obtain texture from original block model
        TextureAtlasSprite[] icons = new TextureAtlasSprite[6];
        for (Direction dir : Direction.values()) {
            icons[dir.get3DDataValue()] = wrapped.getQuads(null, dir, random).get(0).getSprite();
        }
        MultiIconTransformation iconT = new MultiIconTransformation(icons);


        // Render block
        CCRenderState ccrs = CCRenderState.instance();
        ccrs.reset();
        ccrs.brightness = packedLight;
        ccrs.overlay = packedOverlay;
        ccrs.bind(RenderType.cutout(), getter, mStack);

        ccrs.setPipeline(iconT);
        BlockRenderer.renderCuboid(ccrs, BLOCK_BOUNDS, 0);

        // Animate signals
        animateSignal();

        // Render halo
        HaloRenderer.renderInventoryMultiHalo(ccrs, mStack, getter, GLOW_BOUNDS, Vector3.ZERO, signal);
        HaloRenderer.addItemRendererMultiBloom(transformType, mStack, Vector3.ZERO, GLOW_BOUNDS, signal);
    }

    private void animateSignal() {

        Level level = Minecraft.getInstance().level;
        long time = level != null ? level.getGameTime() : System.currentTimeMillis() / 50L; // approximate time progression if no level

        // Only do this once per tick
        if (time == lastSignalAnimateTime) return;
        lastSignalAnimateTime = time;

        // Sine-wave animation
        double t = (Math.sin(time / 200.0) + 1.0) / 2.0 * 15.0; // Sine wave with bounds [0, 15]
        double d = 1.5; // Max distance (max active colours / 2)

        for (int i = 0; i < 16; i++) {
            double diff = Math.min(Math.abs(t - i), d);
            double brightness = 1.0 - diff / d;
            signal[i] = (byte) (255 * brightness);
        }
    }

    @Override
    public @Nullable PerspectiveModelState getModelState() {
        return TransformUtils.DEFAULT_BLOCK;
    }

    @Override
    public boolean useAmbientOcclusion() {
        return true;
    }

    @Override
    public boolean isGui3d() {
        return true;
    }

    @Override
    public boolean usesBlockLight() {
        return true;
    }

}
