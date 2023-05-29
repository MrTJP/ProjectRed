package mrtjp.projectred.illumination.client;

import codechicken.lib.model.bakedmodels.WrappedItemModel;
import codechicken.lib.render.BlockRenderer;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.item.IItemRenderer;
import codechicken.lib.util.TransformUtils;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Vector3;
import codechicken.lib.vec.uv.IconTransformation;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.core.client.HaloRenderer;
import mrtjp.projectred.illumination.block.IllumarLampBlock;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.model.ItemTransforms;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.BakedModel;
import net.minecraft.client.resources.model.ModelState;
import net.minecraft.core.Direction;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;

import java.util.Random;

public class IllumarLampItemRenderer extends WrappedItemModel implements IItemRenderer {

    private static final Cuboid6 BLOCK_BOUNDS = Cuboid6.full.copy().expand(-0.02D);
    private static final Cuboid6 GLOW_BOUNDS = Cuboid6.full.copy().expand(0.02D);
    private static final Random random = new Random();

    public IllumarLampItemRenderer(BakedModel wrapped) {
        super(wrapped);
    }

    @Override
    public void renderItem(ItemStack stack, ItemTransforms.TransformType transformType, PoseStack mStack, MultiBufferSource getter, int packedLight, int packedOverlay) {
        Item item = stack.getItem();
        if (!(item instanceof BlockItem)) return;

        BlockItem blockItem = (BlockItem) item;
        if (!(blockItem.getBlock() instanceof IllumarLampBlock)) return;

        IllumarLampBlock block = (IllumarLampBlock) blockItem.getBlock();
        if (!block.isInverted()) {
            // Non-inverted blocks can use default Minecraft BlockItem model. Shouldn't happen
            // because this renderer should only be registered to inverted variants.
            renderWrapped(stack, mStack, getter, packedLight, packedOverlay, false);
            return;
        }

        // Render actual block. Required because renderWrapped does not play nice with
        // halo rendering. Halo completely obscures wrapped render.

        // Obtain texture from original block model
        TextureAtlasSprite icon = wrapped.getQuads(null, Direction.UP, random).get(0).getSprite();

        // Render block
        CCRenderState ccrs = CCRenderState.instance();
        ccrs.reset();
        ccrs.brightness = packedLight;
        ccrs.overlay = packedOverlay;
        ccrs.bind(RenderType.cutout(), getter, mStack);

        ccrs.setPipeline(new IconTransformation(icon));
        BlockRenderer.renderCuboid(ccrs, BLOCK_BOUNDS, 0);

        // Render halo
        HaloRenderer.renderInventoryHalo(ccrs, mStack, getter, GLOW_BOUNDS, block.getColor(), Vector3.ZERO);
    }

    @Override
    public ModelState getModelTransform() {
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
