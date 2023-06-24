package mrtjp.projectred.transmission.client;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.item.IItemRenderer;
import codechicken.lib.util.TransformUtils;
import codechicken.lib.vec.RedundantTransformation;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.transmission.WireType;
import mrtjp.projectred.transmission.item.FaceWirePartItem;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.model.ItemTransforms;
import net.minecraft.client.resources.model.ModelState;
import net.minecraft.world.item.ItemStack;

public class WirePartItemRenderer implements IItemRenderer {

    public static final WirePartItemRenderer INSTANCE = new WirePartItemRenderer();

    private WirePartItemRenderer() { }

    @Override
    public boolean useAmbientOcclusion() {
        return false;
    }

    @Override
    public boolean isGui3d() {
        return true;
    }

    @Override
    public boolean usesBlockLight() {
        return true;
    }

    @Override
    public ModelState getModelTransform() {
        return TransformUtils.DEFAULT_BLOCK;
    }

    @Override
    public void renderItem(ItemStack stack, ItemTransforms.TransformType transformType, PoseStack mStack, MultiBufferSource getter, int packedLight, int packedOverlay) {

        if (!(stack.getItem() instanceof FaceWirePartItem)) return;

        WireType type = ((FaceWirePartItem) stack.getItem()).getType();

        CCRenderState ccrs = CCRenderState.instance();
        ccrs.reset();
        ccrs.brightness = packedLight;
        ccrs.overlay = packedOverlay;
        ccrs.bind(RenderType.cutout(), getter, mStack);

        WireModelRenderer.renderInventory(ccrs, type.getThickness(), type.getItemColour() << 8 | 0xFF, type.getTextures().get(0), RedundantTransformation.INSTANCE);
    }
}
