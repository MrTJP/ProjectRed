package mrtjp.projectred.integration.client;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.item.IItemRenderer;
import codechicken.lib.util.TransformUtils;
import codechicken.lib.vec.RedundantTransformation;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.projectred.integration.item.GatePartItem;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.model.IModelTransform;
import net.minecraft.client.renderer.model.ItemCameraTransforms;
import net.minecraft.item.ItemStack;

public class GatePartItemRenderer implements IItemRenderer {

    public static final GatePartItemRenderer INSTANCE = new GatePartItemRenderer();

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
    public IModelTransform getModelTransform() {
        return TransformUtils.DEFAULT_BLOCK;
    }

    @Override
    public void renderItem(ItemStack stack, ItemCameraTransforms.TransformType transformType, MatrixStack mStack, IRenderTypeBuffer getter, int packedLight, int packedOverlay) {

        if (!(stack.getItem() instanceof GatePartItem)) return;

        GatePartItem gateItem = (GatePartItem) stack.getItem();

        CCRenderState ccrs = CCRenderState.instance();
        ccrs.reset();
        ccrs.brightness = packedLight;
        ccrs.overlay = packedOverlay;
        ccrs.bind(RenderType.cutout(), getter, mStack);
        GateModelRenderer.instance().renderInventory(ccrs, stack, new RedundantTransformation(), gateItem.getGateType());
    }
}
