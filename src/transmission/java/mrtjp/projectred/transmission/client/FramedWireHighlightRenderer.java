package mrtjp.projectred.transmission.client;

import codechicken.lib.render.CCRenderState;
import codechicken.microblock.CommonMicroFactory;
import codechicken.microblock.IMicroHighlightRenderer;
import codechicken.microblock.api.MicroMaterial;
import codechicken.multipart.block.BlockMultiPart;
import codechicken.multipart.block.TileMultiPart;
import codechicken.multipart.util.PartRayTraceResult;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.projectred.transmission.part.BaseCenterWirePart;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.Hand;
import net.minecraft.util.math.BlockRayTraceResult;

public class FramedWireHighlightRenderer implements IMicroHighlightRenderer {

    public static final FramedWireHighlightRenderer INSTANCE = new FramedWireHighlightRenderer();

    private FramedWireHighlightRenderer() {
    }

    @Override
    public boolean renderHighlight(PlayerEntity player, Hand hand, BlockRayTraceResult hit, CommonMicroFactory mcrFactory, int size, MicroMaterial material, MatrixStack mStack, IRenderTypeBuffer getter, float partialTicks) {

        if (mcrFactory.getFactoryID() != 0 || size != 1 || player.isCrouching() || material.isTransparent()) {
            return false;
        }

        if (!(hit instanceof PartRayTraceResult)) return false;
        PartRayTraceResult partHit = (PartRayTraceResult) hit;

        TileMultiPart tile = BlockMultiPart.getTile(player.level, hit.getBlockPos());
        if (tile == null) return false;

        if (partHit.part instanceof BaseCenterWirePart) {
            BaseCenterWirePart wire = (BaseCenterWirePart) partHit.part;
            if (wire.getMaterial() == null || wire.getMaterial() != material) {
                FramedWireModelRenderer.renderCoverHighlight(wire, material, CCRenderState.instance(), mStack, getter);
                return true;
            }
        }

        return false;
    }
}
