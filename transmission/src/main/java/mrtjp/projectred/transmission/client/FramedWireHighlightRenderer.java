package mrtjp.projectred.transmission.client;

import codechicken.lib.render.CCRenderState;
import codechicken.microblock.api.MicroHighlightRenderer;
import codechicken.microblock.api.MicroMaterial;
import codechicken.microblock.init.CBMicroblockModContent;
import codechicken.microblock.part.StandardMicroFactory;
import codechicken.multipart.block.BlockMultipart;
import codechicken.multipart.block.TileMultipart;
import codechicken.multipart.util.PartRayTraceResult;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.transmission.part.BaseCenterWirePart;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.phys.BlockHitResult;

public class FramedWireHighlightRenderer implements MicroHighlightRenderer {

    public static final FramedWireHighlightRenderer INSTANCE = new FramedWireHighlightRenderer();

    private FramedWireHighlightRenderer() {
    }

    @Override
    public boolean renderHighlight(Player player, InteractionHand hand, BlockHitResult hit, StandardMicroFactory mcrFactory, int size, MicroMaterial material, PoseStack mStack, MultiBufferSource getter, float partialTicks) {


        if (mcrFactory != CBMicroblockModContent.FACE_MICROBLOCK_PART.get() || size != 1 || player.isCrouching() || material.isTransparent()) {
            return false;
        }

        if (!(hit instanceof PartRayTraceResult partHit)) return false;

        TileMultipart tile = BlockMultipart.getTile(player.level(), hit.getBlockPos());
        if (tile == null) return false;

        if (partHit.part instanceof BaseCenterWirePart wire) {
            if (wire.getMaterial() == null || wire.getMaterial() != material) {
                FramedWireModelRenderer.renderCoverHighlight(wire, material, CCRenderState.instance(), mStack, getter);
                return true;
            }
        }

        return false;
    }
}
