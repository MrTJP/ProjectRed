package mrtjp.projectred.fabrication.editor.tools;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.render.BlockRenderer;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.RenderUtils;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Matrix4;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.editor.ICEditorToolType;
import mrtjp.projectred.fabrication.engine.BaseTile;
import mrtjp.projectred.fabrication.gui.ICRenderTypes;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.network.chat.Component;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.lwjgl.glfw.GLFW;

import javax.annotation.Nullable;
import java.util.List;
import java.util.Optional;

import static mrtjp.projectred.fabrication.editor.tools.IICEditorTool.*;

public class InteractTool extends BaseICEditorTool {

    private final Vector3 initialMouseDown = new Vector3();
    private boolean leftMouseDown;

    @Override
    public ICEditorToolType getToolType() {
        return ICEditorToolType.INTERACT_TOOL;
    }

    @Override
    public void readPacket(MCDataInput input) {

        TileCoord pos = new TileCoord(input.readByte(), input.readByte(), input.readByte());
        byte b = input.readByte();

        int z = b & 0x7F;
        boolean leftClick = (b & 0x80) != 0;

        Optional<BaseTile> tile = getEditor().getTileMap().getBaseTile(pos);
        tile.ifPresent(t -> {
            if (leftClick)
                t.onInteractionZoneClicked(z);
            else
                t.onInteractionZoneActivated(z);
        });
    }

    private void executeTool(Vector3 startMouseDown, Vector3 endMouseDown) {
        TileCoord start = toNearestPosition(startMouseDown);
        TileCoord end = toNearestPosition(endMouseDown);
        if (!start.equals(end)) return;

        InteractionZone zone = getTargetInteractionZone(endMouseDown);
        if (zone == null) return;

        byte b = (byte) ((zone.index & 0x7F) | 0x80); //TODO right clicky

        getEditor().getToolStream(this)
                .writeByte(start.x).writeByte(start.y).writeByte(start.z)
                .writeByte(b);
    }

    @Override
    public boolean toolStart(Vector3 mousePosition, int glfwMouseButton) {
        if (glfwMouseButton == GLFW.GLFW_MOUSE_BUTTON_LEFT) {
            leftMouseDown = true;
            initialMouseDown.set(mousePosition);
            return true;
        }
        return false;
    }

    @Override
    public boolean toolReleased(Vector3 mousePosition, int glfwMouseButton) {
        if (glfwMouseButton == GLFW.GLFW_MOUSE_BUTTON_LEFT && leftMouseDown) {
            leftMouseDown = false;
            executeTool(initialMouseDown, mousePosition);
            return true;
        }
        return false;
    }

    @Override
    public boolean toolDragged(Vector3 mousePosition, Vector3 delta, int glfwMouseButton) {
        if (glfwMouseButton == GLFW.GLFW_MOUSE_BUTTON_LEFT) {
            return leftMouseDown;
        }
        return false;
    }

    @Override
    public boolean toolScrolled(Vector3 mousePosition, double scroll) {
        return false;
    }

    @Override
    public void toolLayerChanged(int previousLayer, int newLayer) {
        if (leftMouseDown) {
            initialMouseDown.y = newLayer;
        }
    }

    @Override
    public boolean toolCanceled() {
        if (leftMouseDown) {
            leftMouseDown = false;
            return true;
        }
        return false;
    }

    @Override
    public void toolActivated() {
        leftMouseDown = false;
    }

    @Override
    public void toolDeactivated() {
        leftMouseDown = false;
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void buildTooltip(Vector3 mousePosition, boolean isFirstHit, List<Component> tooltip) {

        if (!isFirstHit) return;

        InteractionZone zone = getTargetInteractionZone(mousePosition);
        if (zone == null) {
            super.buildTooltip(mousePosition, isFirstHit, tooltip);
            return;
        }

        TileCoord pos = toNearestPosition(mousePosition);
        getEditor().getTileMap().getBaseTile(pos).ifPresent(tile ->
                tile.buildInteractionToolTip(tooltip, zone.index));
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void renderOverlay(Vector3 mousePosition, boolean isFirstHit, CCRenderState ccrs, MultiBufferSource getter, PoseStack matrixStack) {

        if (!isFirstHit) return;

        // Selection box for entire tile area
        Vector3 a = leftMouseDown ? initialMouseDown : mousePosition;
        Vector3 b = mousePosition;
        int alpha = leftMouseDown ? isSamePosition(a, b) ? 255 : 128 : 32;

        ccrs.reset();
        ccrs.bind(ICRenderTypes.selectionRenderType, Minecraft.getInstance().renderBuffers().bufferSource(), matrixStack);
        ccrs.baseColour = EnumColour.LIGHT_BLUE.rgba(alpha);

//        ICRenderTypes.renderSelection(ccrs, a, a, 3 / 16D, 2 / 16D); // TODO Selection box that sits AROUND the tile

        // Render tile interaction zone
        TileCoord coord = toNearestPosition(b);
        getEditor().getTileMap().getBaseTile(coord).ifPresent(tile -> {
            List<Cuboid6> zones = tile.getInteractionZones();

            Cuboid6 currentZoneBox = null;

            // Render the interaction zone overlay
            for (Cuboid6 zone : zones) {
                Cuboid6 box = internalToGlobalCuboid(coord, zone);

                boolean isInitialInZone = box.contains(a);
                boolean isCurrentInZone = box.contains(b);

                if (isCurrentInZone) currentZoneBox = box;

                ccrs.baseColour = EnumColour.LIGHT_BLUE.rgba(leftMouseDown && isInitialInZone ? isCurrentInZone ? 255 : 128 : 32);
                BlockRenderer.renderCuboid(ccrs, box, 1);
            }

            // Render bounding box for interaction zone
            if (currentZoneBox != null) {
                RenderUtils.bufferHitBox(new Matrix4(matrixStack.last().pose()), Minecraft.getInstance().renderBuffers().bufferSource(), currentZoneBox);
            }
        });
    }

    private TileCoord getTargetTileCoord(Vector3 mousePosition) {
        return toNearestPosition(leftMouseDown ? initialMouseDown : mousePosition);
    }

    private @Nullable InteractionZone getTargetInteractionZone(Vector3 mousePosition) {
        TileCoord coord = getTargetTileCoord(mousePosition);
        BaseTile tile = getEditor().getTileMap().getBaseTile(coord).orElse(null);

        if (tile == null) return null;

        int i = 0;
        for (Cuboid6 zone : tile.getInteractionZones()) {
            Cuboid6 box = internalToGlobalCuboid(coord, zone);
            if (box.contains(mousePosition)) return new InteractionZone(coord, zone, i);
            i++;
        }

        return null;
    }

    private static class InteractionZone {
        public final TileCoord tileCoord;
        public final Cuboid6 bounds;
        public final int index;

        public InteractionZone(TileCoord tileCoord, Cuboid6 bounds, int index) {
            this.tileCoord = tileCoord;
            this.bounds = bounds;
            this.index = index;
        }
    }
}
