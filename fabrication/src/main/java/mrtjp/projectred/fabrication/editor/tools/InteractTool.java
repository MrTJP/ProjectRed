package mrtjp.projectred.fabrication.editor.tools;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.editor.ICEditorToolType;
import mrtjp.projectred.fabrication.engine.BaseTile;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.network.chat.Component;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.lwjgl.glfw.GLFW;

import javax.annotation.Nullable;
import java.util.List;
import java.util.Optional;

import static mrtjp.projectred.fabrication.editor.tools.IICEditorTool.internalToGlobalCuboid;
import static mrtjp.projectred.fabrication.editor.tools.IICEditorTool.toNearestPosition;

public class InteractTool extends BaseICEditorTool {

    private final Vector3 initialMouseDown = new Vector3();
    private boolean leftMouseDown;
    private boolean rightMouseDown;

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
            var zone =  t.getInteractionZones()[z];

            if (leftClick) {
                zone.onLeftClick();
            }
            else {
                zone.onRightClick();
            }
        });
    }

    private void executeTool(Vector3 startMouseDown, Vector3 endMouseDown) {
        TileCoord start = toNearestPosition(startMouseDown);
        TileCoord end = toNearestPosition(endMouseDown);
        if (!start.equals(end)) return;

        var endZone = getMouseoverZone(endMouseDown);
        if (endZone == null) return;

        var startZone = getMouseoverZone(startMouseDown);
        if (!endZone.equals(startZone)) return;

        byte b = (byte) ((endZone.index & 0x7F) | (leftMouseDown ? 0x80 : 0x00));

        getEditor().getToolStream(this)
                .writeByte(start.x).writeByte(start.y).writeByte(start.z)
                .writeByte(b);
    }

    @Override
    public boolean toolStart(Vector3 mousePosition, int glfwMouseButton) {
        if (glfwMouseButton == GLFW.GLFW_MOUSE_BUTTON_LEFT) {
            leftMouseDown = true;
            rightMouseDown = false;
            initialMouseDown.set(mousePosition);
            return true;
        } else if (glfwMouseButton == GLFW.GLFW_MOUSE_BUTTON_RIGHT) {
            rightMouseDown = true;
            leftMouseDown = false;
            initialMouseDown.set(mousePosition);
            return true;
        }
        return false;
    }

    @Override
    public boolean toolReleased(Vector3 mousePosition, int glfwMouseButton) {
        if ((glfwMouseButton == GLFW.GLFW_MOUSE_BUTTON_LEFT && leftMouseDown) ||
                (glfwMouseButton == GLFW.GLFW_MOUSE_BUTTON_RIGHT && rightMouseDown)) {
            executeTool(initialMouseDown, mousePosition);
            leftMouseDown = false;
            rightMouseDown = false;
            return true;
        }
        return false;
    }

    @Override
    public boolean toolDragged(Vector3 mousePosition, Vector3 delta, int glfwMouseButton) {
        if (glfwMouseButton == GLFW.GLFW_MOUSE_BUTTON_LEFT) {
            return leftMouseDown;
        }
        if (glfwMouseButton == GLFW.GLFW_MOUSE_BUTTON_RIGHT) {
            return rightMouseDown;
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
    public boolean toolKeyPressed(Vector3 mousePosition, int glfwKeyCode, int glfwFlags) {

        TileCoord coord = toNearestPosition(mousePosition);
        BaseTile tile = getEditor().getTileMap().getBaseTile(coord).orElse(null);
        if (tile == null) return false;

        for (var zone : tile.getInteractionZones()) {
            if (zone.onKeyPressed(glfwKeyCode, glfwFlags)) {
                return true;
            }
        }

        return false;
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void buildTooltip(Vector3 mousePosition, boolean isFirstHit, List<Component> tooltip) {
        super.buildTooltip(mousePosition, isFirstHit, tooltip);

        if (!isFirstHit) return;

        var zone = getMouseoverZone(mousePosition);
        if (zone == null) {
            return;
        }

        zone.zone.buildToolTip(tooltip);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void renderOverlay(Vector3 mousePosition, boolean isFirstHit, CCRenderState ccrs, MultiBufferSource getter, PoseStack matrixStack) {

        if (!isFirstHit) return;

        // Selection box for entire tile area
        Vector3 a = (leftMouseDown || rightMouseDown) ? initialMouseDown : mousePosition;
        Vector3 b = mousePosition;

        // Render tile interaction zone
        TileCoord coord = toNearestPosition(a);
        getEditor().getTileMap().getBaseTile(coord).ifPresent(tile -> {

            InteractionZone[] zones2 = tile.getInteractionZones();
            if (zones2.length == 0) return;

            matrixStack.pushPose();
            matrixStack.translate(coord.x, coord.y, coord.z);
            for (InteractionZone zone : zones2) {
                Cuboid6 box = internalToGlobalCuboid(coord, zone.getBounds());
                boolean isSelected = (leftMouseDown || rightMouseDown) && box.contains(a);
                boolean IsMouseover = box.contains(b);
                zone.renderZone(ccrs, getter, matrixStack, isSelected, IsMouseover);
            }
            matrixStack.popPose();
        });
    }

    private TileCoord getTargetTileCoord(Vector3 mousePosition) {
        return toNearestPosition((leftMouseDown || rightMouseDown) ? initialMouseDown : mousePosition);
    }

    private @Nullable ZoneIndexPair getMouseoverZone(Vector3 mousePosition) {
        TileCoord coord = getTargetTileCoord(mousePosition);
        BaseTile tile = getEditor().getTileMap().getBaseTile(coord).orElse(null);

        if (tile == null) return null;

        // Local mouse pos relative to tile
        Vector3 localMousePos = mousePosition.copy().subtract(coord.x, coord.y, coord.z);

        InteractionZone[] zones = tile.getInteractionZones();
        for (int i = 0; i < zones.length; i++) {
            var zone = zones[i];
            if (zone.getBounds().contains(localMousePos)) return new ZoneIndexPair(zone, i);
        }

        return null;
    }

    private record ZoneIndexPair(InteractionZone zone, int index) {
    }
}
