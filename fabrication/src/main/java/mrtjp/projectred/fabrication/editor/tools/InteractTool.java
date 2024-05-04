package mrtjp.projectred.fabrication.editor.tools;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.editor.ICEditorToolType;
import mrtjp.projectred.fabrication.engine.BaseTile;
import net.covers1624.quack.collection.FastStream;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.network.chat.Component;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.lwjgl.glfw.GLFW;

import javax.annotation.Nullable;
import java.util.List;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;
import static mrtjp.projectred.fabrication.editor.tools.IICEditorTool.internalToGlobalCuboid;
import static mrtjp.projectred.fabrication.editor.tools.IICEditorTool.toNearestPosition;

public class InteractTool extends BaseICEditorTool {

    private static final int KEY_MOUSE_CLICK = 0;
    private static final int KEY_KEY_PRESS = 1;

    private final Vector3 initialMouseDown = new Vector3();
    private boolean leftMouseDown;
    private boolean rightMouseDown;

    @Override
    public ICEditorToolType getToolType() {
        return ICEditorToolType.INTERACT_TOOL;
    }

    @Override
    public void readPacket(MCDataInput input) {
        int key = input.readByte();
        switch (key) {
            case KEY_MOUSE_CLICK -> readClickPacket(input);
            case KEY_KEY_PRESS -> readKeyPressPacket(input);
            default -> LOGGER.error("Received invalid packet type for InteractTool: {}", key);
        }
    }

    private void readClickPacket(MCDataInput input) {
        TileCoord pos = new TileCoord(input.readByte(), input.readByte(), input.readByte());
        byte b = input.readByte();
        int z = b & 0x7F;
        boolean leftClick = (b & 0x80) != 0;

        BaseTile tile = getEditor().getTileMap().getBaseTile(pos).orElse(null);
        if (tile == null) return;

        var zone =  tile.getInteractionZones()[z];

        if (leftClick) {
            zone.onLeftClick();
        } else {
            zone.onRightClick();
        }
    }

    private void readKeyPressPacket(MCDataInput input) {
        TileCoord pos = new TileCoord(input.readByte(), input.readByte(), input.readByte());
        int glfwKeyCode = input.readInt();
        int glfwFlags = input.readInt();

        BaseTile tile = getEditor().getTileMap().getBaseTile(pos).orElse(null);
        if (tile == null) return;

        for (var z : tile.getInteractionZones()) {
            z.onKeyPressed(glfwKeyCode, glfwFlags);
        }
    }

    private void sendClick(Vector3 startMouseDown, Vector3 endMouseDown) {
        TileCoord start = toNearestPosition(startMouseDown);
        TileCoord end = toNearestPosition(endMouseDown);
        if (!start.equals(end)) return;

        var endZone = getMouseoverZone(endMouseDown);
        if (endZone == null) return;

        var startZone = getMouseoverZone(startMouseDown);
        if (!endZone.equals(startZone)) return;

        byte b = (byte) ((endZone.index & 0x7F) | (leftMouseDown ? 0x80 : 0x00));

        getEditor().getToolStream(this)
                .writeByte(KEY_MOUSE_CLICK)
                .writeByte(start.x).writeByte(start.y).writeByte(start.z)
                .writeByte(b);
    }

    private void sendKeyPress(TileCoord pos, int glfwKeyCode, int glfwFlags) {
        getEditor().getToolStream(this)
                .writeByte(KEY_KEY_PRESS)
                .writeByte(pos.x).writeByte(pos.y).writeByte(pos.z)
                .writeInt(glfwKeyCode).writeInt(glfwFlags);
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

            sendClick(initialMouseDown, mousePosition);
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

        if (FastStream.of(tile.getInteractionZones()).anyMatch(z -> z.canRespondToKey(glfwKeyCode, glfwFlags))) {
            sendKeyPress(coord, glfwKeyCode, glfwFlags);
            return true;
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
