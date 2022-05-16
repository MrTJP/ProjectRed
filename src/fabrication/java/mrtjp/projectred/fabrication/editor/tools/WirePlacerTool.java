package mrtjp.projectred.fabrication.editor.tools;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.editor.ICEditorToolType;
import mrtjp.projectred.fabrication.engine.wires.ICWireTileType;
import mrtjp.projectred.fabrication.engine.IRotatableICTile;
import mrtjp.projectred.fabrication.engine.wires.WireTile;
import mrtjp.projectred.fabrication.gui.ICRenderTypes;
import mrtjp.projectred.transmission.RenderWire;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.lwjgl.glfw.GLFW;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import static mrtjp.projectred.fabrication.editor.tools.IICEditorTool.toNearestPosition;

public class WirePlacerTool extends BaseICEditorTool {

    private final Vector3 initialMouseDown = new Vector3();
    private final List<TileCoord> selectedPositionsList = new LinkedList<>();
    private final Set<TileCoord> selectedPositions = new HashSet<>();

    private boolean leftMouseDown;

    // Tool config
    private boolean overwrite = false;
    private ICWireTileType wireType = ICWireTileType.RED_ALLOY;

    public void setWireType(ICWireTileType wireType) {
        this.wireType = wireType;
    }

    public ICWireTileType getWireType() {
        return wireType;
    }

    @Override
    public ICEditorToolType getToolType() {
        return ICEditorToolType.WIRE_PLACER_TOOL;
    }

    @Override
    public void readPacket(MCDataInput in) {

        boolean overwrite = in.readBoolean();
        ICWireTileType wireType = ICWireTileType.values()[in.readUByte()];
        int numPositions = in.readUByte();
        int y = in.readByte();

        for (int i = 0; i < numPositions; i++) {
            TileCoord pos = new TileCoord(in.readByte(), y, in.readByte());

            if (editor.getTileMap().getTile(pos).isPresent()) {
                if (!overwrite) continue;
                editor.removeTile(pos);
            }

            WireTile tile = (WireTile) wireType.tileType.create();
            editor.addTile(tile, pos);
        }
    }

    protected void sendPlaceWires() {
        if (selectedPositionsList.isEmpty()) return;

        MCDataOutput out = editor.getToolStream(this);
        out.writeBoolean(overwrite);
        out.writeByte(wireType.ordinal());
        out.writeByte(selectedPositionsList.size());
        out.writeByte(selectedPositionsList.get(0).y); // All on same layer

        for (TileCoord t : selectedPositionsList) {
            out.writeByte(t.x).writeByte(t.z);
        }
    }

    protected void addToSelection(Vector3 mousePosition) {
        TileCoord pos = toNearestPosition(mousePosition);
        if (!selectedPositions.contains(pos) && !editor.getTileMap().getBaseTile(pos).isPresent()) {
            selectedPositions.add(pos);
            selectedPositionsList.add(pos);
        }
    }

    @Override
    public boolean toolStart(Vector3 mousePosition, int glfwMouseButton) {
        if (glfwMouseButton == GLFW.GLFW_MOUSE_BUTTON_LEFT) {
            leftMouseDown = true;
            initialMouseDown.set(mousePosition);
            addToSelection(mousePosition);
            return true;
        }
        return false;
    }

    @Override
    public boolean toolReleased(Vector3 mousePosition, int glfwMouseButton) {
        if (glfwMouseButton == GLFW.GLFW_MOUSE_BUTTON_LEFT && leftMouseDown) {
            addToSelection(mousePosition);
            sendPlaceWires();

            leftMouseDown = false;
            selectedPositions.clear();
            selectedPositionsList.clear();

            return true;
        }
        return false;
    }

    @Override
    public boolean toolDragged(Vector3 mousePosition, Vector3 delta, int glfwMouseButton) {
        if (glfwMouseButton == GLFW.GLFW_MOUSE_BUTTON_LEFT && leftMouseDown) {
            addToSelection(mousePosition);
            return true;
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
            int diff = newLayer - previousLayer;

            // Shift entire selection to the new layer
            initialMouseDown.y += diff;

            List<TileCoord> oldSelection = new LinkedList<>(selectedPositionsList);
            selectedPositions.clear();
            selectedPositionsList.clear();

            for (TileCoord t : oldSelection) {
                TileCoord newPos = t.add(0, diff, 0);
                selectedPositions.add(newPos);
                selectedPositionsList.add(newPos);
            }
        }
    }

    @Override
    public void toolCanceled(Vector3 mousePosition) {
        leftMouseDown = false;
        selectedPositions.clear();
        selectedPositionsList.clear();
    }

    @Override
    public void toolActivated() {
        leftMouseDown = false;
        selectedPositions.clear();
        selectedPositionsList.clear();
    }

    @Override
    public void toolDeactivated() {
        leftMouseDown = false;
        selectedPositions.clear();
        selectedPositionsList.clear();
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void renderOverlay(Vector3 mousePosition, boolean isFirstHit, CCRenderState ccrs, IRenderTypeBuffer getter, MatrixStack matrixStack) {

        if (leftMouseDown) {
            addToSelection(mousePosition);
        }

        ccrs.reset();
        ccrs.bind(ICRenderTypes.layersRenderType, Minecraft.getInstance().renderBuffers().bufferSource(), matrixStack);
        ccrs.alphaOverride = leftMouseDown ? 128 : 64;

        if (!leftMouseDown) {
            renderWireAt(toNearestPosition(mousePosition), ccrs, 0);
        }

        for (TileCoord pos : selectedPositionsList) {
            int rmask = 0;
            for (int r = 0; r < 4; r++) {
                int s = IRotatableICTile.rotationToDir(r);
                if (selectedPositions.contains(pos.offset(s))) {
                    rmask |= 1 << r;
                }
            }
            renderWireAt(pos, ccrs, rmask);
        }
    }

    private void renderWireAt(TileCoord pos, CCRenderState ccrs, int rmask) {
        int wireConnMask = rmask << 4;
        int modelKey = RenderWire.modelKey(0, wireType.multipartType.getThickness(), wireConnMask);
//        RenderWire.render(modelKey, (255 & 0xFF) / 2 + 60 << 24 | 0xFF, renderType.getTextures().get(0), ccrs, new Translation(pos.x, pos.y, pos.z));
        RenderWire.render(modelKey, -1, wireType.multipartType.getTextures().get(0), ccrs, new Translation(pos.x, pos.y, pos.z));
    }

}
