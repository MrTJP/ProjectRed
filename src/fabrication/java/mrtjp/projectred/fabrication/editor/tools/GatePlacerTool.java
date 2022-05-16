package mrtjp.projectred.fabrication.editor.tools;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.math.MathHelper;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.editor.GatePlacementType;
import mrtjp.projectred.fabrication.editor.ICEditorToolType;
import mrtjp.projectred.fabrication.engine.gates.ICGateTileType;
import mrtjp.projectred.fabrication.engine.gates.GateTile;
import mrtjp.projectred.fabrication.gui.ICRenderTypes;
import mrtjp.projectred.integration.RenderGate;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraft.util.text.ITextProperties;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.lwjgl.glfw.GLFW;

import java.util.List;

import static mrtjp.projectred.fabrication.editor.tools.IICEditorTool.toNearestPosition;

public class GatePlacerTool extends BaseICEditorTool {

    private final Vector3 initialMouseDown = new Vector3();
    private boolean leftMouseDown;

    // Tool config
    private boolean overwrite = false;
    private ICGateTileType gateType = ICGateTileType.OR;

    public ICGateTileType getGateType() {
        return gateType;
    }

    public void setGateType(ICGateTileType gateType) {
        this.gateType = gateType;
        leftMouseDown = false;
    }

    @Override
    public ICEditorToolType getToolType() {
        return ICEditorToolType.GATE_PLACER_TOOL;
    }

    @Override
    public void readPacket(MCDataInput input) {

        boolean overwrite = input.readBoolean();
        ICGateTileType gateType = ICGateTileType.values()[input.readUByte()];
        int x = input.readByte();
        int y = input.readByte();
        int z = input.readByte();
        int r = input.readUByte();

        TileCoord pos = new TileCoord(x, y, z);
        if (!editor.getTileMap().isInBounds(pos)) return;
        if (editor.getTileMap().getTile(pos).isPresent()) {
            if (!overwrite) return;
            editor.getTileMap().removeTile(pos);
        }

        GateTile t = (GateTile) gateType.tileType.create();
        t.preparePlacement(r);
        editor.addTile(t, pos);
    }

    private void sendPlaceGatePacket(TileCoord pos, int r) {
        MCDataOutput out = editor.getToolStream(this);
        out.writeBoolean(overwrite);
        out.writeByte(gateType.ordinal());

        out.writeByte(pos.x).writeByte(pos.y).writeByte(pos.z);
        out.writeByte(r);
    }

    private boolean isValidStart(TileCoord pos) {

        if (!editor.getTileMap().isInBounds(pos)) return false;
        if (editor.getTileMap().getTile(pos).isPresent() && !overwrite) return false;

        if (gateType.placementType == GatePlacementType.INTERNAL && !isInBody(pos)) return false;
        if (gateType.placementType == GatePlacementType.IO_EDGE && !isOnIOEdge(pos)) return false;

        return true;
    }

    @Override
    public boolean toolStart(Vector3 mousePosition, int glfwMouseButton) {
        if (glfwMouseButton == GLFW.GLFW_MOUSE_BUTTON_LEFT) {
            TileCoord start = toNearestPosition(mousePosition);
            if (isValidStart(start)) {
                leftMouseDown = true;
                initialMouseDown.set(mousePosition);
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean toolReleased(Vector3 mousePosition, int glfwMouseButton) {
        if (glfwMouseButton == GLFW.GLFW_MOUSE_BUTTON_LEFT && leftMouseDown) {
            leftMouseDown = false;
            TileCoord posA = toNearestPosition(initialMouseDown);
            sendPlaceGatePacket(posA, getPlacementRotation(initialMouseDown, mousePosition));
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
    public void toolCanceled(Vector3 mousePosition) {
        leftMouseDown = false;
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
    public void renderOverlay(Vector3 mousePosition, boolean isFirstHit, CCRenderState ccrs, IRenderTypeBuffer getter, MatrixStack matrixStack) {

        if (!isFirstHit) return;

        ccrs.reset();
        ccrs.bind(ICRenderTypes.layersRenderType, Minecraft.getInstance().renderBuffers().bufferSource(), matrixStack);

        if (!leftMouseDown) {
            ccrs.alphaOverride = 64;
            TileCoord mousePos = toNearestPosition(mousePosition);
            int rot = getHoverRotation(mousePosition);
            if (isValidStart(mousePos)) renderHover(mousePos, rot, ccrs);
        } else {
            ccrs.alphaOverride = 128;
            TileCoord placementPos = toNearestPosition(initialMouseDown);
            int rot = getPlacementRotation(initialMouseDown, mousePosition);
            renderHover(placementPos, rot, ccrs);
        }
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void buildTooltip(Vector3 mousePosition, boolean isFirstHit, List<ITextProperties> tooltip) {
        super.buildTooltip(mousePosition, isFirstHit, tooltip);
    }

    public void renderHover(TileCoord pos, int r, CCRenderState ccrs) {
        RenderGate.instance().renderInv(gateType.renderIndex, r, new Translation(pos.x, pos.y, pos.z), ccrs);
    }

    private int getHoverRotation(Vector3 hoverPos) {
        if (gateType.placementType == GatePlacementType.IO_EDGE) {
            return getEdgeRotation(toNearestPosition(hoverPos));
        }
        return 0;
    }

    private int getPlacementRotation(Vector3 initialPos, Vector3 mousePos) {
        if (gateType.placementType == GatePlacementType.IO_EDGE) {
            return getEdgeRotation(toNearestPosition(initialPos));
        }
        // TODO this can be done by comparing x and z components of the initial -> mouse vector

        Vector3 i = initialPos.copy().floor().add(0.5); // Move initial to center of its closest block space
        Vector3 a = mousePos.copy().subtract(i);
        a.y = 0; // Project onto XY
        Vector3 b = new Vector3(1, 0, 0);

        if (a.mag() < 1.2) return 0; // Don't rotate if mouse is too close

        double angle = b.angle(a); // Angle between 0 - pi
        if (a.z > 0) angle = (2 * MathHelper.pi) - angle; // Full circular angle 0 - 2*pi
        angle = (angle + 1 / 4D * MathHelper.pi) % (2 * MathHelper.pi); // Add 45 degrees
        angle = 2 * MathHelper.pi - angle; // Invert, because rotations are counter-clockwise

        // Convert to rotation
        double percentage = angle / (2 * MathHelper.pi);
        return ((int) (4 * percentage) + 2) % 4;
    }

    private int getEdgeRotation(TileCoord pos) {
        if (pos.z == editor.getTileMap().getMinBounds().z) return 0;
        if (pos.x == editor.getTileMap().getMaxBounds().x) return 1;
        if (pos.z == editor.getTileMap().getMaxBounds().z) return 2;
        if (pos.x == editor.getTileMap().getMinBounds().x) return 3;

        return 0;
    }
}
