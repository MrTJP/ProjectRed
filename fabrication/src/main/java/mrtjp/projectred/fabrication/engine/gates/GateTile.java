package mrtjp.projectred.fabrication.engine.gates;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Transformation;
import mrtjp.projectred.fabrication.editor.tools.HotKeyInteractionZone;
import mrtjp.projectred.fabrication.editor.tools.InteractionZone;
import mrtjp.projectred.fabrication.engine.BaseTile;
import mrtjp.projectred.fabrication.engine.IConnectableICTile;
import mrtjp.projectred.fabrication.engine.IRotatableICTile;
import mrtjp.projectred.fabrication.gui.ICRenderTypes;
import mrtjp.projectred.integration.client.GateModelRenderer;
import mrtjp.projectred.integration.part.IGateRenderData;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import org.lwjgl.glfw.GLFW;

import java.util.List;
import java.util.Optional;

import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_REFLECT;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_ROTATE;

public abstract class GateTile extends BaseTile implements IConnectableICTile, IRotatableICTile, IGateRenderData {

    public static final int PACKET_ROTATION = 1;
    public static final int PACKET_SHAPE = 2;

    private byte connMask = 0;
    private byte gateShape = 0;
    private byte gateRotation = 0;

    private final ICGateTileType gateTileType;

    public GateTile(ICGateTileType gateTileType) {
        super(gateTileType.tileType);
        this.gateTileType = gateTileType;
    }

    public int getShape() {
        return gateShape & 0xFF;
    }

    public void setShape(int shape) {
        gateShape = (byte) shape;
    }

    public void preparePlacement(int r) {
        setRotation(r);
    }

    @Override
    public void save(CompoundTag tag) {
        tag.putByte("orient", gateRotation);
        tag.putByte("shape", gateShape);
        tag.putByte("connMask", connMask);
    }

    @Override
    public void load(CompoundTag tag) {
        gateRotation = tag.getByte("orient");
        gateShape = tag.getByte("shape");
        connMask = tag.getByte("connMask");
    }

    @Override
    public void writeDesc(MCDataOutput out) {
        out.writeByte(gateRotation);
        out.writeByte(gateShape);
    }

    @Override
    public void readDesc(MCDataInput in) {
        gateRotation = in.readByte();
        gateShape = in.readByte();
    }

    @Override
    public void read(MCDataInput in, int key) {
        switch (key) {
            case PACKET_ROTATION -> gateRotation = in.readByte();
            case PACKET_SHAPE -> gateShape = in.readByte();
            default -> super.read(in, key);
        }
    }

    protected void sendRotationUpdate() {
        getWriteStream(PACKET_ROTATION).writeByte(gateRotation);
    }

    protected void sendShapeUpdate() {
        getWriteStream(PACKET_SHAPE).writeByte(gateShape);
    }

    protected void rotateAndSend() {
        setRotation((getRotation()+1)%4);
        updateConns();
        sendRotationUpdate();
        notifyNeighbors(0xF);
        getEditor().markTileChange();
    }

    protected void reflectAndSend() {
        // Reflection is implementation sepecific.
        // Override and call configureShapeAndSend with relected shape
    }

    protected void configureShapeAndSend(int newShape) {
        if (getShape() != newShape) {
            setShape(newShape);
            updateConns();
            sendShapeUpdate();
            notifyNeighbors(0xF);
            getEditor().markTileChange();
        }
    }

    @Override
    public void onNeighborChanged() {
        super.onNeighborChanged();
        if (updateConns())
            getEditor().markTileChange();
    }

    @Override
    public void onAdded() {
        super.onAdded();
        updateConns();
        onGatePlaced();
    }

    @Override
    public void onRemoved() {
        super.onRemoved();
        notifyNeighbors(0xF);
    }

    @Override
    public void renderTile(CCRenderState ccrs, Transformation t, float partialFrame) {
        GateModelRenderer r = GateModelRenderer.instance();
        r.renderStatic(ccrs, this, t);
        r.renderDynamic(ccrs, this, t, partialFrame);
    }

    protected void notifyNeighbors(int rMask) {
        int absRMask = toAbsoluteMask(rMask);
        for (int r = 0; r < 4; r++) if ((absRMask & 1 << r) != 0) {
            int absDir = IRotatableICTile.rotationToDir(r);
            getEditor().queueNeighborChange(getPos().offset(absDir));
        }
    }

    @Override
    public void buildInteractionZoneList(List<InteractionZone> zones) {
        super.buildInteractionZoneList(zones);

        double s = 2/16D;
        double h = s + 0.5/16D;
        Cuboid6 box = new Cuboid6(0, 0, 0, s, h, s);

        int i = 1;
        if (canRotate()) {
            zones.add(new HotKeyInteractionZone.Builder()
                    .keyCode(GLFW.GLFW_KEY_R)
                    .text(Component.translatable(UL_ROTATE))
                    .bounds(box.copy().add(1 - s * i, 0, 1 - s))
                    .keyAction(this::rotateAndSend)
                    .icon(() -> ICRenderTypes.rotateIcon)
                    .build());
            i++;
        }
        if (canReflect()) {
            zones.add(new HotKeyInteractionZone.Builder()
                    .keyCode(GLFW.GLFW_KEY_F)
                    .text(Component.translatable(UL_REFLECT))
                    .bounds(box.copy().add(1 - s * i, 0, 1 - s))
                    .keyAction(this::reflectAndSend)
                    .icon(() -> ICRenderTypes.reflectIcon)
                    .build());
            i++;
        }
    }

    //region IRotatableICTile implementation
    @Override
    public int getRotation() {
        return gateRotation & 0x3;
    }

    @Override
    public void setRotation(int r) {
        gateRotation = (byte) (gateRotation & 0xFC | r);
    }
    //endregion

    //region IConnectableICTile implementation
    @Override
    public int getConnMask() {
        return connMask & 0xFF;
    }

    @Override
    public void setConnMask(int connMask) {
        this.connMask = (byte) connMask;
    }

    @Override
    public IConnectableICTile getTileTowardsDir(int dir) {
        Optional<BaseTile> tile = getMap().getBaseTile(getPos().offset(dir));
        if (tile.isPresent() && tile.get() instanceof IConnectableICTile) return (IConnectableICTile) tile.get();
        return null;
    }

    @Override
    public boolean canConnectTo(IConnectableICTile target, int towardsDir) {
        return canGateConnectTo(target, toInternalRotation(IRotatableICTile.dirToRotation(towardsDir)));
    }

    @Override
    public void onMaskChanged() {
        // Gate rendering does not require connMask, so don't send to clients
    }
    //endregion

    //region IGateRenderKey implementation
    @Override
    public int getRenderIndex() {
        return gateTileType.renderIndex;
    }

    @Override
    public int getOrientation() {
        return getRotation() & 0x3;
    }

    @Override
    public int shape() {
        return this.gateShape;
    }
    //endregion

    //region GateTile logic override points
    protected void onGatePlaced() { }

    protected boolean canRotate() {
        return true;
    }

    protected boolean canReflect() {
        return false;
    }

    protected abstract boolean canGateConnectTo(IConnectableICTile target, int r);
    //endregion
}
