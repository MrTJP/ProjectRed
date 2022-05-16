package mrtjp.projectred.fabrication.engine.gates;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Transformation;
import mrtjp.projectred.fabrication.engine.BaseTile;
import mrtjp.projectred.fabrication.engine.ICTileType;
import mrtjp.projectred.fabrication.engine.IConnectableICTile;
import mrtjp.projectred.fabrication.engine.IRotatableICTile;
import mrtjp.projectred.integration.IGateRenderKey;
import mrtjp.projectred.integration.RenderGate;
import net.minecraft.nbt.CompoundNBT;

import java.util.Optional;

public abstract class GateTile extends BaseTile implements IConnectableICTile, IRotatableICTile, IGateRenderKey {

    public static final int PACKET_ROTATION = 1;
    public static final int PACKET_SHAPE = 2;
    public static final int PACKET_CLIENT_ROTATE_REQUEST = 3;
    public static final int PACKET_CLIENT_CONFIGURE_REQUEST = 4;

    private byte connMask = 0;
    private byte gateShape = 0;
    private byte gateRotation = 0;

    private final int renderIndex;

    public GateTile(ICTileType tileType, int renderIndex) {
        super(tileType);
        this.renderIndex = renderIndex;
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
    public void save(CompoundNBT tag) {
        tag.putByte("orient", gateRotation);
        tag.putByte("shape", gateShape);
        tag.putByte("connMask", connMask);
    }

    @Override
    public void load(CompoundNBT tag) {
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
            case PACKET_ROTATION:
                gateRotation = in.readByte();
                break;
            case PACKET_SHAPE:
                gateShape = in.readByte();
                break;
            case PACKET_CLIENT_ROTATE_REQUEST:
                rotateAndSend();
                break;
            case PACKET_CLIENT_CONFIGURE_REQUEST:
                configureAndSend();
                break;
            default:
                super.read(in, key);
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

    protected void configureAndSend() {
        if (cycleShape()) {
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
        RenderGate r = RenderGate.instance();
        r.renderStatic(renderIndex, this, getRotation() & 0x3, t, ccrs);
        r.renderDynamic(renderIndex, this, getRotation() & 0x3, t, partialFrame, ccrs);
    }

    protected void notifyNeighbors(int rMask) {
        int absRMask = toAbsoluteMask(rMask);
        for (int r = 0; r < 4; r++) if ((absRMask & 1 << r) != 0) {
            int absDir = IRotatableICTile.rotationToDir(r);
            getEditor().queueNeighborChange(getPos().offset(absDir));
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
    public int shape() {
        return this.gateShape;
    }
    //endregion

    //region GateTile logic override points
    protected void onGatePlaced() { }

    protected abstract boolean canGateConnectTo(IConnectableICTile target, int r);

    protected abstract boolean cycleShape();
    //endregion
}
