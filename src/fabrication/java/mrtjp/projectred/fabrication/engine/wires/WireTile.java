package mrtjp.projectred.fabrication.engine.wires;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Transformation;
import mrtjp.projectred.fabrication.engine.BaseTile;
import mrtjp.projectred.fabrication.engine.ICTileType;
import mrtjp.projectred.fabrication.engine.IConnectableICTile;
import mrtjp.projectred.fabrication.engine.IRotatableICTile;
import mrtjp.projectred.transmission.RenderWire;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.nbt.CompoundNBT;

import java.util.Optional;

public abstract class WireTile extends BaseTile implements IConnectableICTile {

    private static final int PACKET_CONN_MASK = 1;

    private final WireType renderType;

    public WireTile(ICTileType tileType, WireType renderType) {
        super(tileType);
        this.renderType = renderType;
    }

    private byte connMask = 0;

    public WireType getWireType() {
        return renderType;
    }

    @Override
    public int getConnMask() {
        return connMask & 0xFF;
    }

    @Override
    public void setConnMask(int connMask) {
        this.connMask = (byte) connMask;
    }

    @Override
    public void save(CompoundNBT tag) {
        tag.putByte("connMask", connMask);
    }

    @Override
    public void load(CompoundNBT tag) {
        connMask = tag.getByte("connMask");
    }

    @Override
    public void writeDesc(MCDataOutput out) {
        out.writeByte(connMask);
    }

    @Override
    public void readDesc(MCDataInput in) {
        connMask = in.readByte();
    }

    @Override
    public void read(MCDataInput in, int key) {
        switch (key) {
            case PACKET_CONN_MASK:
                connMask = in.readByte();
                break;
            default:
                super.read(in, key);
        }
    }

    protected void sendConnUpdate() {
        getWriteStream(PACKET_CONN_MASK).writeByte(connMask);
    }

    @Override
    public void onMaskChanged() {
        sendConnUpdate();
        getEditor().markTileChange();
    }

    @Override
    public void onNeighborChanged() {
        super.onNeighborChanged();
        updateConns();
    }

    @Override
    public void onAdded() {
        super.onAdded();
        updateConns();
    }

    @Override
    public void onRemoved() {
        super.onRemoved();
        for (int s = 0; s < 6; s++) {
            getEditor().queueNeighborChange(getPos().offset(s));
        }
    }

    @Override
    public IConnectableICTile getTileTowardsDir(int dir) {
        Optional<BaseTile> tile = getMap().getBaseTile(getPos().offset(dir));
        if (tile.isPresent() && tile.get() instanceof IConnectableICTile) return (IConnectableICTile) tile.get();
        return null;
    }

    protected int getRenderHue() {
        return -1;
    }

    protected int getTextureIndex() {
        return 0;
    }

    @Override
    public void renderTile(CCRenderState ccrs, Transformation t, float partialFrame) {
        int rmask = IRotatableICTile.dirMaskToRotationMask(getConnMask());
        int wireConnMask = rmask << 4;
        int modelKey = RenderWire.modelKey(0, getWireType().getThickness(), wireConnMask);
        RenderWire.render(modelKey, getRenderHue(), getWireType().getTextures().get(getTextureIndex()), ccrs, t);
    }
}
