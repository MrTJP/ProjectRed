package mrtjp.projectred.transmission.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.raytracer.VoxelShapeCache;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.api.part.FacePart;
import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.api.part.NormalOcclusionPart;
import codechicken.multipart.block.TileMultipart;
import codechicken.multipart.util.PartMap;
import codechicken.multipart.util.PartRayTraceResult;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.PlacementLib;
import mrtjp.projectred.core.part.IConnectableFacePart;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.phys.shapes.CollisionContext;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;

import javax.annotation.Nullable;

public abstract class BaseFaceWirePart extends BaseWirePart implements IConnectableFacePart, NormalOcclusionPart, FacePart {

    public static final Cuboid6[][] sBounds = new Cuboid6[3][6];
    public static final Cuboid6[][] oBounds = new Cuboid6[3][6];
    public static final VoxelShape[][] sShapes = new VoxelShape[3][6];
    public static final VoxelShape[][] oShapes = new VoxelShape[3][6];

    static {
        for (int t = 0; t < 3; t++) {
            Cuboid6 selection = new Cuboid6(0, 0, 0, 1, (t+2)/16D, 1).expand(-0.005);
            Cuboid6 occlusion = new Cuboid6(2/8D, 0, 2/8D, 6/8D, (t+2)/16D, 6/8D);
            for (int s = 0; s < 6; s++) {
                sBounds[t][s] = selection.copy().apply(Rotation.sideRotations[s].at(Vector3.CENTER));
                sShapes[t][s] = VoxelShapeCache.getShape(sBounds[t][s]);
                oBounds[t][s] = occlusion.copy().apply(Rotation.sideRotations[s].at(Vector3.CENTER));
                oShapes[t][s] = VoxelShapeCache.getShape(oBounds[t][s]);
            }
        }
    }

    private static final int KEY_CONN_MAP = 1;

    private int connMap = 0;
    private byte side = 0;

    public BaseFaceWirePart(WireType wireType) {
        super(wireType);
    }

    //region Trait fields
    @Override
    public int getConnMap() {
        return connMap;
    }

    @Override
    public void setConnMap(int map) {
        connMap = map;
    }

    @Override
    public int getSide() {
        return side & 0xFF;
    }
    //endregion

    @Override
    public void save(CompoundTag tag) {
        super.save(tag);
        tag.putInt("connMap", connMap);
        tag.putByte("side", side);
    }

    @Override
    public void load(CompoundTag tag) {
        super.load(tag);
        connMap = tag.getInt("connMap");
        side = tag.getByte("side");
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        packet.writeInt(connMap);
        packet.writeByte(side);
    }

    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        connMap = packet.readInt();
        side = packet.readByte();
    }

    @Override
    protected void read(MCDataInput packet, int key) {
        switch (key) {
            case KEY_CONN_MAP:
                connMap = packet.readInt();
                if (Configurator.staticWires) {
                    tile().markRender();
                }
                break;
            default:
                super.read(packet, key);
        }
    }

    protected void sendConnUpdate() {
        sendUpdate(KEY_CONN_MAP, p -> p.writeInt(connMap));
    }

    @Override
    public void preparePlacement(Direction side) {
        // Wire is placed against the clicked face
        this.side = (byte) (side.ordinal() ^ 1);
    }

    @Override
    public SoundType getPlacementSound(UseOnContext context) {
        return SoundType.GLASS;
    }

    @Override
    public void onPartChanged(@Nullable MultiPart part) {
        super.onPartChanged(part);
        if (!level().isClientSide) {
            updateOutward();
        }
    }

    @Override
    public void onNeighborBlockChanged(BlockPos from) {
        super.onNeighborBlockChanged(from);
        if (!level().isClientSide) {
            if (dropIfCantStay()) return;
            updateOutside();
        }
    }

    @Override
    public void onAdded() {
        super.onAdded();
        if (!level().isClientSide) {
            updateInsideAndOutside();
        }
    }

    @Override
    public void onRemoved() {
        super.onRemoved();
        if (!level().isClientSide) {
            notifyAllExternals();
        }
    }

    @Override
    public float getStrength(Player player, PartRayTraceResult hit) {
        return 2/30F;
    }

    @Override
    public VoxelShape getShape(CollisionContext context) {
        return sShapes[getWireType().getThickness()][getSide()];
    }

    @Override
    public VoxelShape getCollisionShape(CollisionContext context) {
        return Shapes.empty();
    }

    //region Occlusion
    @Override
    public VoxelShape getOcclusionShape() {
        return oShapes[getWireType().getThickness()][getSide()];
    }
    //endregion

    @Override
    public int redstoneConductionMap() {
        return 0xF;
    }

    @Override
    public int getSlotMask() {
        return 1 << getSide();
    }

    private boolean canStay() {
        return PlacementLib.canPlaceWireOnSide(level(),
                pos().relative(Direction.values()[getSide()]), Direction.values()[getSide()^1]);
    }

    protected boolean dropIfCantStay() {
        if (!canStay()) {
            drop();
            return true;
        }
        return false;
    }

    private void drop() {
        TileMultipart.dropItem(getItem(), level(), Vector3.fromTileCenter(tile()));
        tile().remPart(this);
    }

    //region IConnectableFacePart overrides
    @Override
    public boolean canConnectCorner(int r) {
        return true;
    }

    @Override
    public boolean setRenderFlag(IConnectable part) {
        if (part instanceof BaseFaceWirePart wire) {
            // For wires of same thickness, use side for tie-breaker as it
            // is guaranteed to be unique for a corner connected wire
            if (wire.getWireType().getThickness() == getWireType().getThickness()) {
                return getSide() < wire.getSide();
            }

            // Otherwise, the thinner wire always reaches around to the thicker wire
            return wire.getWireType().getThickness() > getWireType().getThickness();
        }

        // If the other part is not a wire, then of course we have to render to it
        return true;
    }

    @Override
    public boolean discoverOpen(int r) {
        // Condition 1: Edge must be empty (this is where strips go)
        MultiPart edgePart = tile().getSlottedPart(PartMap.edgeBetween(getSide(), IConnectableFacePart.absoluteDir(this, r)));
        if (edgePart != null) return false;

        // Condition 2: Part on inside face must not consume the connection
        MultiPart insidePart = tile().getSlottedPart(IConnectableFacePart.absoluteDir(this, r));
        if (insidePart instanceof IConnectable) {
            //TODO this seems sus. Why let part connect inside AND outside?
            return canConnectPart((IConnectable) insidePart, r);
        }

        // If no part, then it can't consume the connection
        return insidePart == null;
    }

    @Override
    public void maskChangeEvent(boolean internalChange, boolean externalChange) {
        if (internalChange || externalChange) {
            sendConnUpdate();
        }
    }
    //endregion
}
