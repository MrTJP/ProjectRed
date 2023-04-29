package mrtjp.projectred.transmission.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.raytracer.VoxelShapeCache;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.api.NormalOcclusionTest;
import codechicken.multipart.api.part.TFacePart;
import codechicken.multipart.api.part.TMultiPart;
import codechicken.multipart.api.part.TNormalOcclusionPart;
import codechicken.multipart.block.TileMultiPart;
import codechicken.multipart.util.PartMap;
import codechicken.multipart.util.PartRayTraceResult;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.PRLib;
import mrtjp.projectred.core.part.IConnectableFacePart;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.block.SoundType;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemUseContext;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.shapes.ISelectionContext;
import net.minecraft.util.math.shapes.VoxelShape;
import net.minecraft.util.math.shapes.VoxelShapes;
import net.minecraft.world.World;

public abstract class BaseFaceWirePart extends BaseWirePart implements IConnectableFacePart, TNormalOcclusionPart, TFacePart {

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

    @Override
    public void setSide(int s) {
        throw new RuntimeException("Attempted to orient a wire!");
    }

    @Override
    public int getRotation() {
        return 0; // Wires don't rotate
    }

    @Override
    public void setRotation(int r) {
        throw new RuntimeException("Attempted to rotate a wire!");
    }

    @Override
    public BlockPos getPos() {
        return pos();
    }

    @Override
    public World getLevel() {
        return world();
    }

    @Override
    public TileMultiPart getTile() {
        return tile();
    }
    //endregion

    @Override
    public void save(CompoundNBT tag) {
        super.save(tag);
        tag.putInt("connMap", connMap);
        tag.putByte("side", side);
    }

    @Override
    public void load(CompoundNBT tag) {
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
                if (Configurator.staticWires()) {
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
    public SoundType getPlacementSound(ItemUseContext context) {
        return SoundType.GLASS;
    }

    @Override
    public void onPartChanged(TMultiPart part) {
        super.onPartChanged(part);
        if (!world().isClientSide) {
            if (updateOutward()) {
                onMaskChanged();
            }
        }
    }

    @Override
    public void onNeighborBlockChanged(BlockPos from) {
        super.onNeighborBlockChanged(from);
        if (!world().isClientSide) {
            if (dropIfCantStay()) return;
            if (updateExternalConns()) {
                onMaskChanged();
            }
        }
    }

    @Override
    public void onAdded() {
        super.onAdded();
        if (!world().isClientSide) {
            if (updateInward()) onMaskChanged();
        }
    }

    @Override
    public void onRemoved() {
        super.onRemoved();
        if (!world().isClientSide) {
            notifyAllExternals();
        }
    }

    @Override
    public float getStrength(PlayerEntity player, PartRayTraceResult hit) {
        return 2/30F;
    }

    @Override
    public VoxelShape getShape(ISelectionContext context) {
        return sShapes[getWireType().getThickness()][getSide()];
    }

    @Override
    public VoxelShape getCollisionShape(ISelectionContext context) {
        return VoxelShapes.empty();
    }

    //region Occlusion
    @Override
    public VoxelShape getOcclusionShape() {
        return oShapes[getWireType().getThickness()][getSide()];
    }

    @Override
    public boolean occlusionTest(TMultiPart npart) {
        return NormalOcclusionTest.test(this, npart);
    }
    //endregion

    @Override
    public boolean solid(int side) {
        return false;
    }

    @Override
    public int redstoneConductionMap() {
        return 0xF;
    }

    @Override
    public int getSlotMask() {
        return 1 << getSide();
    }

    private boolean canStay() {
        return PRLib.canPlaceWireOnSide(world(),
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
        TileMultiPart.dropItem(getItem(), world(), Vector3.fromTileCenter(tile()));
        tile().remPart(this);
    }

    //region IConnectableFacePart overrides
    @Override
    public boolean canConnectCorner(int r) {
        return true;
    }

    @Override
    public boolean setRenderFlag(IConnectable part) {
        if (part instanceof BaseFaceWirePart) {
            BaseFaceWirePart wire = (BaseFaceWirePart) part;
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
        TMultiPart edgePart = tile().getSlottedPart(PartMap.edgeBetween(getSide(), absoluteDir(r)));
        if (edgePart != null) return false;

        // Condition 2: Part on inside face must not consume the connection
        TMultiPart insidePart = tile().getSlottedPart(absoluteDir(r));
        if (insidePart instanceof IConnectable) {
            //TODO this seems sus. Why let part connect inside AND outside?
            return canConnectPart((IConnectable) insidePart, r);
        }

        // If no part, then it can't consume the connection
        return insidePart == null;
    }

    @Override
    public void onMaskChanged() {
        sendConnUpdate();
    }
    //endregion
}
