package mrtjp.projectred.integration.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.raytracer.VoxelShapeCache;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Vector3;
import codechicken.microblock.FaceMicroFactory;
import codechicken.multipart.api.MultiPartType;
import codechicken.multipart.api.NormalOcclusionTest;
import codechicken.multipart.api.part.*;
import codechicken.multipart.block.TileMultiPart;
import codechicken.multipart.util.PartRayTraceResult;
import com.google.common.collect.ImmutableSet;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.api.IScrewdriver;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.PRLib;
import mrtjp.projectred.core.part.IConnectableFacePart;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.client.GateComponentModels;
import net.minecraft.block.SoundType;
import net.minecraft.client.particle.ParticleManager;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.item.ItemUseContext;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.ActionResultType;
import net.minecraft.util.Direction;
import net.minecraft.util.Hand;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.shapes.ISelectionContext;
import net.minecraft.util.math.shapes.VoxelShape;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Consumer;

public abstract class GatePart extends TMultiPart implements IConnectableFacePart, ITickablePart, TFacePart, TNormalOcclusionPart, TIconHitEffectsPart, IGateRenderData {

    private static final int KEY_UPDATE = 0;
    private static final int KEY_ORIENTATION = 1;
    private static final int KEY_SHAPE = 2;

    private static final Cuboid6[][] oBoxes = new Cuboid6[6][2];
    private static final VoxelShape[] oShapes = new VoxelShape[6];

    static {
        for (int s = 0; s < 6; s++) {
            Transformation t = Rotation.sideRotations[s].at(Vector3.CENTER);
            Cuboid6 occlusion1 = new Cuboid6(1 / 8D, 0, 0, 7 / 8D, 1 / 8D, 1);
            Cuboid6 occlusion2 = new Cuboid6(0, 0, 1 / 8D, 1, 1 / 8D, 7 / 8D);

            oBoxes[s][0] = occlusion1.apply(t);
            oBoxes[s][1] = occlusion2.apply(t);

            List<VoxelShape> boxes = new LinkedList<>();
            for (Cuboid6 box : oBoxes[s]) {
                boxes.add(VoxelShapeCache.getShape(box));
            }

            oShapes[s] = VoxelShapeCache.merge(ImmutableSet.copyOf(boxes));
        }
    }

    private final GateType type;

    private byte orientation = 0;
    private byte gateShape = 0;
    private int connMap = 0;
    private long scheduledTime = 0;

    public GatePart(GateType type) {
        this.type = type;
    }

    public void preparePlacement(PlayerEntity player, BlockPos pos, int side) {
        setSide(side ^ 1);
        setRotation((Rotation.getSidedRotation(player, side) + 2) % 4);
    }

    public final GateType getGateType() {
        return type;
    }

    public int getOrientation() {
        return orientation & 0xFF;
    }

    @Override
    public int shape() {
        return gateShape;
    }

    public void setShape(int gateShape) {
        this.gateShape = (byte) gateShape;
    }

    //region Trait variables
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
        return orientation >> 2;
    }

    @Override
    public int getRotation() {
        return orientation & 0x3;
    }

    @Override
    public void setSide(int s) {
        orientation = (byte) ((orientation & 0x3) | (s << 2));
    }

    @Override
    public void setRotation(int r) {
        orientation = (byte) ((orientation & 0xFC) | (r & 0x3));
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

    //region Save/load and descriptions
    @Override
    public void save(CompoundNBT tag) {
        super.save(tag);
        tag.putByte("orient", orientation);
        tag.putByte("shape", gateShape);
        tag.putInt("connMap", connMap);
        tag.putLong("schedTime", scheduledTime);
    }

    @Override
    public void load(CompoundNBT tag) {
        super.load(tag);
        orientation = tag.getByte("orient");
        gateShape = tag.getByte("shape");
        connMap = tag.getInt("connMap");
        scheduledTime = tag.getLong("schedTime");
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        packet.writeByte(orientation);
        packet.writeByte(gateShape);
    }

    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        orientation = packet.readByte();
        gateShape = packet.readByte();
    }
    //endregion

    //region Packets
    public final void sendUpdate(Consumer<MCDataOutput> func) {
        sendUpdate(KEY_UPDATE, func);
    }

    @Override
    public final void readUpdate(MCDataInput packet) {
        read(packet, packet.readUByte());
    }

    protected final void sendUpdate(int key, Consumer<MCDataOutput> func) {
        super.sendUpdate(p -> {
            p.writeByte(key);
            func.accept(p);
        });
    }

    // Override to handle other keys
    protected void read(MCDataInput packet, int key) {
        switch (key) {
            case KEY_UPDATE:
                readDesc(packet);
                break;
            case KEY_ORIENTATION:
                orientation = packet.readByte();
                if (Configurator.staticGates()) {
                    tile().markRender();
                }
                break;
            case KEY_SHAPE:
                gateShape = packet.readByte();
                if (Configurator.staticGates()) {
                    tile().markRender();
                }
                break;
            default:
                // Unknown
        }
    }

    protected void sendShapeUpdate() {
        sendUpdate(KEY_SHAPE, p -> p.writeByte(gateShape));
    }

    protected void sendOrientationUpdate() {
        sendUpdate(KEY_ORIENTATION, p -> p.writeByte(orientation));
    }
    //endregion

    //region Neighbor/part changes
    @Override
    public void onPartChanged(TMultiPart part) {
        super.onPartChanged(part);
        if (!world().isClientSide) {
            updateOutward();
            onChange();
        }
    }

    @Override
    public void onNeighborBlockChanged(BlockPos from) {
        super.onNeighborBlockChanged(from);
        if (!world().isClientSide) {
            if (dropIfCantStay()) {
                return;
            }
            updateExternalConns();
            onChange();
        }
    }

    @Override
    public void onAdded() {
        super.onAdded();
        if (!world().isClientSide) {
            gateLogicSetup();
            updateInward();
            onChange();
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
    public void onChunkLoad(Chunk chunk) {
        super.onChunkLoad(chunk);
        if (tile() != null) {
            gateLogicOnWorldLoad();
        }
    }

    private boolean dropIfCantStay() {
        if (!PRLib.canPlaceGateOnSide(world(), pos().relative(Direction.values()[getSide()]), Direction.values()[getSide() ^ 1])) {
            TileMultiPart.dropItem(getGateType().makeStack(), world(), Vector3.fromTileCenter(tile()));
            tile().remPart(this);
            return true;
        }
        return false;
    }

    protected void onChange() {
        processScheduled();
        gateLogicOnChange();
    }

    @Override
    public void onMaskChanged() {
        // Client doesn't need connmap, wont send
    }
    //endregion

    //region Ticks and scheduling
    @Override
    public void scheduledTick() {
        gateLogicOnScheduledTick();
    }

    /*
     * Super's global part tick schedule used by superclass will fire scheduled tick events if chunk is
     * unloaded and re-loaded after the scheduled time. This is not desirable for gates, so we override
     * with this internal implementation. On chunk unload, the remaining time is saved and re-applied.
     */
    @Override
    public void scheduleTick(int ticks) {
        if (scheduledTime < 0) scheduledTime = world().getGameTime() + ticks;
    }

    public boolean isTickScheduled() {
        return scheduledTime >= 0;
    }

    private void processScheduled() {
        if (scheduledTime >= 0 && world().getGameTime() >= scheduledTime) {
            scheduledTime = -1;
            scheduledTick();
        }
    }

    @Override
    public void tick() {
        if (!world().isClientSide) {
            processScheduled();
        }
        gateLogicOnTick();
    }
    //endregion

    //region Connections
    @Override
    public boolean canConnectPart(IConnectable part, int r) {
        return gateLogicCanConnectTo(part, toInternal(r));
    }

    @Override
    public boolean setRenderFlag(IConnectable part) {
        // Gates don't render around corners. Wires must always render to us
        return false;
    }

    @Override
    public boolean discoverOpen(int r) {
        // No edge face part can close a gate connection. Gates take up the full face,
        // leaving no space for covers or strips to block connections.
        return true;
    }

    @Override
    public boolean canConnectCorner(int r) {
        return false;
    }
    //endregion

    //region Part properties and overrides
    @Override
    public MultiPartType<?> getType() {
        return getGateType().getPartType();
    }

    //region Items and drops
    @Override
    public ItemStack pickItem(PartRayTraceResult hit) {
        return getGateType().makeStack();
    }

    @Override
    public Iterable<ItemStack> getDrops() {
        return Collections.singleton(getGateType().makeStack());
    }
    //endregion

    //region Gate shapes and bounds
    @Override
    public VoxelShape getCollisionShape(ISelectionContext context) {
        return FaceMicroFactory.aShapes()[0x10 | getSide()]; //TODO bring this in-house. No need to use cover's shape
    }

    @Override
    public VoxelShape getShape(ISelectionContext context) {
        return getCollisionShape(context);
    }

    @Override
    public VoxelShape getOcclusionShape() {
        return oShapes[getSide()];
    }

    @Override
    public boolean occlusionTest(TMultiPart npart) {
        return NormalOcclusionTest.test(this, npart) && super.occlusionTest(npart);
    }
    //endregion

    @Override
    public float getStrength(PlayerEntity player, PartRayTraceResult hit) {
        return 2/30f;
    }

    @Override
    public int getLightValue() {
        return Configurator.logicGateLights() ? 7 : 0;
    }

    @Override
    public SoundType getPlacementSound(ItemUseContext context) {
        return SoundType.GLASS;
    }

    //region Faces and slots

    @Override
    public boolean solid(int side) {
        return false;
    }

    @Override
    public int getSlotMask() {
        return 1 << getSide();
    }
    //endregion
    //endregion

    //region Activation handling
    @Override
    public ActionResultType activate(PlayerEntity player, PartRayTraceResult hit, ItemStack held, Hand hand) {
        if (gateLogicActivate(player, held, hit)) return ActionResultType.SUCCESS;

        if (!held.isEmpty() && held.getItem() instanceof IScrewdriver) {
            IScrewdriver screwdriver = (IScrewdriver) held.getItem();
            if (!world().isClientSide) {
                if (player.isCrouching()) {
                    configure();
                } else {
                    rotate();
                }
                screwdriver.damageScrewdriver(player, held);
            }
            return ActionResultType.SUCCESS;
        }

        return ActionResultType.PASS;
    }

    protected void configure() {
        if (gateLogicCycleShape()) {
            updateInward();
            tile().setChanged();
            tile().notifyPartChange(this);
            sendShapeUpdate();
            notifyExternals(0xF);
            onChange();
        }
    }

    protected void rotate() {
        setRotation((getRotation() + 1) % 4);
        updateInward();
        tile().setChanged();
        tile().notifyPartChange(this);
        sendOrientationUpdate();
        notifyExternals(0xF);
        onChange();
    }
    //endregion

    //region Breaking and hit effect
    @Override
    public Cuboid6 getBounds() {
        return new Cuboid6(getShape(ISelectionContext.empty()).bounds());
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public TextureAtlasSprite getBreakingIcon(PartRayTraceResult hit) {
        return GateComponentModels.baseIcon.icon;
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public TextureAtlasSprite getBrokenIcon(int side) {
        return GateComponentModels.baseIcon.icon;
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void addHitEffects(PartRayTraceResult hit, ParticleManager manager) {
        IconHitEffects.addHitEffects(this, hit, manager);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void addDestroyEffects(PartRayTraceResult hit, ParticleManager manager) {
        IconHitEffects.addDestroyEffects(this, manager);
    }
    //endregion

    //region Overrides for gate logic
    protected abstract boolean gateLogicCanConnectTo(IConnectable part, int r);
    protected abstract void gateLogicOnChange();
    protected abstract void gateLogicOnScheduledTick();

    protected boolean gateLogicCycleShape() {
        return false;
    }

    protected void gateLogicOnTick() {
    }

    protected void gateLogicSetup() {
    }

    protected void gateLogicOnWorldLoad() {
    }

    protected boolean gateLogicActivate(PlayerEntity player, ItemStack held, PartRayTraceResult hit) {
        return false;
    }
    //endregion
}
