package mrtjp.projectred.integration.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.raytracer.VoxelShapeCache;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Vector3;
import codechicken.microblock.part.face.FaceMicroblockPart;
import codechicken.multipart.api.MultipartType;
import codechicken.multipart.api.part.*;
import codechicken.multipart.block.TileMultipart;
import codechicken.multipart.util.MultipartPlaceContext;
import codechicken.multipart.util.PartRayTraceResult;
import com.google.common.collect.ImmutableSet;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.api.IScrewdriver;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.PlacementLib;
import mrtjp.projectred.core.part.IConnectableFacePart;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.client.GateComponentModels;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.level.chunk.LevelChunk;
import net.minecraft.world.phys.shapes.CollisionContext;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Consumer;

public abstract class GatePart extends BaseMultipart implements IConnectableFacePart, TickablePart, FacePart, NormalOcclusionPart, IconHitEffectsPart, IGateRenderData {

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

    public boolean preparePlacement(MultipartPlaceContext context) {
        int side = context.getClickedFace().ordinal();
        setSide(side ^ 1);
        setRotation(context.getPlayer() == null ? 0 : (Rotation.getSidedRotation(context.getPlayer(), side) + 2) % 4);
        return true;
    }

    public final GateType getGateType() {
        return type;
    }

    @Override
    public int getOrientation() {
        return orientation & 0xFF;
    }

    @Override
    public int getRenderIndex() {
        return type.ordinal();
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
    //endregion

    //region Save/load and descriptions
    @Override
    public void save(CompoundTag tag) {
        super.save(tag);
        tag.putByte("orient", orientation);
        tag.putByte("shape", gateShape);
        tag.putInt("connMap", connMap);
        tag.putLong("schedTime", scheduledTime);
    }

    @Override
    public void load(CompoundTag tag) {
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
                if (Configurator.staticGates) {
                    tile().markRender();
                }
                break;
            case KEY_SHAPE:
                gateShape = packet.readByte();
                if (Configurator.staticGates) {
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
    public void onPartChanged(@Nullable MultiPart part) {
        super.onPartChanged(part);
        if (!level().isClientSide) {
            updateOutward();
            onChange();
        }
    }

    @Override
    public void onNeighborBlockChanged(BlockPos from) {
        super.onNeighborBlockChanged(from);
        if (!level().isClientSide) {
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
        if (!level().isClientSide) {
            gateLogicSetup();
            updateInward();
            onChange();
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
    public void onChunkLoad(LevelChunk chunk) {
        super.onChunkLoad(chunk);
        if (hasTile()) {
            gateLogicOnWorldLoad();
        }
    }

    private boolean dropIfCantStay() {
        if (!PlacementLib.canPlaceGateOnSide(level(), pos().relative(Direction.values()[getSide()]), Direction.values()[getSide() ^ 1])) {
            TileMultipart.dropItem(getGateType().makeStack(), level(), Vector3.fromTileCenter(tile()));
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
        if (scheduledTime < 0) scheduledTime = level().getGameTime() + ticks;
    }

    public boolean isTickScheduled() {
        return scheduledTime >= 0;
    }

    private void processScheduled() {
        if (scheduledTime >= 0 && level().getGameTime() >= scheduledTime) {
            scheduledTime = -1;
            scheduledTick();
        }
    }

    @Override
    public void tick() {
        if (!level().isClientSide) {
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
    public MultipartType<?> getType() {
        return getGateType().getPartType();
    }

    //region Items and drops
    @Override
    public ItemStack getCloneStack(PartRayTraceResult hit) {
        return getItem();
    }

    @Override
    public Iterable<ItemStack> getDrops() {
        return Collections.singleton(getItem());
    }

    protected ItemStack getItem() {
        return getGateType().makeStack();
    }
    //endregion

    //region Gate shapes and bounds
    @Override
    public VoxelShape getCollisionShape(CollisionContext context) {
        return FaceMicroblockPart.aShapes[0x10 | getSide()]; //TODO bring this in-house. No need to use cover's shape
    }

    @Override
    public VoxelShape getShape(CollisionContext context) {
        return getCollisionShape(context);
    }

    @Override
    public VoxelShape getOcclusionShape() {
        return oShapes[getSide()];
    }
    //endregion

    @Override
    public float getStrength(Player player, PartRayTraceResult hit) {
        return 2/30f;
    }

    @Override
    public int getLightEmission() {
        return Configurator.logicGateLights ? 7 : 0;
    }

    @Override
    public SoundType getPlacementSound(UseOnContext context) {
        return SoundType.GLASS;
    }

    //region Faces and slots
    @Override
    public int getSlotMask() {
        return 1 << getSide();
    }
    //endregion
    //endregion

    //region Activation handling
    @Override
    public InteractionResult activate(Player player, PartRayTraceResult hit, ItemStack held, InteractionHand hand) {
        if (gateLogicActivate(player, held, hit)) return InteractionResult.SUCCESS;

        if (!held.isEmpty() && held.getItem() instanceof IScrewdriver screwdriver) {
            if (!level().isClientSide) {
                if (player.isCrouching()) {
                    configure();
                } else {
                    rotate();
                }
                screwdriver.damageScrewdriver(player, held);
            }
            return InteractionResult.SUCCESS;
        }

        return InteractionResult.PASS;
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
        return new Cuboid6(getShape(CollisionContext.empty()).bounds());
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

    protected boolean gateLogicActivate(Player player, ItemStack held, PartRayTraceResult hit) {
        return false;
    }
    //endregion
}
