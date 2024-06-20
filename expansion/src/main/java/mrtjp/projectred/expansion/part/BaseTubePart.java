package mrtjp.projectred.expansion.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.raytracer.IndexedVoxelShape;
import codechicken.lib.raytracer.VoxelShapeCache;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Vector3;
import codechicken.microblock.api.MicroMaterial;
import codechicken.microblock.api.SlottedHollowConnect;
import codechicken.microblock.item.ItemMicroBlock;
import codechicken.microblock.util.MicroMaterialRegistry;
import codechicken.multipart.api.MultipartType;
import codechicken.multipart.api.part.BaseMultipart;
import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.api.part.NormalOcclusionPart;
import codechicken.multipart.api.part.SlottedPart;
import codechicken.multipart.util.MultipartPlaceContext;
import codechicken.multipart.util.PartRayTraceResult;
import com.google.common.collect.ImmutableSet;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.part.IConnectableCenterPart;
import mrtjp.projectred.expansion.TubeType;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.phys.shapes.CollisionContext;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import javax.annotation.Nullable;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;

public abstract class BaseTubePart extends BaseMultipart implements IConnectableCenterPart, NormalOcclusionPart, SlottedPart, SlottedHollowConnect {

    public static final Cuboid6[] fOBounds = new Cuboid6[7];
    public static final VoxelShape[] fOShapes = new VoxelShape[7];
    public static final VoxelShape[] fOShapeStates = new VoxelShape[64];
    public static int expandBounds = -1;

    static {
        double w = 2 / 8D;
        fOBounds[6] = new Cuboid6(0.5 - w, 0.5 - w, 0.5 - w, 0.5 + w, 0.5 + w, 0.5 + w);
        fOShapes[6] = VoxelShapeCache.getShape(fOBounds[6]);
        for (int s = 0; s < 6; s++) {
            fOBounds[s] = new Cuboid6(0.5 - w, 0, 0.5 - w, 0.5 + w, 0.5 - w, 0.5 + w).apply(Rotation.sideRotations[s].at(Vector3.CENTER));
            fOShapes[s] = VoxelShapeCache.getShape(fOBounds[s]);
        }

        for (int m = 0; m < 64; m++) {
            List<VoxelShape> shapes = new LinkedList<>();
            shapes.add(VoxelShapeCache.getShape(fOBounds[6]));
            for (int s = 0; s < 6; s++) {
                if ((m & (1 << s)) != 0) {
                    shapes.add(VoxelShapeCache.getShape(fOBounds[s]));
                }
            }
            fOShapeStates[m] = VoxelShapeCache.merge(ImmutableSet.copyOf(shapes));
        }
    }

    protected static final int KEY_UPDATE = 0;
    private static final int KEY_CONN_MAP = 1;
    private static final int KEY_MATERIAL = 2;
    private static final int KEY_REMOVE_MATERIAL = 3;

    private final TubeType pipeType;

    private int connMap = 0;
    private @Nullable MicroMaterial material = null;

    public BaseTubePart(TubeType pipeType) {
        this.pipeType = pipeType;
    }

    public @Nullable MicroMaterial getMaterial() {
        return material;
    }

    public TubeType getPipeType() {
        return pipeType;
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
    //endregion

    //region TMultiPart overrides
    @Override
    public void save(CompoundTag tag) {
        tag.putInt("connMap", connMap);
        if (material != null) {
            tag.putString("mat", Objects.requireNonNull(material.getRegistryName()).toString());
        }
    }

    @Override
    public void load(CompoundTag tag) {
        connMap = tag.getInt("connMap");
        material = tag.contains("mat") ? MicroMaterialRegistry.getMaterial(tag.getString("mat")) : null;
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        packet.writeByte(packedConnMap());
        packet.writeBoolean(material != null);
        if (material != null) {
            packet.writeRegistryIdDirect(MicroMaterialRegistry.MICRO_MATERIALS, material);
        }
    }

    @Override
    public MultipartType<?> getType() {
        return getPipeType().getPartType();
    }

    protected ItemStack getItem() {
        return getPipeType().makeStack();
    }

    @Override
    public ItemStack getCloneStack(PartRayTraceResult hit) {
        return getItem();
    }

    @Override
    public void readDesc(MCDataInput packet) {
        connMap = packet.readUByte();
        if (packet.readBoolean()) {
            material = packet.readRegistryIdDirect(MicroMaterialRegistry.MICRO_MATERIALS);
        }
    }
    //endregion

    //region Packets
    @Override
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
            case KEY_UPDATE -> readDesc(packet);
            case KEY_CONN_MAP -> {
                connMap = packet.readUByte();
                if (useStaticRenderer()) tile().markRender();
            }
            case KEY_MATERIAL -> {
                material = packet.readRegistryIdDirect(MicroMaterialRegistry.MICRO_MATERIALS);
                if (useStaticRenderer()) tile().markRender();
            }
            case KEY_REMOVE_MATERIAL -> {
                material = null;
                if (useStaticRenderer()) tile().markRender();
            }
            default -> {
                // Unknown
            }
        }
    }

    protected void sendConnUpdate() {
        sendUpdate(KEY_CONN_MAP, p -> p.writeByte(packedConnMap()));
    }

    protected void sendMaterialUpdate() {
        if (material == null) {
            sendUpdate(KEY_REMOVE_MATERIAL, p -> {});
        } else {
            sendUpdate(KEY_MATERIAL, p -> p.writeRegistryIdDirect(MicroMaterialRegistry.MICRO_MATERIALS, material));
        }
    }
    //endregion

    public int packedConnMap() {
        return connMap & 0x3F | connMap >> 6 & 0x3F;
    }

    public boolean useStaticRenderer() {
        return Configurator.staticWires;
    }

    @OnlyIn(Dist.CLIENT)
    public TextureAtlasSprite getIcon() {
        return getPipeType().getTextures().get(0);
    }

    public boolean preparePlacement(MultipartPlaceContext context) {
        return true;
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
        if (material != null) {
            return Math.min(1.25f / 30f, material.getStrength(player));
        }
        return 1.25f / 30f;
    }

    @Override
    public Iterable<ItemStack> getDrops() {
        List<ItemStack> list = new LinkedList<>();
        list.add(getItem());

        if (material != null) {
            list.add(ItemMicroBlock.create(0, 1, material));
        }

        return list;
    }

    @Override
    public VoxelShape getShape(CollisionContext context) {
        return new IndexedVoxelShape(getCollisionShape(context), 0);
    }

    @Override
    public VoxelShape getCollisionShape(CollisionContext context) {
        int m = 0;
        for (int s = 0; s < 6; s++) {
            if (maskConnects(s)) {
                m |= 1 << s;
            }
        }
        return fOShapeStates[m];
    }

    //region Occlusion
    @Override
    public VoxelShape getOcclusionShape() {
        return fOShapes[expandBounds >= 0 ? expandBounds : 6];
    }
    //endregion

    @Override
    public int getHoleSize(int side) {
        return 8;
    }

    @Override
    public int getSlotMask() {
        return 1 << 6;
    }

    @Override
    public InteractionResult activate(Player player, PartRayTraceResult hit, ItemStack held, InteractionHand hand) {
        return super.activate(player, hit, held, hand);

        //TODO Uncomment to deal with material application
        //     * Implement model generation and rendering in TubeModelRenderer (or rather, move FramedJacketedWireModelBuilder to core)
        //     * Add a Highlight renderer
        //     * Don't sync items to client when jacketed (you can't see them anyway)
//        if (super.activate(player, hit, held, hand).shouldSwing()) return InteractionResult.SUCCESS;
//
//        // Couch + right click with empty hand removes material
//        if (held.isEmpty() && player.isCrouching() && material != null) {
//            if (!level().isClientSide) {
//                if (material != null && !player.isCreative()) {
//                    PlacementLib.dropTowardsPlayer(level(), pos(), ItemMicroBlock.create(0, 1, material), player);
//                }
//                material = null;
//                sendMaterialUpdate();
//            }
//            return InteractionResult.SUCCESS;
//        }
//
//        // Right click with cover Microblock allows adding a cover material
//        if (!held.isEmpty() && held.getItem() == CBMicroblockModContent.MICRO_BLOCK_ITEM.get() && ItemMicroBlock.getFactory(held) == CBMicroblockModContent.FACE_MICROBLOCK_PART.get() && ItemMicroBlock.getSize(held) == 1) {
//            MicroMaterial newMat = ItemMicroBlock.getMaterialFromStack(held);
//            if (newMat != null && (material == null || newMat != material)) {
//                if (!level().isClientSide) {
//                    // Exclude incompatible materials
//                    if (newMat.isTransparent()) {
//                        return InteractionResult.PASS;
//                    }
//
//                    // Drop old material if player is not in creative
//                    if (material != null && !player.isCreative()) {
//                        PlacementLib.dropTowardsPlayer(level(), pos(), ItemMicroBlock.create(0, 1, material), player);
//                    }
//
//                    // Swap the material
//                    material = newMat;
//                    sendMaterialUpdate();
//
//                    // Play material sound
//                    SoundType sound = newMat.getSound();
//                    if (sound != null) {
//                        level().playSound(null, pos(), sound.getPlaceSound(), SoundSource.BLOCKS, sound.getVolume() + 1.0F/2.0F, sound.getPitch() * 0.8F);
//                    }
//
//                    // Consume item from player if not in creative
//                    if (!player.isCreative()) {
//                        held.shrink(1);
//                    }
//                }
//                return InteractionResult.SUCCESS;
//            }
//        }
//
//        return InteractionResult.PASS;
    }

    //region IConnectableCenterPart overrides
    @Override
    public boolean discoverOpen(int s) {
        MultiPart part = tile().getSlottedPart(s);

        // If nothing on the face, connection is possible
        if (part == null) {
            return true;
        }

        // If a compatible connectable part is on the face, connection is possible
        if (part instanceof IConnectable ic && canConnectPart(ic, s)) {
            return true;
        }

        expandBounds = s;
        boolean fits = tile().canReplacePart(this, this);
        expandBounds = -1;
        return fits;
    }

    @Override
    public void maskChangeEvent(boolean internalChange, boolean externalChange) {
        if (internalChange || externalChange) {
            sendConnUpdate();
        }
    }
    //endregion
}
