package mrtjp.projectred.transmission.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.raytracer.IndexedVoxelShape;
import codechicken.lib.raytracer.VoxelShapeCache;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Vector3;
import codechicken.microblock.ItemMicroBlock;
import codechicken.microblock.MicroMaterialRegistry;
import codechicken.microblock.api.ISidedHollowConnect;
import codechicken.microblock.api.MicroMaterial;
import codechicken.microblock.handler.MicroblockModContent;
import codechicken.multipart.api.NormalOcclusionTest;
import codechicken.multipart.api.part.TMultiPart;
import codechicken.multipart.api.part.TNormalOcclusionPart;
import codechicken.multipart.api.part.TSlottedPart;
import codechicken.multipart.block.TileMultiPart;
import codechicken.multipart.util.PartRayTraceResult;
import com.google.common.collect.ImmutableSet;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.PlacementLib;
import mrtjp.projectred.core.part.IConnectableCenterPart;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.block.SoundType;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.item.ItemUseContext;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.ActionResultType;
import net.minecraft.util.Direction;
import net.minecraft.util.Hand;
import net.minecraft.util.SoundCategory;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.shapes.ISelectionContext;
import net.minecraft.util.math.shapes.VoxelShape;
import net.minecraft.world.World;

import java.util.LinkedList;
import java.util.List;
import java.util.Objects;

public abstract class BaseCenterWirePart extends BaseWirePart implements IConnectableCenterPart, TNormalOcclusionPart, TSlottedPart, ISidedHollowConnect {

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

    private static final int KEY_CONN_MAP = 1;
    private static final int KEY_MATERIAL = 2;
    private static final int KEY_REMOVE_MATERIAL = 3;

    private int connMap = 0;
    private MicroMaterial material = null;

    public BaseCenterWirePart(WireType wireType) {
        super(wireType);
    }

    public MicroMaterial getMaterial() {
        return material;
    }

    //region Trait fields
    @Override
    public World level() {
        return world();
    }

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
    public void save(CompoundNBT tag) {
        super.save(tag);
        tag.putInt("connMap", connMap);
        if (material != null) {
            tag.putString("mat", Objects.requireNonNull(material.getRegistryName()).toString());
        }
    }

    @Override
    public void load(CompoundNBT tag) {
        super.load(tag);
        connMap = tag.getInt("connMap");
        material = tag.contains("mat") ? MicroMaterialRegistry.getMaterial(tag.getString("mat")) : null;
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        packet.writeByte(packedConnMap());
        packet.writeBoolean(material != null);
        if (material != null) {
            packet.writeRegistryIdUnsafe(MicroMaterialRegistry.MICRO_MATERIALS(), material);
        }
    }

    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        connMap = packet.readUByte();
        if (packet.readBoolean()) {
            material = packet.readRegistryIdUnsafe(MicroMaterialRegistry.MICRO_MATERIALS());
        }
    }

    @Override
    protected void read(MCDataInput packet, int key) {
        switch (key) {
            case KEY_CONN_MAP:
                connMap = packet.readUByte();
                if (useStaticRenderer()) tile().markRender();
                break;
            case KEY_MATERIAL:
                material = packet.readRegistryIdUnsafe(MicroMaterialRegistry.MICRO_MATERIALS());
                if (useStaticRenderer()) tile().markRender();
                break;
            case KEY_REMOVE_MATERIAL:
                material = null;
                if (useStaticRenderer()) tile().markRender();
                break;
            default:
                super.read(packet, key);
        }
    }
    //endregion

    protected void sendConnUpdate() {
        sendUpdate(KEY_CONN_MAP, p -> p.writeByte(packedConnMap()));
    }

    protected void sendMaterialUpdate() {
        if (material == null) {
            sendUpdate(KEY_REMOVE_MATERIAL, p -> {});
        } else {
            sendUpdate(KEY_MATERIAL, p -> p.writeRegistryIdUnsafe(MicroMaterialRegistry.MICRO_MATERIALS(), material));
        }
    }

    public int packedConnMap() {
        return connMap & 0x3F | connMap >> 6 & 0x3F;
    }

    @Override
    public void preparePlacement(Direction side) {
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
        if (material != null) {
            return Math.min(1.25f / 30f, material.getStrength(player));
        }
        return 1.25f / 30f;
    }

    @Override
    public Iterable<ItemStack> getDrops() {
        List<ItemStack> list = new LinkedList<>();
        for (ItemStack s : super.getDrops())
            list.add(s);

        if (material != null) {
            list.add(ItemMicroBlock.create(0, 1, material));
        }

        return list;
    }

    @Override
    public VoxelShape getShape(ISelectionContext context) {
        return new IndexedVoxelShape(getCollisionShape(context), 0);
    }

    @Override
    public VoxelShape getCollisionShape(ISelectionContext context) {
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

    @Override
    public boolean occlusionTest(TMultiPart npart) {
        return NormalOcclusionTest.test(this, npart);
    }
    //endregion

    @Override
    public int getHollowSize(int side) {
        return 8;
    }

    @Override
    public int getSlotMask() {
        return 1 << 6;
    }

    @Override
    public ActionResultType activate(PlayerEntity player, PartRayTraceResult hit, ItemStack held, Hand hand) {

        if (super.activate(player, hit, held, hand).shouldSwing()) return ActionResultType.SUCCESS;

        // Couch + right click with empty hand removes material
        if (held.isEmpty() && player.isCrouching() && material != null) {
            if (!world().isClientSide) {
                if (material != null && !player.isCreative()) {
                    PlacementLib.dropTowardsPlayer(world(), pos(), ItemMicroBlock.create(0, 1, material), player);
                }
                material = null;
                sendMaterialUpdate();
            }
            return ActionResultType.SUCCESS;
        }

        // Right click with cover Microblock allows adding a cover material
        if (!held.isEmpty() && held.getItem() == MicroblockModContent.itemMicroBlock() && MicroMaterialRegistry.microFactory(held) == 0 && MicroMaterialRegistry.microSize(held) == 1) {
            MicroMaterial newMat = ItemMicroBlock.getMaterialFromStack(held);
            if (newMat != null && (material == null || newMat != material)) {
                if (!world().isClientSide) {
                    // Exclude incompatible materials
                    if (newMat.isTransparent()) {
                        return ActionResultType.PASS;
                    }

                    // Drop old material if player is not in creative
                    if (material != null && !player.isCreative()) {
                        PlacementLib.dropTowardsPlayer(world(), pos(), ItemMicroBlock.create(0, 1, material), player);
                    }

                    // Swap the material
                    material = newMat;
                    sendMaterialUpdate();

                    // Play material sound
                    SoundType sound = newMat.getSound();
                    world().playSound(null, pos(), sound.getPlaceSound(), SoundCategory.BLOCKS, sound.getVolume() + 1.0F/2.0F, sound.getPitch() * 0.8F);

                    // Consume item from player if not in creative
                    if (!player.isCreative()) {
                        held.shrink(1);
                    }
                }
                return ActionResultType.SUCCESS;
            }
        }

        return ActionResultType.PASS;
    }

    //region IConnectableCenterPart overrides
    @Override
    public boolean discoverOpen(int s) {
        TMultiPart part = tile().getSlottedPart(s);

        // If nothing on the face, connection is possible
        if (part == null) {
            return true;
        }

        // If a compatible connectable part is on the face, connection is possible
        if (part instanceof IConnectable && canConnectPart((IConnectable) part, s)) {
            return true;
        }

        expandBounds = s;
        boolean fits = tile().canReplacePart(this, this);
        expandBounds = -1;
        return fits;
    }

    @Override
    public void onMaskChanged() {
        sendConnUpdate();
    }
    //endregion
}
