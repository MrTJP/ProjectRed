package mrtjp.projectred.illumination.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Vector3;
import codechicken.microblock.part.hollow.HollowMicroblockPart;
import codechicken.multipart.api.MultipartType;
import codechicken.multipart.api.RedstoneInteractions;
import codechicken.multipart.api.part.*;
import codechicken.multipart.api.part.redstone.RedstonePart;
import codechicken.multipart.block.BlockMultipart;
import codechicken.multipart.block.TileMultipart;
import codechicken.multipart.util.PartRayTraceResult;
import mrtjp.projectred.core.PlacementLib;
import mrtjp.projectred.illumination.MultipartLightProperties;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.phys.shapes.CollisionContext;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;

import javax.annotation.Nullable;
import java.util.Collections;

public class MultipartLightPart extends BaseMultipart implements SlottedPart, NormalOcclusionPart, RedstonePart, IconHitEffectsPart {

    private final MultipartType<?> type;
    private final MultipartLightProperties properties;
    private final int color;
    private final boolean inverted;

    protected boolean powered = false;
    protected int side = 0;

    public MultipartLightPart(MultipartType<?> type, MultipartLightProperties properties, int color, boolean inverted) {
        this.type = type;
        this.properties = properties;
        this.color = color;
        this.inverted = inverted;
    }

    public boolean isInverted() {
        return inverted;
    }

    public int getSide() {
        return side;
    }

    public int getColor() {
        return color;
    }

    public boolean isLightOn() {
        return powered != inverted;
    }

    public MultipartLightProperties getProperties() {
        return properties;
    }

    public void preparePlacement(int side) {
        this.side = side;
    }

    @Override
    public void save(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        tag.putBoolean("pow", powered);
        tag.putByte("side", (byte) side);
    }

    @Override
    public void load(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        powered = tag.getBoolean("pow");
        side = tag.getByte("side") & 0xFF;
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        packet.writeByte(side).writeBoolean(powered);
    }

    @Override
    public void readDesc(MCDataInput packet) {
        side = packet.readUByte();
        powered = packet.readBoolean();
    }

    @Override
    public void readUpdate(MCDataInput packet) {
        readDesc(packet);
        updateRender();
    }

    @Override
    public void onNeighborBlockChanged(BlockPos from) {
        if (checkSupport()) return;
        updateState(false);
    }

    @Override
    public void onPartChanged(@Nullable MultiPart part) {
        if (checkSupport()) return;
        updateState(false);
    }

    @Override
    public void onAdded() {
        if (checkSupport()) return;
        updateState(true);
    }

    @Override
    public int getLightEmission() {
        return (isInverted() != powered) ? 15 : 0;
    }

    @Override
    public Cuboid6 getBounds() {
        return properties.getBounds(side);
    }

    @Override
    public TextureAtlasSprite getBreakingIcon(PartRayTraceResult hit) {
        return getBrokenIcon(side);
    }

    @Override
    public TextureAtlasSprite getBrokenIcon(int side) {
        return properties.getIcon(color);
    }

    private ItemStack getItem() {
        return properties.makeStack(color, inverted);
    }

    private boolean checkSupport() {
        if (level().isClientSide) return false;
        if (properties.canFloat()) return false;

        BlockPos bc = pos().relative(Direction.values()[side]);
        if (MultipartLightPart.canPlaceLight(level(), bc, Direction.values()[side^1])) return false;

        TileMultipart.dropItem(getItem(), level(), Vector3.fromTileCenter(tile()));
        tile().remPart(this);
        return true;
    }

    private boolean checkPower() {
        for (int s = 0; s < 6; s++) {
            if (s != (side^1) && RedstoneInteractions.getPowerTo(this, s) > 0) {
                return true;
            }
        }
        return false;
    }

    private void updateState(boolean forceRender) {
        boolean updated = false;
        if (!level().isClientSide) {
            boolean oldPower = powered;
            powered = checkPower();
            if (oldPower != powered) {
                updated = true;
                updateRender();
            }
        }
        if (forceRender && !updated) {
            updateRender();
        }
    }

    private void updateRender() {
        if (!level().isClientSide)
            sendUpdate(this::writeDesc);
        tile().recalcLight(false, true);
        tile().markRender();
    }

    @Override
    public VoxelShape getShape(CollisionContext context) {
        return properties.getShape(side);
    }

    @Override
    public VoxelShape getOcclusionShape() {
        return getShape(CollisionContext.empty());
    }

    @Override
    public VoxelShape getBlockSupportShape() {
        return Shapes.empty();
    }

    @Override
    public MultipartType<?> getType() {
        return type;
    }

    @Override
    public float getStrength(Player player, PartRayTraceResult hit) {
        return 2/30f;
    }

    @Override
    public int getSlotMask() {
        return 1<<6;
    }

    @Override
    public Iterable<ItemStack> getDrops() {
        return Collections.singletonList(getItem());
    }

    @Override
    public ItemStack getCloneStack(PartRayTraceResult hit) {
        return getItem();
    }

    @Override
    public boolean canConnectRedstone(int side) {
        return true;
    }

    @Override
    public int strongPowerLevel(int side) {
        return 0;
    }

    @Override
    public int weakPowerLevel(int side) {
        return 0;
    }

    @Override
    public SoundType getPlacementSound(UseOnContext context) {
        return SoundType.GLASS;
    }

    public static boolean canPlaceLight(Level world, BlockPos pos, Direction side) {

        if (PlacementLib.canPlaceLight(world, pos, side)) return true;

        MultiPart part = BlockMultipart.getPart(world, pos, side.ordinal());
        if (part instanceof HollowMicroblockPart) return true;

        return false;
    }
}
