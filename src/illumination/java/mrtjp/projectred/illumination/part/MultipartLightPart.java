package mrtjp.projectred.illumination.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Vector3;
import codechicken.microblock.HollowMicroblock;
import codechicken.multipart.api.MultiPartType;
import codechicken.multipart.api.NormalOcclusionTest;
import codechicken.multipart.api.RedstoneInteractions;
import codechicken.multipart.api.part.TMultiPart;
import codechicken.multipart.api.part.TNormalOcclusionPart;
import codechicken.multipart.api.part.TSlottedPart;
import codechicken.multipart.api.part.redstone.IRedstonePart;
import codechicken.multipart.block.BlockMultiPart;
import codechicken.multipart.block.TileMultiPart;
import codechicken.multipart.util.PartRayTraceResult;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.projectred.core.PlacementLib;
import mrtjp.projectred.core.client.HaloRenderer;
import mrtjp.projectred.illumination.MultipartLightProperties;
import net.minecraft.block.SoundType;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.item.ItemUseContext;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.shapes.ISelectionContext;
import net.minecraft.util.math.shapes.VoxelShape;
import net.minecraft.world.World;

import java.util.Collections;

public class MultipartLightPart extends TMultiPart implements TSlottedPart, TNormalOcclusionPart, IRedstonePart {

    private final MultiPartType<?> type;
    private final MultipartLightProperties properties;
    private final int color;
    private final boolean inverted;

    protected boolean powered = false;
    protected int side = 0;

    public MultipartLightPart(MultiPartType<?> type, MultipartLightProperties properties, int color, boolean inverted) {
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

    public void preparePlacement(int side) {
        this.side = side;
    }

    @Override
    public void save(CompoundNBT tag) {
        tag.putBoolean("pow", powered);
        tag.putByte("side", (byte) side);
    }

    @Override
    public void load(CompoundNBT tag) {
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
    public void onPartChanged(TMultiPart part) {
        if (checkSupport()) return;
        updateState(false);
    }

    @Override
    public void onAdded() {
        if (checkSupport()) return;
        updateState(true);
    }

    @Override
    public int getLightValue() {
        return (isInverted() != powered) ? 15 : 0;
    }

    private ItemStack getItem() {
        return properties.makeStack(color, inverted);
    }

    private boolean checkSupport() {
        if (world().isClientSide) return false;
        if (properties.canFloat()) return false;

        BlockPos bc = pos().relative(Direction.values()[side]);
        if (MultipartLightPart.canPlaceLight(world(), bc, Direction.values()[side^1])) return false;

        TileMultiPart.dropItem(getItem(), world(), Vector3.fromTileCenter(tile()));
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
        if (!world().isClientSide) {
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
        if (!world().isClientSide)
            sendUpdate(this::writeDesc);
        tile().recalcLight(false, true);
        tile().markRender();
    }

    @Override
    public boolean occlusionTest(TMultiPart npart) {
        return NormalOcclusionTest.test(this, npart) && super.occlusionTest(npart);
    }

    @Override
    public VoxelShape getShape(ISelectionContext context) {
        return properties.getShape(side);
    }

    @Override
    public VoxelShape getOcclusionShape() {
        return getShape(ISelectionContext.empty());
    }

    @Override
    public MultiPartType<?> getType() {
        return type;
    }

    @Override
    public float getStrength(PlayerEntity player, PartRayTraceResult hit) {
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
    public ItemStack pickItem(PartRayTraceResult hit) {
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
    public SoundType getPlacementSound(ItemUseContext context) {
        return SoundType.GLASS;
    }

    @Override
    public boolean renderStatic(RenderType layer, CCRenderState ccrs) {
        if (layer == null || layer == RenderType.cutout()) {
            ccrs.setBrightness(world(), pos());
            properties.render(this, Vector3.ZERO, ccrs);
            return true;
        }
        return false;
    }

    @Override
    public void renderDynamic(MatrixStack mStack, IRenderTypeBuffer buffers, int packedLight, int packedOverlay, float partialTicks) {
//        if (isOn)
//            RenderHalo.addLight(pos, getColor, getLightBounds) //TODO RenderWorldLastEvent rendering is broken
        if (isLightOn())
            HaloRenderer.renderHalo(CCRenderState.instance(), mStack, buffers, properties.getGlowBounds(getSide()), getColor(), Vector3.ZERO);
    }

    public static boolean canPlaceLight(World world, BlockPos pos, Direction side) {

        if (PlacementLib.canPlaceLight(world, pos, side)) return true;

        TMultiPart part = BlockMultiPart.getPart(world, pos, side.ordinal());
        if (part instanceof HollowMicroblock) return true;

        return false;
    }
}
