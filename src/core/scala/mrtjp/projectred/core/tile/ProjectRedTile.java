package mrtjp.projectred.core.tile;

import codechicken.lib.data.MCDataByteBuf;
import codechicken.lib.vec.Vector3;
import net.minecraft.block.BlockState;
import net.minecraft.entity.item.ItemEntity;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.tileentity.TileEntityType;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public abstract class ProjectRedTile extends TileEntity implements IBlockEventTile {

    public ProjectRedTile(TileEntityType<?> type) {
        super(type);
    }

    @Override
    public final CompoundNBT save(CompoundNBT tag) {
        super.save(tag);
        saveToNBT(tag);
        return tag;
    }

    @Override
    public final void load(BlockState state, CompoundNBT tag) {
        super.load(state, tag);
        loadFromNBT(tag);
    }

    @Override
    public final CompoundNBT getUpdateTag() {
        CompoundNBT tag = super.getUpdateTag();
        MCDataByteBuf out = new MCDataByteBuf();
        writeDesc(out);
        out.writeToNBT(tag, "descpkt");
        return tag;
    }

    @Override
    public final void handleUpdateTag(BlockState state, CompoundNBT tag) {
        super.handleUpdateTag(state, tag);
        MCDataByteBuf in = MCDataByteBuf.readFromNBT(tag, "descpkt");
        readDesc(in);
    }

    protected void pushBlockState() {
        if (!getBlockLevel().isClientSide) {
            BlockState currentState = getBlockLevel().getBlockState(getBlockPosition());
            BlockState newState = storeBlockState(currentState);
            getBlockLevel().setBlockAndUpdate(getBlockPosition(), newState);
        }
    }

    // Obfuscation bug: cant use getLevel name
    @Override
    public World getBlockLevel() {
        return getLevel();
    }

    // Obfuscation bug: cant use getBlockPos name
    @Override
    public BlockPos getBlockPosition() {
        return getBlockPos();
    }

    public static void dropItem(ItemStack stack, World world, Vector3 pos) {
        ItemEntity item = new ItemEntity(world, pos.x, pos.y, pos.z, stack);
        item.setDeltaMovement(world.random.nextGaussian() * 0.05, world.random.nextGaussian() * 0.05 + 0.2, world.random.nextGaussian() * 0.05);
        item.setPickUpDelay(10);
        world.addFreshEntity(item);
    }

    public static void dropInventory(IInventory inventory, World world, Vector3 pos) {
        for (int i = 0; i < inventory.getContainerSize(); i++) {
            ItemStack stack = inventory.getItem(i);
            if (!stack.isEmpty()) {
                dropItem(stack, world, pos);
                inventory.setItem(i, ItemStack.EMPTY);
            }
        }
    }
}
