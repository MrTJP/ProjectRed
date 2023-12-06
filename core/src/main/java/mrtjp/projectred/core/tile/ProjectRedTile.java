package mrtjp.projectred.core.tile;

import codechicken.lib.data.MCDataByteBuf;
import codechicken.lib.vec.Vector3;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.Container;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

import java.util.Objects;

public abstract class ProjectRedTile extends BlockEntity implements IBlockEventTile {

    public ProjectRedTile(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
    }

    @Override
    protected void saveAdditional(CompoundTag tag) {
        super.saveAdditional(tag);
        saveToNBT(tag);
    }

    @Override
    public void load(CompoundTag tag) {
        super.load(tag);
        loadFromNBT(tag);
    }

    @Override
    public final CompoundTag getUpdateTag() {
        CompoundTag tag = super.getUpdateTag();
        MCDataByteBuf out = new MCDataByteBuf();
        writeDesc(out);
        out.writeToNBT(tag, "descpkt");
        return tag;
    }

    @Override
    public void handleUpdateTag(CompoundTag tag) {
        super.handleUpdateTag(tag);
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
    public Level getBlockLevel() {
        return Objects.requireNonNull(getLevel());
    }

    // Obfuscation bug: cant use getBlockPos name
    @Override
    public BlockPos getBlockPosition() {
        return getBlockPos();
    }

    public static void dropItem(ItemStack stack, Level world, Vector3 pos) {
        ItemEntity item = new ItemEntity(world, pos.x, pos.y, pos.z, stack);
        item.setDeltaMovement(world.random.nextGaussian() * 0.05, world.random.nextGaussian() * 0.05 + 0.2, world.random.nextGaussian() * 0.05);
        item.setPickUpDelay(10);
        world.addFreshEntity(item);
    }

    public static void dropInventory(Container inventory, Level world, Vector3 pos) {
        for (int i = 0; i < inventory.getContainerSize(); i++) {
            ItemStack stack = inventory.getItem(i);
            if (!stack.isEmpty()) {
                dropItem(stack, world, pos);
                inventory.setItem(i, ItemStack.EMPTY);
            }
        }
    }
}
