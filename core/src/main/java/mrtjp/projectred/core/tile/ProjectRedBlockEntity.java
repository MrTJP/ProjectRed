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

public abstract class ProjectRedBlockEntity extends BlockEntity implements IBlockEventBlockEntity {

    public ProjectRedBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
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

    public static void ejectItem(ItemStack stack, Level world, BlockPos pos, int dir, double velocity) {
        Vector3 dirVec = switch (dir) {
            case 0 -> new Vector3(0, -1, 0);
            case 1 -> new Vector3(0, 1, 0);
            case 2 -> new Vector3(0, 0, -1);
            case 3 -> new Vector3(0, 0, 1);
            case 4 -> new Vector3(-1, 0, 0);
            case 5 -> new Vector3(1, 0, 0);
            default -> throw new IllegalStateException("Unexpected value: " + dir);
        };

        Vector3 itemPos = Vector3.fromBlockPosCenter(pos).add(dirVec.copy().multiply(0.6));
        Vector3 itemMotion = dirVec.copy().multiply(velocity);

        ItemEntity item = new ItemEntity(world, itemPos.x, itemPos.y, itemPos.z, stack);
        item.setDeltaMovement(itemMotion.x, itemMotion.y, itemMotion.z);
        item.setPickUpDelay(10);
        world.addFreshEntity(item);
    }
}
