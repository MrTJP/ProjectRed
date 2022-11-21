package mrtjp.projectred.core.tile;

import codechicken.lib.data.MCDataByteBuf;
import net.minecraft.block.BlockState;
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
    public CompoundNBT save(CompoundNBT tag) {
        super.save(tag);
        saveToNBT(tag);
        return tag;
    }

    @Override
    public void load(BlockState state, CompoundNBT tag) {
        super.load(state, tag);
        loadFromNBT(tag);
    }

    @Override
    public CompoundNBT getUpdateTag() {
        CompoundNBT tag = super.getUpdateTag();
        MCDataByteBuf out = new MCDataByteBuf();
        writeDesc(out);
        out.writeToNBT(tag, "descpkt");
        return tag;
    }

    @Override
    public void handleUpdateTag(BlockState state, CompoundNBT tag) {
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
}
