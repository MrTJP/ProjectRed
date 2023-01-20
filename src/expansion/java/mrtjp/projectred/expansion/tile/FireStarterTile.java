package mrtjp.projectred.expansion.tile;

import codechicken.multipart.api.tile.IRedstoneConnector;
import mrtjp.projectred.expansion.init.ExpansionReferences;
import net.minecraft.block.AbstractFireBlock;
import net.minecraft.block.BlockState;
import net.minecraft.block.Blocks;
import net.minecraft.block.CampfireBlock;
import net.minecraft.state.properties.BlockStateProperties;
import net.minecraft.util.Direction;
import net.minecraft.util.SoundCategory;
import net.minecraft.util.SoundEvents;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public class FireStarterTile extends BaseDeviceTile implements IRedstoneConnector {

    public FireStarterTile() {
        super(ExpansionReferences.FIRE_STARTER_TILE);
    }

    @Override
    protected void onActivated() {

        // Emulates flint and steel right-click
        BlockPos pos = getBlockPos().relative(Direction.values()[side ^ 1]); //Block in front of fire starter
        emulateFlintAndSteel(pos);
    }

    private boolean emulateFlintAndSteel(BlockPos pos) {
        BlockState state = getLevel().getBlockState(pos);
        if (CampfireBlock.canLight(state)) {
            playFlintAndSteelSound(getLevel(), pos);
            getLevel().setBlock(pos, state.setValue(BlockStateProperties.LIT, true), 11);
            return true;
        }

        if (AbstractFireBlock.canBePlacedAt(getLevel(), pos, Direction.NORTH)) {
            playFlintAndSteelSound(getLevel(), pos);
            getLevel().setBlock(pos, AbstractFireBlock.getState(getLevel(), pos), 11);
            return true;
        }

        return false;
    }

    private static void playFlintAndSteelSound(World level, BlockPos pos) {
        level.playSound(null,
                pos.getX() + 0.5D,
                pos.getY() + 0.5D,
                pos.getZ() + 0.5D,
                SoundEvents.FLINTANDSTEEL_USE,
                SoundCategory.BLOCKS,
                1.0F,
                level.getRandom().nextFloat() * 0.4F + 0.8F);
    }

    @Override
    protected void onDeactivated() {
        BlockPos pos = getBlockPos().relative(Direction.values()[side ^ 1]);
        BlockState state = getLevel().getBlockState(pos);

        if (CampfireBlock.isLitCampfire(state)) {
            getLevel().setBlock(pos, state.setValue(BlockStateProperties.LIT, false), 11);
            return;
        }

        if (state.is(Blocks.FIRE) || state.is(Blocks.SOUL_FIRE) || state.is(Blocks.NETHER_PORTAL)) {
            getLevel().setBlock(pos, Blocks.AIR.defaultBlockState(), 11);
        }
    }

    @Override
    public boolean isFireSource(int side) {
        return active && this.side == (side^1); // If active, top face keeps fire lit
    }

    //region IRedstoneConnector implementation
    @Override
    public int getConnectionMask(int side) {
        return (((side ^ 1) == this.side) ? 0 : 0x1F);
    }

    @Override
    public int weakPowerLevel(int side, int mask) {
        return 0;
    }
    //endregion
}
