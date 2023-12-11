package mrtjp.projectred.expansion.tile;

import codechicken.multipart.api.tile.RedstoneConnector;
import mrtjp.projectred.expansion.init.ExpansionBlocks;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.sounds.SoundSource;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.CampfireBlock;
import net.minecraft.world.level.block.FireBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;

public class FireStarterTile extends BaseDeviceTile implements RedstoneConnector {

    public FireStarterTile(BlockPos pos, BlockState state) {
        super(ExpansionBlocks.FIRE_STARTER_TILE.get(), pos, state);
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

        if (FireBlock.canBePlacedAt(getLevel(), pos, Direction.NORTH)) {
            playFlintAndSteelSound(getLevel(), pos);
            getLevel().setBlock(pos, FireBlock.getState(getLevel(), pos), 11);
            return true;
        }

        return false;
    }

    private static void playFlintAndSteelSound(Level level, BlockPos pos) {
        level.playSound(null,
                pos.getX() + 0.5D,
                pos.getY() + 0.5D,
                pos.getZ() + 0.5D,
                SoundEvents.FLINTANDSTEEL_USE,
                SoundSource.BLOCKS,
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
