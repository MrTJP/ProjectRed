package mrtjp.projectred.expansion.tile;

import codechicken.multipart.api.tile.RedstoneConnector;
import com.mojang.authlib.GameProfile;
import mrtjp.projectred.expansion.init.ExpansionBlocks;
import mrtjp.projectred.expansion.part.PneumaticTubePayload;
import mrtjp.projectred.expansion.pneumatics.PneumaticTransportMode;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Tier;
import net.minecraft.world.item.Tiers;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.state.BlockState;
import net.neoforged.neoforge.common.TierSortingRegistry;

import java.util.List;
import java.util.UUID;

import static mrtjp.projectred.expansion.pneumatics.PneumaticTransportMode.PASSIVE_BACKSTUFF;

public class BlockBreakerBlockEntity extends BasePneumaticDeviceBlockEntity implements RedstoneConnector {

    // Fake player ID used to break blocks
    private static final GameProfile PR_FAKE_PLAYER = new GameProfile(UUID.fromString("6140461b-e5b4-41ba-beb1-dce616e6abc0"), "[ProjectRed]");

    public BlockBreakerBlockEntity(BlockPos pos, BlockState state) {
        super(ExpansionBlocks.BLOCK_BREAKER_BLOCK_ENTITY.get(), pos, state);
    }

    //region PneumaticTransportDevice implementation
    @Override
    public boolean canConnectTube(int s) {
        return s == side;
    }

    @Override
    public boolean canAcceptPayload(int s, PneumaticTubePayload payload, PneumaticTransportMode mode) {
        return s == side && mode == PASSIVE_BACKSTUFF;
    }
    //endregion

    //region BaseDeviceTile implementation
    @Override
    protected void onActivated() {
        var pos = getBlockPos().relative(Direction.values()[side ^ 1]);
        var state = getLevel().getBlockState(pos);

        if (state.isAir() || state.is(Blocks.BEDROCK)) return;
        if (!TierSortingRegistry.isCorrectTierForDrops(getBreakingTier(), state)) return;

        // Get drops
        List<ItemStack> drops = Block.getDrops(state, (ServerLevel) getLevel(), pos, getLevel().getBlockEntity(pos));

        // Break block
        getLevel().destroyBlock(pos, false);

        //TODO eventually do this right with a fake player and in-world item capturing so we can
        //     get drops of inventory contents, etc.
//        // Set up fake player
//        var player = FakePlayerFactory.get((ServerLevel) getLevel(), PR_FAKE_PLAYER);
//        player.setItemSlot(EquipmentSlot.MAINHAND, new ItemStack(Items.IRON_PICKAXE));
//
//        // Set up item capturing event
//        // TODO
//
//        // Break block. This goes through a cancelable event
//        if (!player.gameMode.destroyBlock(pos)) {
//            return; // Don't drop if destroying was stopped
//        }

        // Add drops to queue
        for (ItemStack drop : drops) {
            itemQueue.add(new PneumaticTubePayload(drop));
        }
        exportQueue();
    }

    @Override
    protected void onDeactivated() {
    }
    //endregion

    protected Tier getBreakingTier() {
        return Tiers.IRON;
    }
}
