package mrtjp.projectred.expansion.tile;

import codechicken.multipart.api.tile.RedstoneConnector;
import mrtjp.projectred.core.CenterLookup;
import mrtjp.projectred.expansion.init.ExpansionBlocks;
import mrtjp.projectred.expansion.part.PneumaticTubePayload;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.WorldlyContainer;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.common.capabilities.ForgeCapabilities;

public class TransposerBlockEntity extends BasePneumaticDeviceBlockEntity implements RedstoneConnector {

    public TransposerBlockEntity(BlockPos pos, BlockState state) {
        super(ExpansionBlocks.TRANSPOSER_BLOCK_ENTITY.get(), pos, state);
    }

    //region BaseDeviceTile implementation
    @Override
    protected void onActivated() {
        if (importFromInventory()) return;
        suckEntities();
    }

    @Override
    protected void onDeactivated() {
    }
    //endregion

    protected int containerImportStackSize() {
        return 1;
    }

    private boolean importFromInventory() {

        var lookup = CenterLookup.lookupStraightCenter(getLevel(), getBlockPos(), side ^ 1);
        if (lookup.tile == null) return false;

        // Import from WorldlyContainer
        if (lookup.tile instanceof WorldlyContainer wc) {
            var extractDir = Direction.values()[lookup.otherDirection];
            var slots = wc.getSlotsForFace(extractDir);
            for (int s : slots) {
                var stack = wc.getItem(s);
                if (stack.isEmpty()) continue;
                if (!wc.canTakeItemThroughFace(s, stack, extractDir)) continue;

                int toImport = Math.max(stack.getMaxStackSize(), containerImportStackSize());
                var removed = wc.removeItem(s, toImport);
                if (removed.isEmpty()) continue;

                // Item extracted! Add to queue and return
                itemQueue.add(new PneumaticTubePayload(removed));
                return true;
            }
        }

        // Import from Item Handler capability
        var itemCapOpt = lookup.tile.getCapability(ForgeCapabilities.ITEM_HANDLER, Direction.values()[lookup.otherDirection]);
        if (itemCapOpt.isPresent()) {
            var itemCap = itemCapOpt.orElseThrow(NullPointerException::new);

            for (int s = 0; s < itemCap.getSlots(); s++) {
                var extracted = itemCap.extractItem(s, containerImportStackSize(), false);
                if (extracted.isEmpty()) continue;

                // Item extracted! Add to queue and return
                itemQueue.add(new PneumaticTubePayload(extracted));
                return true;
            }
        }

        return false;
    }

    private boolean suckEntities() {
        return false;
    }
    //endregion

    //region Redstone connection
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
