package mrtjp.projectred.api;

import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;

/**
 * Alternative to implementing {@link Frame} directly on a block or block entity, or using the block entity capability instance
 * at {@link IExpansionAPI#FRAME_CAPABILITY}. This is the least preferred method of implementing frame logic.
 * <p>
 * This class must be registered via {@link IExpansionAPI#registerFrameInteraction(FrameInteraction)}.
 */
public interface FrameInteraction extends Frame {

    /**
     * Check to see if this interaction is applicable to the given position.
     *
     * @param level Level
     * @param pos   Position
     * @return True if this interaction is valid for the given location
     */
    boolean canInteract(Level level, BlockPos pos);
}