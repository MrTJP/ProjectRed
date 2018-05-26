/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.api;

import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.CapabilityInject;

/**
 * Interface that can be implemented on Blocks or as a tile capability that wish to act as frames,
 * which are the blocks that stick together and form a moving structure when moved through the
 * {@link Relocator}. No other action besides implementation of this interface is needed for the
 * block to function.
 */
public interface IFrame
{
    @CapabilityInject(IFrame.class)
    Capability<IFrame> CAPABILITY = null;

    /**
     * Used to check if this frame block is allowed to grab
     * a block on the given side.
     * <p>
     * Convention requires that this method yields the same result
     * on both client and server.
     *
     * @param w    The world.
     * @param pos  The block's position.
     * @param side The side of this block.
     * @return True if the side can grab another block.
     */
    boolean stickOut(World w, BlockPos pos, EnumFacing side);

    /**
     * Used to check if this frame block can be grabbed on the
     * given side by another frame.
     * <p>
     * Convention requires that this method yields the same result
     * on both client and server.
     *
     * @param w    The world.
     * @param pos  The block's position.
     * @param side The side of this block.
     * @return True if the side can be grabbed by a frame block.
     */
    boolean stickIn(World w, BlockPos pos, EnumFacing side);
}