/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.api;

import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;

import java.util.Set;

/**
 * Helper class used to resolve frame structures.
 */
public abstract class StickResolver {
    /**
     * Method used to obtain the block coordinates of every block that is part of the same
     * framed structure as the one that was given as an argument.
     *
     * @param world      The world
     * @param pos        The position of any block in the structure.
     * @param exclusions All coordinates to not include in the structure.
     *                   generally, one of these is the motor block that moved the structure.
     * @return A set of all block coordinates that are part of the structure that the input block was in.
     */
    public abstract Set<BlockPos> getStructure(Level world, BlockPos pos, BlockPos... exclusions);
}