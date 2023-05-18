/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.api;

import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;

/**
 * Class used instead of {@link IFrame} to add frame capabilities to a block without having
 * the block or tile in that location implement the IFrame interface or capability. This
 * class must be registered in the {@link IRelocationAPI}.
 */
public interface IFrameInteraction extends IFrame {
    /**
     * Check to see if this interaction can be executed at the given
     * coordinates.
     *
     * @param w The world.
     * @param pos The coordinates.
     * @return True if this interaction is valid for the given
     * location.
     */
    boolean canInteract(Level w, BlockPos pos);
}