/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.api;

import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.CapabilityManager;
import net.minecraftforge.common.capabilities.CapabilityToken;

public interface IRelocationAPI
{
    /** The capability instance for {@link IFrame} */
    Capability<IFrame> FRAME_CAPABILITY = CapabilityManager.get(new CapabilityToken<>() {});

    /** The capability instance for {@link IConditionallyMovable} */
    Capability<IConditionallyMovable> CONDITIONALLY_MOVABLE_CAPABILITY = CapabilityManager.get(new CapabilityToken<>() {});

    /**
     * Used to register a {@link ITileMover} class that manages the movement of certain tiles.
     * This method must be called during FML pre-initialization.
     *
     * @param name  The name of the mover that will be used to assign blocks to it.
     * @param desc  The description of what this mover is used for or why it is included.
     * @param mover The {@link ITileMover} to register.
     */
    void registerTileMover(String name, String desc, ITileMover mover);

    /**
     * Used to register an in-game block to a specific {@link ITileMover} which are registered
     * with the method above. This method adds the key-value pairs similar to the config,
     * but their assignments can be changed from the config file if you would like.
     * This method must be called during FML pre-initialization.
     *
     * @param key   The mod or block key regex. This works the same way it does from the
     *              config file:
     *              Possible keys:
     *              default - to assign default mover
     *              mod:<modID> - to assign every block from a mod
     *              <modID>:<blockname> - to assign block from a mod for every meta
     *              <modID>:<blockname>m<meta> - to assign block from mod for specific meta
     * @param value The name of the mover to assign this block. A list of all available
     *              movers will show up in the configs.
     */
    void registerPreferredMover(String key, String value);

    /**
     * Used to register an in-game block to a specific {@link ITileMover} which are registered
     * with the method above. This method adds the key-value pairs similar to the config. These
     * assignments are manatory, and blocks that are registered will be locked to the specified
     * mover, and it cannot be changed from the config.
     * This method must be called during FML pre-initialization.
     *
     * @param key   The mod or block key regex. This works the same way it does from the
     *              config file:
     *              Possible keys:
     *              default - to assign default mover
     *              mod:<modID> - to assign every block from a mod
     *              <modID>:<blockname> - to assign block from a mod for every meta
     *              <modID>:<blockname>m<meta> - to assign block from mod for specific meta
     * @param value The name of the mover to assign this block. A list of all available
     *              movers will show up in the configs.
     */
    void registerMandatoryMover(String key, String value);

    /**
     * Used to register a {@link IFrameInteraction}, which is a class that
     * can be used to add frame-like properties to any block.
     *
     * @param interaction The interaction to register.
     */
    void registerFrameInteraction(IFrameInteraction interaction);

    /**
     * Getter for the global Relocator object which is what is used
     * to actually initiate movements.
     *
     * @return The Relocator object
     */
    Relocator getRelocator();

    /**
     * Getter for the optional StickResolver object which can be used to quicky
     * resolve a structure based on default {@link IFrame} implmementation and
     * and Relocation stick rules.
     *
     * @return The StickResolver object
     */
    StickResolver getStickResolver();

    /**
     * Used to check if the given block is currently moving. This method is
     * client and server safe.
     *
     * @param world The world the block is in.
     * @param pos   The position of the block.
     * @return True if the block is currently moving.
     */
    boolean isMoving(Level world, BlockPos pos);
}