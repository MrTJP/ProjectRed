/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.relocation

import java.util.{Set => JSet}

import mrtjp.projectred.api.{IMovementCallback, Relocator}
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World

import scala.collection.JavaConversions._
import scala.collection.mutable.{Set => MSet, Stack => MStack}

class RelocationRun
{
    var world:World = _
    var dir = -1
    var speed = 0.0
    var callback:IMovementCallback = _
    val blocks = MSet[BlockPos]()

    def clear()
    {
        world = null
        dir = -1
        speed = 0
        callback = null
        blocks.clear()
    }
}

object Relocator_Impl extends Relocator
{
    var mainStack = new MStack[RelocationRun]()
    var tempStack = new MStack[RelocationRun]()

    private def assertState()
    {
        if (mainStack.isEmpty) throw new IllegalStateException("Relocator stack is empty.")
    }

    override def push()
    {
        val r = if (tempStack.isEmpty) new RelocationRun else tempStack.pop()
        mainStack.push(r)
    }

    override def pop()
    {
        assertState()
        val r = mainStack.pop()
        r.clear()
        tempStack.push(r)
    }

    override def setWorld(world:World)
    {
        assertState()
        val top = mainStack.top
        if (top.world != null) throw new IllegalStateException("World already set.")
        top.world = world
    }

    override def setDirection(dir:Int)
    {
        assertState()
        val top = mainStack.top
        if (top.dir != -1) throw new IllegalStateException("Direction already set.")
        top.dir = dir
    }

    override def setSpeed(speed:Double)
    {
        assertState()
        val top = mainStack.top
        if (top.speed > 0) throw new IllegalStateException("Speed already set.")
        top.speed = speed
    }

    override def setCallback(callback:IMovementCallback)
    {
        assertState()
        val top = mainStack.top
        if (top.callback != null) throw new IllegalStateException("Callback already set.")
        top.callback = callback
    }

    override def addBlock(bc:BlockPos)
    {
        assertState()
        mainStack.top.blocks += bc
    }

    override def addBlocks(blocks:JSet[BlockPos])
    {
        for (b <- blocks) addBlock(b)
    }

    override def execute() =
    {
        assertState()
        val top = mainStack.top
        if (top.world == null) throw new IllegalStateException("World must be set before move.")
        if (top.world.isRemote) throw new IllegalStateException("Movements cannot be executed client-side.")
        if (top.dir == -1) throw new IllegalStateException("Direction must be set before move.")
        if (top.speed <= 0) throw new IllegalStateException("Speed must be greater than 0.")
        if (top.speed >= 1) throw new IllegalStateException("Speed must be less than 1.")
        if (top.blocks.isEmpty) throw new IllegalStateException("No blocks queued for move.")
        MovementManager.tryStartMove(top.world, top.blocks.toSet, top.dir, top.speed, top.callback)
    }
}