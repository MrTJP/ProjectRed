/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.render.uv.MultiIconTransformation
import codechicken.lib.vec.BlockCoord
import codechicken.multipart.IRedstoneConnector
import mrtjp.core.block.TInstancedBlockRender
import mrtjp.core.render.TCubeMapRender
import mrtjp.core.world.WorldLib
import mrtjp.projectred.ProjectRedExpansion
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.init.Blocks
import net.minecraft.util.IIcon
import net.minecraft.world.IBlockAccess

class TileFireStarter extends TileMachine with TActiveDevice with IRedstoneConnector
{
    override def getBlock = ProjectRedExpansion.machine2

    override def doesRotate = false
    override def doesOrient = true

    override def onActivate()
    {
        val pos = new BlockCoord(x, y, z).offset(side^1)
        if (world.isAirBlock(pos.x, pos.y, pos.z))
        {
            world.setBlock(pos.x, pos.y, pos.z, Blocks.fire, 0, 3)
            world.playSoundEffect(pos.x+0.5D, pos.y+0.5D, pos.z+0.5D, "fire.ignite", 1.0F, world.rand.nextFloat*0.4F+0.8F)
        }
    }

    override def onDeactivate()
    {
        val pos = new BlockCoord(x, y, z).offset(side^1)
        val b = world.getBlock(pos.x, pos.y, pos.z)
        if (b == Blocks.fire || b == Blocks.portal)
            world.setBlockToAir(pos.x, pos.y, pos.z)
    }

    override def isFireSource(s:Int) = side == 0 && s == 1 && active

    override def getConnectionMask(side:Int) = if ((side^1) == this.side) 0 else 0x1F
    override def weakPowerLevel(side:Int, mask:Int) = 0
}

object RenderFireStarter extends TInstancedBlockRender with TCubeMapRender
{
    var bottom:IIcon = _
    var side1A:IIcon = _
    var side2A:IIcon = _
    var topA:IIcon = _
    var side1B:IIcon = _
    var side2B:IIcon = _
    var topB:IIcon = _

    override def getData(w:IBlockAccess, x:Int, y:Int, z:Int) =
    {
        val te = WorldLib.getTileEntity(w, x, y, z, classOf[TActiveDevice])
        if (te != null) (te.side, te.rotation, if (te.active || te.powered)
            new MultiIconTransformation(bottom, topB, side1B, side1B, side2B, side2B)
        else new MultiIconTransformation(bottom, topA, side1A, side1A, side2A, side2A))
        else (0, 0, new MultiIconTransformation(bottom, topA, side1A, side1A, side2A, side2A))
    }

    override def getInvData = (0, 0, new MultiIconTransformation(bottom, topA, side1A, side1A, side2A, side2A))

    override def getIcon(s:Int, meta:Int) = s match
    {
        case 0 => bottom
        case 1 => topA
        case 2 => side1A
        case 3 => side1A
        case 4 => side2A
        case 5 => side2A
        case _ => bottom
    }

    override def registerIcons(reg:IIconRegister)
    {
        def register(s:String) = reg.registerIcon(s"projectred:mechanical/fire/$s")
        bottom = register("bottom")

        side1A = register("side1A")
        side2A = register("side2A")
        topA = register("topA")

        side1B = register("side1B")
        side2B = register("side2B")
        topB = register("topB")
    }
}