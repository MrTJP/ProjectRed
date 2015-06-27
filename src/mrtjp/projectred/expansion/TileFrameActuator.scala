/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.render.uv.{MultiIconTransformation, UVTransformation}
import mrtjp.core.render.TCubeMapRender
import mrtjp.core.world.WorldLib
import mrtjp.projectred.ProjectRedExpansion
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.util.IIcon
import net.minecraft.world.{IBlockAccess, World}

class TileFrameActuator extends TileMachine with TMotorTile
{
    override def getBlock = ProjectRedExpansion.machine2
    override def doesRotate = false
    override def doesOrient = true

    override def stickOut(w:World, x:Int, y:Int, z:Int, side:Int) = side == (this.side^1)
    override def stickIn(w:World, x:Int, y:Int, z:Int, side:Int) = side != (this.side^1)

    override def getMoveDir = side^1

    override def drawPower(size:Int) = cond.drawPower(200+10*size)
}

object RenderFrameActuator extends TCubeMapRender
{
    var bottom:IIcon = _
    var top:IIcon = _
    var sidea:IIcon = _
    var sideb:IIcon = _
    var sidec:IIcon = _

    var iconT1:UVTransformation = null
    var iconT2:UVTransformation = null
    var iconT3:UVTransformation = null

    override def getData(w:IBlockAccess, x:Int, y:Int, z:Int) =
    {
        val te = WorldLib.getTileEntity(w, x, y, z, classOf[TileFrameActuator])
        if (te != null) (te.side, te.rotation,
                if (te.isCharged && te.isPowered) iconT3
                else if (te.isCharged) iconT2
                else iconT1)
        else getInvData
    }

    override def getInvData = (0, 0, iconT1)

    override def getIcon(s:Int, meta:Int) = s match
    {
        case 0 => bottom
        case 1 => top
        case _ => sidea
    }

    override def registerIcons(reg:IIconRegister)
    {
        bottom = reg.registerIcon("projectred:machines/actuator/bottom")
        top = reg.registerIcon("projectred:machines/actuator/top")
        sidea = reg.registerIcon("projectred:machines/actuator/sidea")
        sideb = reg.registerIcon("projectred:machines/actuator/sideb")
        sidec = reg.registerIcon("projectred:machines/actuator/sidec")

        iconT1 = new MultiIconTransformation(bottom, top, sidea, sidea, sidea, sidea)
        iconT2 = new MultiIconTransformation(bottom, top, sideb, sideb, sideb, sideb)
        iconT3 = new MultiIconTransformation(bottom, top, sidec, sidec, sidec, sidec)
    }
}