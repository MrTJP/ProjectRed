/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.model.bakery.SimpleBlockRenderer
import codechicken.lib.vec.uv.{MultiIconTransformation, UVTransformation}
import mrtjp.projectred.ProjectRedExpansion
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.item.ItemStack
import net.minecraft.util.math.BlockPos
import net.minecraft.util.{EnumFacing, ResourceLocation}
import net.minecraft.world.{IBlockAccess, World}
import net.minecraftforge.common.property.IExtendedBlockState

class TileFrameActuator extends TileMachine with TMotorTile
{
    override def getBlock = ProjectRedExpansion.machine2
    override def doesRotate = false
    override def doesOrient = true

    override def stickOut(w:World, pos:BlockPos, side:EnumFacing) = side.ordinal == (this.side^1)
    override def stickIn(w:World, pos:BlockPos, side:EnumFacing) = side.ordinal != (this.side^1)

    override def getMoveDir = side^1

    override def drawPower(size:Int) = cond.drawPower(200+20*size)
}

object RenderFrameActuator extends SimpleBlockRenderer
{
    import java.lang.{Boolean => JBool, Integer => JInt}

    import mrtjp.projectred.expansion.BlockProperties._
    import org.apache.commons.lang3.tuple.Triple

    var bottom:TextureAtlasSprite = _
    var top:TextureAtlasSprite = _
    var sidea:TextureAtlasSprite = _
    var sideb:TextureAtlasSprite = _
    var sidec:TextureAtlasSprite = _

    var iconT1:UVTransformation = _
    var iconT2:UVTransformation = _
    var iconT3:UVTransformation = _

    override def handleState(state:IExtendedBlockState, world:IBlockAccess, pos:BlockPos) = world.getTileEntity(pos) match {
        case t:TileFrameActuator =>
            var s = state
            s = s.withProperty(UNLISTED_SIDE_PROPERTY, t.side.asInstanceOf[Integer])
            s = s.withProperty(UNLISTED_ROTATION_PROPERTY, t.rotation.asInstanceOf[JInt])
            s = s.withProperty(UNLISTED_WORKING_PROPERTY, t.isMoving.asInstanceOf[JBool])
            s = s.withProperty(UNLISTED_CHARGED_PROPERTY, t.isCharged.asInstanceOf[JBool])
            s
        case _ => state
    }

    override def getWorldTransforms(state:IExtendedBlockState) = {
        val side = state.getValue(UNLISTED_SIDE_PROPERTY)
        val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
        val isWorking = state.getValue(UNLISTED_WORKING_PROPERTY)
        val isCharged = state.getValue(UNLISTED_CHARGED_PROPERTY)
        Triple.of(side, rotation,
            if (isWorking && isCharged) iconT3
            else if (isCharged) iconT2
            else iconT1)
    }

    override def getItemTransforms(stack:ItemStack) = Triple.of(0, 0, iconT1)

    override def shouldCull() = true

    override def registerIcons(reg:TextureMap)
    {
        bottom = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/actuator/bottom"))
        top = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/actuator/top"))
        sidea = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/actuator/sidea"))
        sideb = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/actuator/sideb"))
        sidec = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/actuator/sidec"))

        iconT1 = new MultiIconTransformation(bottom, top, sidea, sidea, sidea, sidea)
        iconT2 = new MultiIconTransformation(bottom, top, sideb, sideb, sideb, sideb)
        iconT3 = new MultiIconTransformation(bottom, top, sidec, sidec, sidec, sidec)
    }
}
