/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.model.blockbakery.SimpleBlockRenderer
import codechicken.lib.vec.uv.{MultiIconTransformation, UVTransformation}
import codechicken.multipart.IRedstoneConnector
import mrtjp.core.world.WorldLib
import mrtjp.projectred.ProjectRedExpansion
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.init.{Blocks, SoundEvents}
import net.minecraft.item.ItemStack
import net.minecraft.tileentity.TileEntity
import net.minecraft.util.{EnumFacing, ResourceLocation, SoundCategory}
import net.minecraft.world.IBlockAccess
import net.minecraftforge.common.property.IExtendedBlockState

class TileFireStarter extends TileMachine with TActiveDevice with IRedstoneConnector
{
    override def getBlock = ProjectRedExpansion.machine2

    override def doesRotate = false
    override def doesOrient = true

    override def onActivate()
    {
        val pos = getPos.offset(EnumFacing.VALUES(side^1))
        if (world.isAirBlock(pos))
        {
            world.setBlockState(pos, Blocks.FIRE.getDefaultState, 3)
            world.playSound(null, pos.getX + 0.5D, pos.getY + 0.5D, pos.getZ + 0.5D, SoundEvents.ITEM_FLINTANDSTEEL_USE, SoundCategory.BLOCKS, 1.0F, world.rand.nextFloat*0.4F+0.8F)
        }
    }

    override def onDeactivate()
    {
        val pos = getPos.offset(EnumFacing.VALUES(side^1))
        val s = world.getBlockState(pos)
        if (s.getBlock == Blocks.FIRE || s.getBlock == Blocks.PORTAL)
            world.setBlockToAir(pos)
    }

    override def isFireSource(s:Int) = side == 0 && s == 1 && active

    override def getConnectionMask(side:Int) = if ((side^1) == this.side) 0 else 0x1F
    override def weakPowerLevel(side:Int, mask:Int) = 0
}

object RenderFireStarter extends SimpleBlockRenderer
{
    import java.lang.{Boolean => JBool, Integer => JInt}

    import mrtjp.core.util.CCLConversions._
    import mrtjp.projectred.expansion.BlockProperties._
    var bottom:TextureAtlasSprite = _
    var side1A:TextureAtlasSprite = _
    var side2A:TextureAtlasSprite = _
    var topA:TextureAtlasSprite = _
    var side1B:TextureAtlasSprite = _
    var side2B:TextureAtlasSprite = _
    var topB:TextureAtlasSprite = _

    var iconT1: UVTransformation = _
    var iconT2: UVTransformation = _

    override def handleState(state: IExtendedBlockState, tileEntity: TileEntity): IExtendedBlockState = tileEntity match {
        case t:TActiveDevice => {
            var s = state
            s = s.withProperty(UNLISTED_SIDE_PROPERTY, t.side.asInstanceOf[JInt])
            s = s.withProperty(UNLISTED_ROTATION_PROPERTY, t.rotation.asInstanceOf[JInt])
            s = s.withProperty(UNLISTED_ACTIVE_PROPERTY, t.active.asInstanceOf[JBool])
            s.withProperty(UNLISTED_POWERED_PROPERTY, t.powered.asInstanceOf[JBool])
        }
        case _ => state
    }

    override def getWorldTransforms(state: IExtendedBlockState) = {
        val side = state.getValue(UNLISTED_SIDE_PROPERTY)
        val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
        val active = state.getValue(UNLISTED_ACTIVE_PROPERTY).asInstanceOf[Boolean]
        val powered = state.getValue(UNLISTED_POWERED_PROPERTY).asInstanceOf[Boolean]
        createTriple(side, rotation, if (active || powered) iconT2 else iconT1)
    }

    override def getItemTransforms(stack:ItemStack) = createTriple(0, 0, iconT1)

    override def shouldCull() = true

    def getIcon(s:Int, meta:Int) = s match
    {
        case 0 => bottom
        case 1 => topA
        case 2 => side1A
        case 3 => side1A
        case 4 => side2A
        case 5 => side2A
        case _ => bottom
    }

    override def registerIcons(reg:TextureMap)
    {
        def register(s:String) = reg.registerSprite(new ResourceLocation(s"projectred:blocks/mechanical/fire/$s"))
        bottom = register("bottom")

        side1A = register("side1A")
        side2A = register("side2A")
        topA = register("topA")

        side1B = register("side1B")
        side2B = register("side2B")
        topB = register("topB")

        iconT1 = new MultiIconTransformation(bottom, topA, side1A, side1A, side2A, side2A)
        iconT2 = new MultiIconTransformation(bottom, topB, side1B, side1B, side2B, side2B)
    }
}
