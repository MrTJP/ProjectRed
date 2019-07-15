/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.model.bakery.SimpleBlockRenderer
import codechicken.lib.vec.uv.{MultiIconTransformation, UVTransformation}
import codechicken.multipart.IRedstoneConnector
import mrtjp.core.item.ItemKey
import mrtjp.projectred.ProjectRedExpansion
import net.minecraft.block.Block
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.init.Blocks
import net.minecraft.item.ItemStack
import net.minecraft.util.math.BlockPos
import net.minecraft.util.{EnumFacing, ResourceLocation}
import net.minecraft.world.IBlockAccess
import net.minecraftforge.common.property.IExtendedBlockState

import scala.collection.JavaConversions._

class TileBlockBreaker extends TileMachine with TPressureActiveDevice with IRedstoneConnector with TNonStickableFrontFace
{
    override def getBlock = ProjectRedExpansion.machine2
    override def doesRotate = false
    override def doesOrient = true

    override def canAcceptInput(item:ItemKey, side:Int) = false
    override def canAcceptBacklog(item:ItemKey, side:Int) = side == this.side
    override def canConnectSide(side:Int) = side == this.side

    override def onActivate()
    {
        val bc = getPos.offset(EnumFacing.VALUES(side^1))
        val state = world.getBlockState(bc)

        if (state.getBlock == Blocks.BEDROCK) return
        if (state.getBlock.isAir(state, world, bc)) return
        if (state.getBlockHardness(world, bc) < 0) return
        if (state.getBlock.getHarvestLevel(state) > getHarvestLevel) return

        state.getBlock.getDrops(world, bc, state, 0).foreach(itemStorage.add)
        world.playEvent(null, 2001, getPos, Block.getStateId(state))
        world.setBlockToAir(bc)
        exportBuffer()
    }

    def getHarvestLevel = 2

    override def getConnectionMask(side:Int) = if ((side^1) == this.side) 0 else 0x1F
    override def weakPowerLevel(side:Int, mask:Int) = 0
}

class TileDiamondBlockBreaker extends TileBlockBreaker
{
    override def getHarvestLevel = 3
}

class RenderBlockBreakerBase(spriteFolder:String) extends SimpleBlockRenderer
{
    import java.lang.{Boolean => JBool, Integer => JInt}

    import mrtjp.projectred.expansion.BlockProperties._
    import org.apache.commons.lang3.tuple.Triple

    var bottom:TextureAtlasSprite = _
    var side1:TextureAtlasSprite = _
    var top1:TextureAtlasSprite = _
    var side2:TextureAtlasSprite = _
    var top2:TextureAtlasSprite = _

    var iconT1:UVTransformation = _
    var iconT2:UVTransformation = _

    override def handleState(state:IExtendedBlockState, world:IBlockAccess, pos:BlockPos):IExtendedBlockState = world.getTileEntity(pos) match {
        case t:TActiveDevice => {
            var s = state
            s = s.withProperty(UNLISTED_SIDE_PROPERTY, t.side.asInstanceOf[JInt])
            s = s.withProperty(UNLISTED_ROTATION_PROPERTY, t.rotation.asInstanceOf[JInt])
            s = s.withProperty(UNLISTED_ACTIVE_PROPERTY, t.active.asInstanceOf[JBool])
            s.withProperty(UNLISTED_POWERED_PROPERTY, t.powered.asInstanceOf[JBool])
        }
        case _ => state
    }

    override def getWorldTransforms(state:IExtendedBlockState) = {
        val side = state.getValue(UNLISTED_SIDE_PROPERTY)
        val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
        val active = state.getValue(UNLISTED_ACTIVE_PROPERTY).asInstanceOf[Boolean]
        val powered = state.getValue(UNLISTED_POWERED_PROPERTY).asInstanceOf[Boolean]
        Triple.of(side, rotation, if (active || powered) iconT2 else iconT1)
    }

    override def getItemTransforms(stack:ItemStack) = Triple.of(0, 0, iconT1)

    override def shouldCull() = true

    override def registerIcons(reg:TextureMap)
    {
        bottom = reg.registerSprite(new ResourceLocation(s"projectred:blocks/mechanical/$spriteFolder/bottom"))
        top1 = reg.registerSprite(new ResourceLocation(s"projectred:blocks/mechanical/$spriteFolder/top1"))
        side1 = reg.registerSprite(new ResourceLocation(s"projectred:blocks/mechanical/$spriteFolder/side1"))
        top2 = reg.registerSprite(new ResourceLocation(s"projectred:blocks/mechanical/$spriteFolder/top2"))
        side2 = reg.registerSprite(new ResourceLocation(s"projectred:blocks/mechanical/$spriteFolder/side2"))

        iconT1 = new MultiIconTransformation(bottom, top1, side1, side1, side1, side1)
        iconT2 = new MultiIconTransformation(bottom, top2, side2, side2, side2, side2)
    }
}

object RenderBlockBreaker extends RenderBlockBreakerBase("breaker")

object RenderDiamondBlockBreaker extends RenderBlockBreakerBase("dbreaker")