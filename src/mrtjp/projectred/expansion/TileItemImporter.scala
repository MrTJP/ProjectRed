/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import java.util.{List => JList}

import codechicken.lib.model.bakery.SimpleBlockRenderer
import codechicken.lib.vec.uv.{MultiIconTransformation, UVTransformation}
import codechicken.lib.vec.{Cuboid6, Rotation, Vector3}
import codechicken.multipart.IRedstoneConnector
import mrtjp.core.inventory.InvWrapper
import mrtjp.core.item.ItemKey
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.expansion.TileItemImporter._
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.entity.Entity
import net.minecraft.entity.item.EntityItem
import net.minecraft.item.ItemStack
import net.minecraft.tileentity.TileEntity
import net.minecraft.util.math.BlockPos
import net.minecraft.util.{EnumFacing, ResourceLocation}
import net.minecraft.world.IBlockAccess
import net.minecraftforge.common.property.IExtendedBlockState

import scala.collection.JavaConversions._

class TileItemImporter extends TileMachine with TPressureActiveDevice with IRedstoneConnector
{
    override def getBlock = ProjectRedExpansion.machine2

    override def getCollisionBounds = TileItemImporter.cbounds(side)

    override def doesRotate = false
    override def doesOrient = true

    //side = out, side^1 = in
    override def canAcceptInput(item:ItemKey, side:Int) = (side^1) == this.side && !powered && itemStorage.isEmpty
    override def canAcceptBacklog(item:ItemKey, side:Int) = side == this.side
    override def canConnectSide(side:Int) = (side&6) == (this.side&6)

    override def onActivate()
    {
        if (importInv() || importEntities())
            return
    }

    def getExtractAmount = 1

    def importInv():Boolean =
    {
        val s = EnumFacing.VALUES(side)
        val inv = InvWrapper.wrap(world, getPos.offset(s.getOpposite), s)
        if (inv == null) return false
        val list = inv.getAllItemStacks
        for ((k, v) <- list) if (canImport(k))
        {
            val toExtract = math.min(k.getMaxStackSize, getExtractAmount)
            val extracted = inv.extractItem(k, toExtract)

            if (extracted > 0)
            {
                itemStorage.add(k.makeStack(extracted))
                active = true
                sendStateUpdate()
                scheduleTick(4)
                exportBuffer()
                return true
            }
            return false
        }
        false
    }

    def importEntities():Boolean =
    {
        suckEntities(sbounds(side))
    }

    override def onEntityCollision(ent:Entity)
    {
        if (!world.isRemote && !powered && itemStorage.isEmpty)
            suckEntities(ibounds(side))
    }

    def suckEntities(box:Cuboid6):Boolean =
    {
        if (!canSuckEntities) return false

        val elist = world.getEntitiesWithinAABB(classOf[EntityItem],
            box.copy.add(new Vector3(x, y, z)).aabb)
        var added = false
        for (ei <- elist) if (!ei.isDead && ei.getEntityItem.getCount > 0 && canImport(ItemKey.get(ei.getEntityItem)))
        {
            itemStorage.add(ei.getEntityItem)
            world.removeEntity(ei)
            added = true
        }
        if (added)
        {
            active = true
            sendStateUpdate()
            scheduleTick(4)
            exportBuffer()
        }
        added
    }

    def canSuckEntities:Boolean =
    {
        val bc = getPos.offset(EnumFacing.VALUES(side^1))
        val s = world.getBlockState(bc)
        world.isAirBlock(bc) || !s.getBlock
                .isSideSolid(s, world, bc, EnumFacing.VALUES(side))
    }

    def canImport(key:ItemKey) = true

    override def getConnectionMask(side:Int) = if ((side^1) == this.side) 0 else 0x1F
    override def weakPowerLevel(side:Int, mask:Int) = 0
}

object TileItemImporter
{
    val cbounds = createSided(new Cuboid6(0, 0, 0, 1, 0.99, 1))
    val ibounds = createSided(new Cuboid6(0.25, 0.99, 0.25, 0.75, 1.1, 0.75))
    val sbounds = createSided(new Cuboid6(-1, 0.99, -1, 2, 2, 2))

    private def createSided(box:Cuboid6) =
    {
        val b = new Array[Cuboid6](6)
        b(0) = box
        for (s <- 1 until 6)
            b(s) = b(0).copy.apply(Rotation.sideRotations(s).at(Vector3.center))
        b
    }
}

object RenderItemImporter extends SimpleBlockRenderer
{
    import java.lang.{Boolean => JBool, Integer => JInt}

    import org.apache.commons.lang3.tuple.Triple
    import mrtjp.projectred.expansion.BlockProperties._

    var bottom:TextureAtlasSprite = _
    var side1:TextureAtlasSprite = _
    var top1:TextureAtlasSprite = _
    var side2:TextureAtlasSprite = _
    var top2:TextureAtlasSprite = _

    var iconT1:UVTransformation = _
    var iconT2:UVTransformation = _

    override def handleState(state: IExtendedBlockState, world: IBlockAccess, pos: BlockPos): IExtendedBlockState = world.getTileEntity(pos) match {
        case t: TActiveDevice => {
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
        Triple.of(side, rotation, if (active || powered) iconT2 else iconT1)
    }

    override def getItemTransforms(stack: ItemStack) = Triple.of(0, 0, iconT1)

    override def shouldCull() = false

    override def registerIcons(reg:TextureMap)
    {
        bottom = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/importer/bottom"))
        top1 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/importer/top1"))
        side1 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/importer/side1"))
        top2 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/importer/top2"))
        side2 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/importer/side2"))

        iconT1 = new MultiIconTransformation(bottom, top1, side1, side1, side1, side1)
        iconT2 = new MultiIconTransformation(bottom, top2, side2, side2, side2, side2)
    }
}
