/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import java.util.{List => JList}

import codechicken.lib.render.uv.{MultiIconTransformation, UVTransformation}
import codechicken.lib.vec.{Rotation, Cuboid6, Vector3}
import mrtjp.core.block.TInstancedBlockRender
import mrtjp.core.inventory.InvWrapper
import mrtjp.core.item.ItemKey
import mrtjp.core.render.TCubeMapRender
import mrtjp.core.world.WorldLib
import mrtjp.projectred.ProjectRedExpansion
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.entity.Entity
import net.minecraft.entity.item.EntityItem
import net.minecraft.item.ItemStack
import net.minecraft.util.IIcon
import net.minecraft.world.IBlockAccess
import net.minecraftforge.common.util.ForgeDirection
import TileItemImporter._
import scala.collection.JavaConversions._

class TileItemImporter extends TileMachine with TPressureActiveDevice
{
    override def getBlock = ProjectRedExpansion.machine2

    override def getCollisionBounds = TileItemImporter.cbounds(side)

    override def doesRotate = false
    override def doesOrient = true

    //side = out, side^1 = in
    override def canAcceptInput(item:ItemKey, side:Int) = (side^1) == this.side && !powered && storage.isEmpty
    override def canAcceptBacklog(item:ItemKey, side:Int) = side == this.side
    override def canConnectSide(side:Int) = (side&6) == (this.side&6)

    override def onActivate()
    {
        if (importInv() || importEntities())
            return
    }

    def extractCount = 1

    def importInv():Boolean =
    {
        val inv = InvWrapper.getInventory(world, position.offset(side^1))
        if (inv == null) return false
        val w = InvWrapper.wrap(inv)
        w.setSlotsFromSide(side)
        val list = w.getAllItemStacks
        for ((k, v) <- list)
        {
            val stack = k.makeStack(w.extractItem(k, extractCount))
            if (stack.stackSize > 0 && canImport(k))
            {
                storage.add(stack)
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
        if (!world.isRemote && !powered && storage.isEmpty)
            suckEntities(ibounds(side))
    }

    def suckEntities(box:Cuboid6):Boolean =
    {
        if (!canSuckEntities) return false

        val elist = world.getEntitiesWithinAABB(classOf[EntityItem],
            box.copy.add(new Vector3(x, y, z)).toAABB).asInstanceOf[JList[EntityItem]]
        var added = false
        for (ei <- elist) if (!ei.isDead && ei.getEntityItem.stackSize > 0 && canImport(ItemKey.get(ei.getEntityItem)))
        {
            storage.add(ei.getEntityItem)
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
        val bc = position.offset(side^1)
        world.isAirBlock(bc.x, bc.y, bc.z) || !world.getBlock(bc.x, bc.y, bc.z)
                .isSideSolid(world, bc.x, bc.y, bc.z, ForgeDirection.getOrientation(side))
    }

    def canImport(key:ItemKey) = true
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

object RenderItemImporter extends TInstancedBlockRender with TCubeMapRender
{
    var bottom:IIcon = _
    var side1:IIcon = _
    var top1:IIcon = _
    var side2:IIcon = _
    var top2:IIcon = _

    var iconT1:UVTransformation = _
    var iconT2:UVTransformation = _

    override def getData(w:IBlockAccess, x:Int, y:Int, z:Int) =
    {
        val te = WorldLib.getTileEntity(w, x, y, z, classOf[TActiveDevice])
        if (te != null) (te.side, te.rotation, if (te.active || te.powered) iconT2 else iconT1)
        else (0, 0, iconT1)
    }

    override def getInvData = (0, 0, iconT1)

    override def getIcon(s:Int, meta:Int) = s match
    {
        case 0 => bottom
        case 1 => top1
        case _ => side1
    }

    override def registerIcons(reg:IIconRegister)
    {
        bottom = reg.registerIcon("projectred:machines/importer/bottom")
        top1 = reg.registerIcon("projectred:machines/importer/top1")
        side1 = reg.registerIcon("projectred:machines/importer/side1")
        top2 = reg.registerIcon("projectred:machines/importer/top2")
        side2 = reg.registerIcon("projectred:machines/importer/side2")

        iconT1 = new MultiIconTransformation(bottom, top1, side1, side1, side1, side1)
        iconT2 = new MultiIconTransformation(bottom, top2, side2, side2, side2, side2)
    }
}