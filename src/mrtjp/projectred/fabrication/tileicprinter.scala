/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.render.CCModel
import codechicken.lib.vec.{Cuboid6, Translation}
import mrtjp.core.block.TInstancedBlockRender
import net.minecraft.client.renderer.RenderBlocks
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.client.renderer.tileentity.TileEntitySpecialRenderer
import net.minecraft.tileentity.TileEntity
import net.minecraft.util.ResourceLocation
import net.minecraft.world.IBlockAccess

import scala.collection.JavaConversions._

class TileICPrinter extends TileICMachine
{

}

object RenderICPrinter extends TInstancedBlockRender
{
    val models =
    {
        val map = CCModel.parseObjModels(new ResourceLocation("projectred:textures/obj/circuits/printer.obj")).toMap
        map.values.foreach
        { m =>
            m.apply(new Translation(8/16D, 10/16D, 8/16D))
            m.computeNormals()
            m.shrinkUVs(0.0005)
        }
        map
    }

    val lowerBox =
    {
        val m = CCModel.quadModel(24)
        m.generateBlock(0, new Cuboid6(0, 0, 0, 1, 10/16D, 1))
        m.computeNormals()
        m.shrinkUVs(0.0005)
        m
    }

    override def renderWorldBlock(r:RenderBlocks, w:IBlockAccess, x:Int, y:Int, z:Int, meta:Int) = ???

    override def getIcon(side:Int, meta:Int) = ???

    override def renderInvBlock(r:RenderBlocks, meta:Int) = ???

    override def registerIcons(reg:IIconRegister) = ???
}

object RenderICPrinterDynamic extends TileEntitySpecialRenderer
{
    var progress = 0.0

    override def renderTileEntityAt(tile:TileEntity, x:Double, y:Double, z:Double, frame:Float)
    {
        val ptile = tile.asInstanceOf[TileICPrinter]
        val baseT = ptile.rotationT `with` new Translation(x+0.5, y+10/16D, z+0.5)
        RenderICPrinter.models("")
    }
}