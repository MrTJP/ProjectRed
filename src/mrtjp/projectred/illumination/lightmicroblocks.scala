/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.illumination

import java.util

import codechicken.lib.vec.Rotation._
import codechicken.lib.vec.Vector3._
import codechicken.lib.vec.{Cuboid6, Vector3}
import codechicken.microblock.MicroblockGenerator.IGeneratedMaterial
import codechicken.microblock._
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.projectred.ProjectRedIllumination
import mrtjp.projectred.core.RenderHalo

class LightMicroMaterial(meta:Int) extends BlockMicroMaterial(ProjectRedIllumination.blockLamp, meta) with IGeneratedMaterial
{
    override def addTraits(traits:util.BitSet, mcrClass:MicroblockClass, client:Boolean)
    {
        traits.set(LightMicroMaterial.traitID)
    }
}

object LightMicroMaterial
{
    var traitID:Int = _

    def register()
    {
        traitID = MicroblockGenerator.registerTrait(classOf[LightMicroblock])

        for (i <- 16 until 32)
            MicroMaterialRegistry.replaceMaterial(
                new LightMicroMaterial(i),
                BlockMicroMaterial.materialKey(ProjectRedIllumination.blockLamp, i)
            )
    }
}

trait LightMicroblock extends Microblock
{
    @SideOnly(Side.CLIENT)
    override def renderDynamic(pos:Vector3, frame:Float, pass:Int)
    {
        if (pass == 0)
        {
            val boxes = this match
            {
                case h:HollowMicroblock =>
                    val size = h.getHollowSize
                    val d1 = 0.5-size/32D
                    val d2 = 0.5+size/32D
                    val t = (shape>>4)/8D
                    val ex = 0.025

                    val tr = sideRotations(shape&0xF).at(center)

                    Seq(new Cuboid6(0-ex, 0-ex, 0-ex, 1+ex, t+ex, d1+ex),
                        new Cuboid6(0-ex, 0-ex, d2-ex, 1+ex, t+ex, 1+ex),
                        new Cuboid6(0-ex, 0-ex, d1+ex, d1+ex, t+ex, d2-ex),
                        new Cuboid6(d2-ex, 0-ex, d1+ex, 1+ex, t+ex, d2-ex))
                            .map(c => c.apply(tr))
                case _ =>
                    val it = getCollisionBoxes.iterator()
                    val bb = Seq.newBuilder[Cuboid6]
                    while(it.hasNext) bb += it.next()
                    bb.result().map(_.copy.expand(0.025))
            }

            val colour = getIMaterial.asInstanceOf[LightMicroMaterial].meta-16

            for (box <- boxes) RenderHalo.addLight(x, y, z, colour, box)
        }
    }

    override def getLightValue =
    {
        val totalSize = tile.partList.collect { case p:LightMicroblock => p }.map(_.getSize/8.0).sum
        math.min(15, (15*totalSize).toInt)
    }
}