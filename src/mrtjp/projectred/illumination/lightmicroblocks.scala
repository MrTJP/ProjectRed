package mrtjp.projectred.illumination

import java.util

import codechicken.lib.vec.Rotation._
import codechicken.lib.vec.Vector3._
import codechicken.lib.vec.uv.{IconTransformation, MultiIconTransformation}
import codechicken.lib.vec.{Cuboid6, Vector3}
import codechicken.microblock._
import codechicken.multipart.TMultiPart
import mrtjp.projectred.ProjectRedIllumination
import mrtjp.projectred.core.RenderHalo
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

class LightMicroMaterial(val colour:Int) extends BlockMicroMaterial(ProjectRedIllumination.blockLamp.getDefaultState) with IGeneratedMaterial
{
    override def addTraits(traits:util.BitSet, mcrFactory:MicroblockFactory, client:Boolean)
    {
        traits.set(LightMicroMaterial.traitID)
    }

    @SideOnly(Side.CLIENT) override
    def loadIcons()
    {
        icont = new MultiIconTransformation(LampRenderer.iconsOn(colour))
        pIconT = new IconTransformation(LampRenderer.iconsOn(colour))
    }
}

object LightMicroMaterial
{
    var traitID:Int = _

    def register()
    {
        traitID = MicroblockGenerator.registerTrait(classOf[LightMicroblock])

        for (i <- 0 until 16)
            MicroMaterialRegistry.registerMaterial(
                new LightMicroMaterial(i),
                BlockMicroMaterial.materialKey(ProjectRedIllumination.blockLamp.getDefaultState)+"[colour:"+i+"]"
            )
    }
}

trait LightMicroblock extends Microblock
{
    @SideOnly(Side.CLIENT)
    override def renderDynamic(pos:Vector3, pass:Int, frame:Float)
    {
        if (pass == 0) {
            val boxes = this match {
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

            val colour = getIMaterial.asInstanceOf[LightMicroMaterial].colour

            for (box <- boxes) RenderHalo.addLight(x, y, z, colour, box)
        }
    }

    override def getLightValue =
    {
        val lightVolume = tile.partList.collect { case p:LightMicroblock => p }.map { light =>
            val b = light.getBounds
            math.abs(b.max.x-b.min.x)*math.abs(b.max.y-b.min.y)*math.abs(b.max.z-b.min.z)
        }.sum

        math.min(15, 10+5*lightVolume*8).toInt
    }
}