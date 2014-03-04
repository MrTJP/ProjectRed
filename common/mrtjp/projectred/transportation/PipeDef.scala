package mrtjp.projectred.transportation

import mrtjp.projectred.core.utils.{LiteEnumVal, LiteEnumCollector}
import net.minecraft.util.Icon
import cpw.mods.fml.relauncher.{SideOnly, Side}
import net.minecraft.client.renderer.texture.IconRegister
import net.minecraft.item.ItemStack
import mrtjp.projectred.ProjectRedTransportation
import scala.collection.mutable
import mrtjp.projectred.transmission.WireDef

object PipeDef extends LiteEnumCollector
{
    val BASIC = PipeDef("pr_ptube", "basic")
    val ROUTEDJUNCTION = PipeDef("pr_rbasic", "routedjunc", "routed", "unrouted", "routedconn", "unroutedconn")
    val ROUTEDINTERFACE = PipeDef("pr_rinterface", "routedint")
    val ROUTEDCRAFTING = PipeDef("pr_rcrafting", "routedcrafting")
    val ROUTEDREQUEST = PipeDef("pr_rrequest", "routedrequest")
    val ROUTEDEXTENSION = PipeDef("pr_rextension", "routedextension")

    def VALID_PIPE =
    {
        val build = new mutable.ArrayBuilder.ofRef[PipeDef]
        for (i <- values) build += i.asInstanceOf[PipeDef]
        build.result()
    }

    def apply(name:String, tex:String*) = new PipeDef(name, tex)
}

class PipeDef(val partname:String, val textures:Seq[String]) extends LiteEnumVal
{
    override def getCollector = PipeDef

    val meta = ordinal

    val sprites = new Array[Icon](textures.length)

    @SideOnly(Side.CLIENT)
    def loadTextures(reg:IconRegister)
    {
        if (textures.length > 0) for (i <- 0 until textures.length)
            sprites(i) = reg.registerIcon("projectred:pipes/"+textures(i))
    }

    def getItemStack:ItemStack = getItemStack(1)

    def getItemStack(size:Int) = new ItemStack(ProjectRedTransportation.itemPartPipe, size, meta)
}