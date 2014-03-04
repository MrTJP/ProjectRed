package mrtjp.projectred.transmission

import cpw.mods.fml.relauncher.{SideOnly, Side}
import mrtjp.projectred.ProjectRedTransmission
import mrtjp.projectred.core.utils.{LiteEnumVal, LiteEnumCollector}
import net.minecraft.client.renderer.texture.IconRegister
import net.minecraft.item.ItemStack
import net.minecraft.util.Icon
import net.minecraftforge.oredict.OreDictionary
import scala.collection.mutable

object WireDef extends LiteEnumCollector
{
    val RED_ALLOY = WireDef("pr_redwire", "pr_fredwire", 0, 0xC80000, "redalloy")

    val INSULATED_0 = WireDef("pr_insulated", "pr_finsulated", 1, 0xFFFFFF, "insulated/whiteoff", "insulated/whiteon")
    val INSULATED_1 = WireDef("pr_insulated", "pr_finsulated", 1, 0xFFFFFF, "insulated/orangeoff", "insulated/orangeon")
    val INSULATED_2 = WireDef("pr_insulated", "pr_finsulated", 1, 0xFFFFFF, "insulated/magentaoff", "insulated/magentaon")
    val INSULATED_3 = WireDef("pr_insulated", "pr_finsulated", 1, 0xFFFFFF, "insulated/lightblueoff", "insulated/lightblueon")
    val INSULATED_4 = WireDef("pr_insulated", "pr_finsulated", 1, 0xFFFFFF, "insulated/yellowoff", "insulated/yellowon")
    val INSULATED_5 = WireDef("pr_insulated", "pr_finsulated", 1, 0xFFFFFF, "insulated/limeoff", "insulated/limeon")
    val INSULATED_6 = WireDef("pr_insulated", "pr_finsulated", 1, 0xFFFFFF, "insulated/pinkoff", "insulated/pinkon")
    val INSULATED_7 = WireDef("pr_insulated", "pr_finsulated", 1, 0xFFFFFF, "insulated/greyoff", "insulated/greyon")
    val INSULATED_8 = WireDef("pr_insulated", "pr_finsulated", 1, 0xFFFFFF, "insulated/lightgreyoff", "insulated/lightgreyon")
    val INSULATED_9 = WireDef("pr_insulated", "pr_finsulated", 1, 0xFFFFFF, "insulated/cyanoff", "insulated/cyanon")
    val INSULATED_10 = WireDef("pr_insulated", "pr_finsulated", 1, 0xFFFFFF, "insulated/purpleoff", "insulated/purpleon")
    val INSULATED_11 = WireDef("pr_insulated", "pr_finsulated", 1, 0xFFFFFF, "insulated/blueoff", "insulated/blueon")
    val INSULATED_12 = WireDef("pr_insulated", "pr_finsulated", 1, 0xFFFFFF, "insulated/brownoff", "insulated/brownon")
    val INSULATED_13 = WireDef("pr_insulated", "pr_finsulated", 1, 0xFFFFFF, "insulated/greenoff", "insulated/greenon")
    val INSULATED_14 = WireDef("pr_insulated", "pr_finsulated", 1, 0xFFFFFF, "insulated/redoff", "insulated/redon")
    val INSULATED_15 = WireDef("pr_insulated", "pr_finsulated", 1, 0xFFFFFF, "insulated/blackoff", "insulated/blackon")

    val BUNDLED_N = WireDef("pr_bundled", "pr_fbundled", 2, 0xFFFFFF, "bundled/neutral")
    val BUNDLED_0 = WireDef("pr_bundled", null, 2, 0xFFFFFF, "bundled/white")
    val BUNDLED_1 = WireDef("pr_bundled", null, 2, 0xFFFFFF, "bundled/orange")
    val BUNDLED_2 = WireDef("pr_bundled", null, 2, 0xFFFFFF, "bundled/magenta")
    val BUNDLED_3 = WireDef("pr_bundled", null, 2, 0xFFFFFF, "bundled/lightblue")
    val BUNDLED_4 = WireDef("pr_bundled", null, 2, 0xFFFFFF, "bundled/yellow")
    val BUNDLED_5 = WireDef("pr_bundled", null, 2, 0xFFFFFF, "bundled/lime")
    val BUNDLED_6 = WireDef("pr_bundled", null, 2, 0xFFFFFF, "bundled/pink")
    val BUNDLED_7 = WireDef("pr_bundled", null, 2, 0xFFFFFF, "bundled/grey")
    val BUNDLED_8 = WireDef("pr_bundled", null, 2, 0xFFFFFF, "bundled/lightgrey")
    val BUNDLED_9 = WireDef("pr_bundled", null, 2, 0xFFFFFF, "bundled/cyan")
    val BUNDLED_10 = WireDef("pr_bundled", null, 2, 0xFFFFFF, "bundled/purple")
    val BUNDLED_11 = WireDef("pr_bundled", null, 2, 0xFFFFFF, "bundled/blue")
    val BUNDLED_12 = WireDef("pr_bundled", null, 2, 0xFFFFFF, "bundled/brown")
    val BUNDLED_13 = WireDef("pr_bundled", null, 2, 0xFFFFFF, "bundled/green")
    val BUNDLED_14 = WireDef("pr_bundled", null, 2, 0xFFFFFF, "bundled/red")
    val BUNDLED_15 = WireDef("pr_bundled", null, 2, 0xFFFFFF, "bundled/black")

    val POWER_100v = WireDef("pr_100v", "pr_f100v", 1, 0xFFFFFF, "power/100v")

    def VALID_WIRE =
    {
        val build = new mutable.ArrayBuilder.ofRef[WireDef]
        for (i <- values) build += i.asInstanceOf[WireDef]
        build.result()
    }

    val INSULATED_WIRE =
    {
        val build = new mutable.ArrayBuilder.ofRef[WireDef]
        for (i <- INSULATED_0.meta to INSULATED_15.meta) build += VALID_WIRE(i)
        build.result()
    }

    val BUNDLED_WIRE =
    {
        val build = new mutable.ArrayBuilder.ofRef[WireDef]
        for (i <- BUNDLED_N.meta to BUNDLED_15.meta) build += VALID_WIRE(i)
        build.result()
    }

    val oreDictDefinitionInsulated = "projredInsulatedWire"
    val oreDictDefinitionInsFramed = "projredInsFramedWire"
    val oreDictDefinitionBundled = "projredBundledCable"

    def initOreDict()
    {
        for (w <- INSULATED_WIRE)
        {
            if (w.hasFramedForm) OreDictionary.registerOre(oreDictDefinitionInsFramed, w.getFramedItemStack)
            OreDictionary.registerOre(oreDictDefinitionInsulated, w.getItemStack)
        }
        for (w <- BUNDLED_WIRE) OreDictionary.registerOre(oreDictDefinitionBundled, w.getItemStack)
    }

    def apply(wireType:String, framedType:String, thickness:Int, itemColour:Int, textures:String*) =
        new WireDef(wireType, framedType, thickness, itemColour, textures)
}

class WireDef(val wireType:String, val framedType:String, val thickness:Int, val itemColour:Int, textures:Seq[String]) extends LiteEnumVal
{
    def hasFramedForm = framedType != null

    val meta = ordinal

    val wireSprites = new Array[Icon](textures.length)

    @SideOnly(Side.CLIENT)
    def loadTextures(reg: IconRegister)
    {
        for (i <- 0 until textures.length)
            wireSprites(i) = reg.registerIcon("projectred:wires/" + textures(i))
    }

    def getItemStack:ItemStack = getItemStack(1)
    def getItemStack(i:Int) = new ItemStack(ProjectRedTransmission.itemPartWire, i, meta)

    def getFramedItemStack:ItemStack = getFramedItemStack(1)
    def getFramedItemStack(i:Int) =
        if (hasFramedForm) new ItemStack(ProjectRedTransmission.itemPartFramedWire, i, meta) else null

    override def getCollector = WireDef
}