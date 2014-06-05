package mrtjp.projectred.transmission

import cpw.mods.fml.relauncher.{SideOnly, Side}
import mrtjp.projectred.ProjectRedTransmission
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.item.ItemStack
import net.minecraft.util.IIcon
import net.minecraftforge.oredict.OreDictionary
import mrtjp.projectred.core.ItemDefinition

object WireDef extends ItemDefinition
{
    override type EnumVal = WireDef
    override def getItem = ProjectRedTransmission.itemPartWire

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

    //Groups
    val INSULATED_WIRES = INSULATED_0 to INSULATED_15 toArray
    val BUNDLED_WIRES = BUNDLED_N to BUNDLED_15 toArray

    val oreDictDefinitionInsulated = "projredInsulatedWire"
    val oreDictDefinitionInsFramed = "projredInsFramedWire"
    val oreDictDefinitionBundled = "projredBundledCable"

    def initOreDict()
    {
        for (w <- INSULATED_WIRES)
        {
            if (w.hasFramedForm) OreDictionary.registerOre(oreDictDefinitionInsFramed, w.makeFramedStack)
            OreDictionary.registerOre(oreDictDefinitionInsulated, w.makeStack)
        }
        for (w <- BUNDLED_WIRES) OreDictionary.registerOre(oreDictDefinitionBundled, w.makeStack)
    }

    def apply(wireType:String, framedType:String, thickness:Int, itemColour:Int, textures:String*) =
        new WireDef(wireType, framedType, thickness, itemColour, textures)

    class WireDef(val wireType:String, val framedType:String, val thickness:Int, val itemColour:Int, textures:Seq[String]) extends ItemDef
    {
        def hasWireForm = wireType != null
        def hasFramedForm = framedType != null

        val wireSprites = new Array[IIcon](textures.length)

        @SideOnly(Side.CLIENT)
        def loadTextures(reg:IIconRegister)
        {
            for (i <- 0 until textures.length)
                wireSprites(i) = reg.registerIcon("projectred:wires/"+textures(i))
        }

        def makeFramedStack:ItemStack = makeFramedStack(1)
        def makeFramedStack(i:Int) =
        if (hasFramedForm) new ItemStack(ProjectRedTransmission.itemPartFramedWire, i, meta) else null
    }
}