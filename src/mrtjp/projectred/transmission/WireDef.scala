package mrtjp.projectred.transmission

import mrtjp.core.item.ItemDefinition
import mrtjp.projectred.ProjectRedTransmission
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.item.ItemStack
import net.minecraft.util.ResourceLocation
import net.minecraftforge.fml.relauncher.{Side, SideOnly}
import net.minecraftforge.oredict.OreDictionary

object WireDef extends ItemDefinition
{
    override type EnumVal = WireDef
    override def getItem = ProjectRedTransmission.itemPartWire

    val typeRedAlloy = new ResourceLocation("projectred-transmission:wireRedAlloy")
    val typeInsulated = new ResourceLocation("projectred-transmission:wireInsulated")
    val typeBundled = new ResourceLocation("projectred-transmission:wireBundled")
    val typeFramedRedAlloy = new ResourceLocation("projectred-transmission:framedWireRedAlloy")
    val typeFramedInsulated = new ResourceLocation("projectred-transmission:framedWireInsulated")
    val typeFramedBundled = new ResourceLocation("projectred-transmission:framedWireBundled")

    val typeLowLoad = new ResourceLocation("projectred-transmission:low_power")
    val typeFramedLowLoad = new ResourceLocation("projectred-transmission:framed_low_power")

    val RED_ALLOY = WireDef(typeRedAlloy, typeFramedRedAlloy, 0, 0xC80000, "redalloy")

    val INSULATED_0  = WireDef(typeInsulated, typeFramedInsulated, 1, 0xFFFFFF, "insulated/whiteoff", "insulated/whiteon")
    val INSULATED_1  = WireDef(typeInsulated, typeFramedInsulated, 1, 0xFFFFFF, "insulated/orangeoff", "insulated/orangeon")
    val INSULATED_2  = WireDef(typeInsulated, typeFramedInsulated, 1, 0xFFFFFF, "insulated/magentaoff", "insulated/magentaon")
    val INSULATED_3  = WireDef(typeInsulated, typeFramedInsulated, 1, 0xFFFFFF, "insulated/lightblueoff", "insulated/lightblueon")
    val INSULATED_4  = WireDef(typeInsulated, typeFramedInsulated, 1, 0xFFFFFF, "insulated/yellowoff", "insulated/yellowon")
    val INSULATED_5  = WireDef(typeInsulated, typeFramedInsulated, 1, 0xFFFFFF, "insulated/limeoff", "insulated/limeon")
    val INSULATED_6  = WireDef(typeInsulated, typeFramedInsulated, 1, 0xFFFFFF, "insulated/pinkoff", "insulated/pinkon")
    val INSULATED_7  = WireDef(typeInsulated, typeFramedInsulated, 1, 0xFFFFFF, "insulated/greyoff", "insulated/greyon")
    val INSULATED_8  = WireDef(typeInsulated, typeFramedInsulated, 1, 0xFFFFFF, "insulated/lightgreyoff", "insulated/lightgreyon")
    val INSULATED_9  = WireDef(typeInsulated, typeFramedInsulated, 1, 0xFFFFFF, "insulated/cyanoff", "insulated/cyanon")
    val INSULATED_10 = WireDef(typeInsulated, typeFramedInsulated, 1, 0xFFFFFF, "insulated/purpleoff", "insulated/purpleon")
    val INSULATED_11 = WireDef(typeInsulated, typeFramedInsulated, 1, 0xFFFFFF, "insulated/blueoff", "insulated/blueon")
    val INSULATED_12 = WireDef(typeInsulated, typeFramedInsulated, 1, 0xFFFFFF, "insulated/brownoff", "insulated/brownon")
    val INSULATED_13 = WireDef(typeInsulated, typeFramedInsulated, 1, 0xFFFFFF, "insulated/greenoff", "insulated/greenon")
    val INSULATED_14 = WireDef(typeInsulated, typeFramedInsulated, 1, 0xFFFFFF, "insulated/redoff", "insulated/redon")
    val INSULATED_15 = WireDef(typeInsulated, typeFramedInsulated, 1, 0xFFFFFF, "insulated/blackoff", "insulated/blackon")

    val BUNDLED_N  = WireDef(typeBundled, typeFramedBundled, 2, 0xFFFFFF, "bundled/neutral")
    val BUNDLED_0  = WireDef(typeBundled, null, 2, 0xFFFFFF, "bundled/white")
    val BUNDLED_1  = WireDef(typeBundled, null, 2, 0xFFFFFF, "bundled/orange")
    val BUNDLED_2  = WireDef(typeBundled, null, 2, 0xFFFFFF, "bundled/magenta")
    val BUNDLED_3  = WireDef(typeBundled, null, 2, 0xFFFFFF, "bundled/lightblue")
    val BUNDLED_4  = WireDef(typeBundled, null, 2, 0xFFFFFF, "bundled/yellow")
    val BUNDLED_5  = WireDef(typeBundled, null, 2, 0xFFFFFF, "bundled/lime")
    val BUNDLED_6  = WireDef(typeBundled, null, 2, 0xFFFFFF, "bundled/pink")
    val BUNDLED_7  = WireDef(typeBundled, null, 2, 0xFFFFFF, "bundled/grey")
    val BUNDLED_8  = WireDef(typeBundled, null, 2, 0xFFFFFF, "bundled/lightgrey")
    val BUNDLED_9  = WireDef(typeBundled, null, 2, 0xFFFFFF, "bundled/cyan")
    val BUNDLED_10 = WireDef(typeBundled, null, 2, 0xFFFFFF, "bundled/purple")
    val BUNDLED_11 = WireDef(typeBundled, null, 2, 0xFFFFFF, "bundled/blue")
    val BUNDLED_12 = WireDef(typeBundled, null, 2, 0xFFFFFF, "bundled/brown")
    val BUNDLED_13 = WireDef(typeBundled, null, 2, 0xFFFFFF, "bundled/green")
    val BUNDLED_14 = WireDef(typeBundled, null, 2, 0xFFFFFF, "bundled/red")
    val BUNDLED_15 = WireDef(typeBundled, null, 2, 0xFFFFFF, "bundled/black")

    val POWER_LOWLOAD = WireDef(typeLowLoad, typeFramedLowLoad, 1, 0xFFFFFF, "power/lowload")

    //Groups
    val INSULATED_WIRES = INSULATED_0 to INSULATED_15 toArray
    val BUNDLED_WIRES = BUNDLED_N to BUNDLED_15 toArray

    val oreDictDefinitionInsulated = "projredInsulatedWire"
    val oreDictDefinitionInsFramed = "projredInsFramedWire"
    val oreDictDefinitionBundled = "projredBundledCable"

    def initOreDict()
    {
        for (w <- INSULATED_WIRES) {
            if (w.hasFramedForm) OreDictionary.registerOre(oreDictDefinitionInsFramed, w.makeFramedStack)
            OreDictionary.registerOre(oreDictDefinitionInsulated, w.makeStack)
        }
        for (w <- BUNDLED_WIRES) OreDictionary.registerOre(oreDictDefinitionBundled, w.makeStack)
    }

    def apply(wireType:ResourceLocation, framedType:ResourceLocation, thickness:Int, itemColour:Int, textures:String*) =
        new WireDef(wireType, framedType, thickness, itemColour, textures)

    class WireDef(val wireType:ResourceLocation, val framedType:ResourceLocation, val thickness:Int, val itemColour:Int, textures:Seq[String]) extends ItemDef(wireType.toString)
    {
        var wireSprites:Array[TextureAtlasSprite] = _

        def hasWireForm = wireType != null
        def hasFramedForm = framedType != null

        @SideOnly(Side.CLIENT)
        def loadTextures(map:TextureMap)
        {
            wireSprites = new Array[TextureAtlasSprite](textures.length)
            for (i <- textures.indices)
                wireSprites(i) = map.registerSprite(
                    new ResourceLocation("projectred:blocks/integration/"+textures(i)))
        }

        def makeFramedStack:ItemStack = makeFramedStack(1)
        def makeFramedStack(i:Int) =
        if (hasFramedForm) new ItemStack(ProjectRedTransmission.itemPartFramedWire, i, meta) else ItemStack.EMPTY
    }
}
