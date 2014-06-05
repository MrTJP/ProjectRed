package mrtjp.projectred.core.libmc

import net.minecraft.util.ResourceLocation
import net.minecraft.client.Minecraft
import net.minecraft.client.audio.PositionedSoundRecord

object ResourceLib
{
    val guiSlot = registerPR("textures/gui/slot.png")
    val guiExtras = registerPR("textures/gui/guiextras.png")
    val guiTex = registerPR("textures/gui/widgets.png")
    val guiBag = registerPR("textures/gui/bpgui.png")
    val guiChipContainer = registerPR("textures/gui/chipcontainer.png")
    val guiChipUpgrade = registerPR("textures/gui/chipupgradecontainer.png")
    val guiPipeInterface = registerPR("textures/gui/guiinterfacepipe.png")
    val guiPipeCrafting = registerPR("textures/gui/guicraftingpipe.png")

    val soundButton = register("gui.button.press")

    def register(path:String) = new RLUtil(new ResourceLocation(path))
    def registerPR(path:String) = new RLUtil(new ResourceLocation("projectred", path))
}

class RLUtil(loc:ResourceLocation)
{
    def mc = Minecraft.getMinecraft

    def bind(){mc.renderEngine.bindTexture(loc)}

    def play(){play(1.0F)}
    def play(volume:Float){mc.getSoundHandler.playSound(PositionedSoundRecord.func_147674_a(loc, volume))}
}