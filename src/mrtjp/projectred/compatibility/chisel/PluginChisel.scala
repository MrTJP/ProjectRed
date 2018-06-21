package mrtjp.projectred.compatibility.chisel

import mrtjp.projectred.compatibility.IPRPlugin
import mrtjp.projectred.core.Configurator
import mrtjp.projectred.exploration.DecorativeStoneDefs
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.fml.common.event.FMLInterModComms

object PluginChisel extends IPRPlugin
{
    override def getModIDs = Array("chisel", "projectred-exploration")

    override def isEnabled = Configurator.compat_Chisel

    override def preInit(){}

    override def init()
    {
        for (s <- DecorativeStoneDefs.values) {
            val message = new NBTTagCompound
            message.setString("group", "projectred")
            message.setTag("stack", s.makeStack.serializeNBT)
            FMLInterModComms.sendMessage("chisel", "add_variation", message)
        }
    }

    override def postInit(){}

    override def desc() = "Chisel: Exploration decorative blocks"
}