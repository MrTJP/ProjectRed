package mrtjp.projectred.compatibility.chisel

import mrtjp.projectred.compatibility.IPRPlugin
import mrtjp.projectred.core.Configurator
import mrtjp.projectred.exploration.DecorativeStoneDefs
import net.minecraft.block.Block
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.fml.common.event.FMLInterModComms

object PluginChisel extends IPRPlugin
{
    val chiselModID = "chisel"

    override def getModIDs = Array(chiselModID, "projectred-exploration")

    override def isEnabled = Configurator.compat_Chisel

    override def preInit(){}

    override def init()
    {
        ChiselExplorationIntegration.initChiselModIntegration()
    }

    override def postInit(){}

    override def desc() = "Chisel: Exploration decorative blocks"
}

private object ChiselExplorationIntegration
{
    private val IMCKeyAddVariation = "add_variation"
    private val tagKeyGroup = "group"
    private val tagKeyStack = "stack"
    private val tagKeyBlock = "block"
    private val tagKeyMeta = "meta"

    def initChiselModIntegration()
    {
        def addToGroup(group:String, stack:ItemStack, block:Block, meta:Int) {
            val message = new NBTTagCompound
            message.setString(tagKeyGroup, group)
            message.setTag(tagKeyStack, stack.serializeNBT())
            message.setString(tagKeyBlock, block.getRegistryName.toString)
            message.setInteger(tagKeyMeta, meta)
            FMLInterModComms.sendMessage(PluginChisel.chiselModID, IMCKeyAddVariation, message)
        }

        /** Add conversion group [marble, marble brick] **/
        addToGroup("marble", DecorativeStoneDefs.MARBLE.makeStack, DecorativeStoneDefs.getBlock, DecorativeStoneDefs.MARBLE.meta)
        addToGroup("marble", DecorativeStoneDefs.MARBLEBRICK.makeStack, DecorativeStoneDefs.getBlock, DecorativeStoneDefs.MARBLEBRICK.meta)

        /** Add conversion group [basalt cobble, basalt brick, basalt stone] **/
        addToGroup("basalt", DecorativeStoneDefs.BASALTCOBBLE.makeStack, DecorativeStoneDefs.getBlock, DecorativeStoneDefs.BASALTCOBBLE.meta)
        addToGroup("basalt", DecorativeStoneDefs.BASALT.makeStack, DecorativeStoneDefs.getBlock, DecorativeStoneDefs.BASALT.meta)
        addToGroup("basalt", DecorativeStoneDefs.BASALTBRICK.makeStack, DecorativeStoneDefs.getBlock, DecorativeStoneDefs.BASALTBRICK.meta)
    }
}