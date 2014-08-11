package mrtjp.projectred.compatibility.treecapitator

import mrtjp.projectred.compatibility.IPRPlugin
import net.minecraft.nbt.{NBTTagList, NBTTagCompound}
import mrtjp.projectred.ProjectRedExploration
import cpw.mods.fml.common.event.FMLInterModComms
import net.minecraft.item.Item
import net.minecraft.block.Block

object PluginTreecapitator extends IPRPlugin
{
    override def getModIDs = Array("TreeCapitator")

    override def preInit() {}

    override def init()
    {
        val tpModCfg = new NBTTagCompound
        tpModCfg.setString("modID", "ProjRed|Exploration")
        tpModCfg.setString("axeIDList", "%d; %d; %d".format(Item.getIdFromItem(ProjectRedExploration.itemPeridotAxe),
                Item.getIdFromItem(ProjectRedExploration.itemRubyAxe), Item.getIdFromItem(ProjectRedExploration.itemSapphireAxe)))
        tpModCfg.setString("shearsIDList", "")
        tpModCfg.setBoolean("useShiftedItemID", false)

        /* TODO Re enable when ProjectRedExploration.blockStainedLeaf is ready

        val treeList = new NBTTagList

        // Vanilla Oak additions
        var tree:NBTTagCompound = new NBTTagCompound
        tree.setString("treeName", "dyed_oak")
        tree.setString("logConfigKeys", "17,0")
        tree.setString("leafConfigKeys", "%d".format(Block.getIdFromBlock(ProjectRedExploration.blockStainedLeaf)))
        tree.setBoolean("requireLeafDecayCheck", false)
        treeList.appendTag(tree)


        // Vanilla Oak additions
        tree = new NBTTagCompound
        tree.setString("treeName", "dyed_spruce")
        tree.setString("logConfigKeys", "17,1")
        tree.setString("leafConfigKeys", "%d".format(Block.getIdFromBlock(ProjectRedExploration.blockStainedLeaf)))
        tree.setBoolean("requireLeafDecayCheck", false)
        treeList.appendTag(tree)


        // Vanilla Oak additions
        tree = new NBTTagCompound
        tree.setString("treeName", "dyed_birch")
        tree.setString("logConfigKeys", "17,2")
        tree.setString("leafConfigKeys", "%d".format(Block.getIdFromBlock(ProjectRedExploration.blockStainedLeaf)))
        tree.setBoolean("requireLeafDecayCheck", false)
        treeList.appendTag(tree)

        // Vanilla Oak additions
        tree = new NBTTagCompound
        tree.setString("treeName", "dyed_jungle")
        tree.setString("logConfigKeys", "17,3")
        tree.setString("leafConfigKeys", "%d".format(Block.getIdFromBlock(ProjectRedExploration.blockStainedLeaf)))
        tree.setBoolean("requireLeafDecayCheck", false)
        treeList.appendTag(tree)
        tpModCfg.setTag("trees", treeList)
        */


        FMLInterModComms.sendMessage("TreeCapitator", "ThirdPartyModConfig", tpModCfg)
    }

    override def postInit() {}

    override def desc() = "Gem axe compatibility"
}
