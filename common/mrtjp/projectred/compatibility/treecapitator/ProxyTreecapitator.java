package mrtjp.projectred.compatibility.treecapitator;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.PRLogger;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import cpw.mods.fml.common.event.FMLInterModComms;

public class ProxyTreecapitator
{
    public ProxyTreecapitator()
    {
        PRLogger.info("Loaded Treecapitator Compatibility Proxy");
    }
    
    public void init()
    {
        NBTTagCompound tpModCfg = new NBTTagCompound();
        tpModCfg.setString("modID", "ProjRed|Exploration");
        tpModCfg.setString("axeIDList", String.format("%d; %d; %d",
                ProjectRedExploration.itemPeridotAxe().itemID,
                ProjectRedExploration.itemRubyAxe().itemID,
                ProjectRedExploration.itemSapphireAxe().itemID));
        tpModCfg.setString("shearsIDList", "");
        tpModCfg.setBoolean("useShiftedItemID", false);
        
        NBTTagList treeList = new NBTTagList();
        
        // Vanilla Oak additions
        NBTTagCompound tree = new NBTTagCompound();
        tree.setString("treeName", "dyed_oak");
        tree.setString("logConfigKeys", "17,0");
        tree.setString("leafConfigKeys", String.format("%d", ProjectRedExploration.blockStainedLeaf().blockID));
        tree.setBoolean("requireLeafDecayCheck", false);
        treeList.appendTag(tree);
        
        // Vanilla Oak additions
        tree = new NBTTagCompound();
        tree.setString("treeName", "dyed_spruce");
        tree.setString("logConfigKeys", "17,1");
        tree.setString("leafConfigKeys", String.format("%d", ProjectRedExploration.blockStainedLeaf().blockID));
        tree.setBoolean("requireLeafDecayCheck", false);
        treeList.appendTag(tree);
        
        // Vanilla Oak additions
        tree = new NBTTagCompound();
        tree.setString("treeName", "dyed_birch");
        tree.setString("logConfigKeys", "17,2");
        tree.setString("leafConfigKeys", String.format("%d", ProjectRedExploration.blockStainedLeaf().blockID));
        tree.setBoolean("requireLeafDecayCheck", false);
        treeList.appendTag(tree);
        
        // Vanilla Oak additions
        tree = new NBTTagCompound();
        tree.setString("treeName", "dyed_jungle");
        tree.setString("logConfigKeys", "17,3");
        tree.setString("leafConfigKeys", String.format("%d", ProjectRedExploration.blockStainedLeaf().blockID));
        tree.setBoolean("requireLeafDecayCheck", false);
        treeList.appendTag(tree);
        
        tpModCfg.setTag("trees", treeList);
        
        FMLInterModComms.sendMessage("TreeCapitator", "ThirdPartyModConfig", tpModCfg);
    }
}
