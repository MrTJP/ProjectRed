package mrtjp.projectred.exploration;

import mrtjp.projectred.ProjectRedExploration;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import cpw.mods.fml.common.network.IGuiHandler;

public class ExplorationGuiHandler implements IGuiHandler {
    public static final int ID_Bag = 1;
    
    @Override
    public Object getServerGuiElement(int ID, EntityPlayer player, World world, int x, int y, int z) {
        if (ID == ID_Bag) {
            ItemStack held = player.getHeldItem();
            if (held.itemID == ProjectRedExploration.itemBackpack.itemID) {
                return ItemBackpack.getContainer(player);
            }
        }
        return null;
    }

    @Override
    public Object getClientGuiElement(int ID, EntityPlayer player, World world, int x, int y, int z) {
        if (ID == ID_Bag) {
            ItemStack held = player.getHeldItem();
            if (held.itemID == ProjectRedExploration.itemBackpack.itemID) {
                return new GuiBackpack(player, ItemBackpack.getBackpackInventory(player), held);
            }
        }
        return null;
    }
}
