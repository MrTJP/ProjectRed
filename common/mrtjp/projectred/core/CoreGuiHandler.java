package mrtjp.projectred.core;

import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.world.World;
import codechicken.lib.vec.BlockCoord;
import cpw.mods.fml.common.network.IGuiHandler;

public class CoreGuiHandler implements IGuiHandler {

    public static final int alloyID = 1;

    @Override
    public Object getServerGuiElement(int ID, EntityPlayer player, World world, int x, int y, int z) {
        if (ID == alloyID) {
            TileAlloySmelter tile = (TileAlloySmelter) BasicUtils.getTileEntity(world, new BlockCoord(x, y, z), TileAlloySmelter.class);
            if (tile != null)
                return tile.getContainer(player);
        }
        return null;
    }

    @Override
    public Object getClientGuiElement(int ID, EntityPlayer player, World world, int x, int y, int z) {
        if (ID == alloyID) {
            TileAlloySmelter tile = (TileAlloySmelter) BasicUtils.getTileEntity(world, new BlockCoord(x, y, z), TileAlloySmelter.class);
            if (tile != null)
                return new GuiAlloySmelter(player, tile);
        }
        return null;
    }

}
