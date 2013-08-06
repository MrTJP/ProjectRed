package mrtjp.projectred.expansion;

import mrtjp.projectred.core.BasicUtils;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.world.World;
import codechicken.lib.vec.BlockCoord;
import cpw.mods.fml.common.network.IGuiHandler;

public class ExpansionGuiHandler implements IGuiHandler {

	public static final int alloyID = 1;
	public static final int rotaryID = 2;
	@Override
	public Object getServerGuiElement(int ID, EntityPlayer player, World world, int x, int y, int z) {
		if (ID == alloyID) {
			TileAlloySmelter tile = (TileAlloySmelter) BasicUtils.getTileEntity(world, new BlockCoord(x, y, z), TileAlloySmelter.class);
			if (tile != null) {
				return tile.getContainer(player);
			}
		}
		if (ID == rotaryID) {
			TileTurbineRotary tile = (TileTurbineRotary) BasicUtils.getTileEntity(world, new BlockCoord(x, y, z), TileTurbineRotary.class);
			if (tile != null) {
				return tile.getContainer(player);
			}
		}
		return null;
	}

	@Override
	public Object getClientGuiElement(int ID, EntityPlayer player, World world, int x, int y, int z) {
		if (ID == alloyID) {
			TileAlloySmelter tile = (TileAlloySmelter) BasicUtils.getTileEntity(world, new BlockCoord(x, y, z), TileAlloySmelter.class);
			if (tile != null) {
				return new GuiAlloySmelter(player, tile);
			}
		}
		if (ID == rotaryID) {
			TileTurbineRotary tile = (TileTurbineRotary) BasicUtils.getTileEntity(world, new BlockCoord(x, y, z), TileTurbineRotary.class);
			if (tile != null) {
				return new GuiTurbineRotary(player, tile);
			}
		}
		return null;
	}

}
