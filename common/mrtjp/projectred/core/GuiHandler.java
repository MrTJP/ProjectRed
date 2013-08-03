package mrtjp.projectred.core;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.expansion.GuiAlloySmelter;
import mrtjp.projectred.expansion.GuiTurbineRotary;
import mrtjp.projectred.expansion.TileAlloySmelter;
import mrtjp.projectred.expansion.TileTurbineRotary;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import cpw.mods.fml.common.network.IGuiHandler;

public class GuiHandler implements IGuiHandler {

	@Override
	public Object getServerGuiElement(int ID, EntityPlayer player, World world, int x, int y, int z) {
		for (IProjectRedModule m : ProjectRed.initializedModules) {
			IGuiHandler g = m.getGuiHandler();
			if (g == null) {
				continue;
			}
			Object servGui = g.getServerGuiElement(ID, player, world, x, y, z);
			if (servGui != null) {
				return servGui;
			}
		}
		
		if (ID == GuiIDs.ID_Timer) {
			//GatePart tile = (GatePart) BasicUtils.getTileEntity(world, new Coords(x, y, z), GatePart.class);
			//if (tile != null) {
			//	return new ContainerTimer(player, tile);
			//}
		}
		if (ID == GuiIDs.ID_Counter) {
			//GatePart tile = (GatePart) BasicUtils.getTileEntity(world, new Coords(x, y, z), GatePart.class);
			//if (tile != null) {
			//	return new ContainerCounter(player, tile);
			//}
		}
		if (ID == GuiIDs.ID_Alloy) {
			TileAlloySmelter tile = (TileAlloySmelter) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileAlloySmelter.class);
			if (tile != null) {
				return tile.getContainer(player);
			}
		}
		if (ID == GuiIDs.ID_Bag) {
			ItemStack held = player.getHeldItem();
			if (held.itemID == ProjectRed.itemBackpack.itemID) {
				return ItemBackpack.getContainer(player);
			}
		}
		if (ID == GuiIDs.ID_TurbineRotary) {
			TileTurbineRotary tile = (TileTurbineRotary) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileTurbineRotary.class);
			if (tile != null) {
				return tile.getContainer(player);
			}
		}
		return null;
	}

	@Override
	public Object getClientGuiElement(int ID, EntityPlayer player, World world, int x, int y, int z) {
		for (IProjectRedModule m : ProjectRed.initializedModules) {
			IGuiHandler g = m.getGuiHandler();
			if (g == null) {
				continue;
			}
			Object cGui = g.getClientGuiElement(ID, player, world, x, y, z);
			if (cGui != null) {
				return cGui;
			}
		}

		if (ID == GuiIDs.ID_Timer) {
			//GatePart tile = (GatePart) BasicUtils.getTileEntity(world, new Coords(x, y, z), GatePart.class);
			//if (tile != null) {
			//	return new GuiTimer(new ContainerTimer(player, tile));
			//}
		}
		if (ID == GuiIDs.ID_Counter) {
			//GatePart tile = (GatePart) BasicUtils.getTileEntity(world, new Coords(x, y, z), GatePart.class);
			//if (tile != null) {
			//	return new GuiCounter(new ContainerCounter(player, tile));
			//}
		}
		if (ID == GuiIDs.ID_Alloy) {
			TileAlloySmelter tile = (TileAlloySmelter) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileAlloySmelter.class);
			if (tile != null) {
				return new GuiAlloySmelter(player, tile);
			}
		}
		if (ID == GuiIDs.ID_Bag) {
			ItemStack held = player.getHeldItem();
			if (held.itemID == ProjectRed.itemBackpack.itemID) {
				return new GuiBackpack(player, ItemBackpack.getBackpackInventory(player), held);
			}
		}
		if (ID == GuiIDs.ID_TurbineRotary) {
			TileTurbineRotary tile = (TileTurbineRotary) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileTurbineRotary.class);
			if (tile != null) {
				return new GuiTurbineRotary(player, tile);
			}
		}
		
		return null;
	}
}
