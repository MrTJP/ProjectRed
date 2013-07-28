package mrtjp.projectred.network;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.integration.TileGate;
import mrtjp.projectred.items.ItemBackpack;
import mrtjp.projectred.multipart.wiring.gates.ContainerCounter;
import mrtjp.projectred.multipart.wiring.gates.ContainerTimer;
import mrtjp.projectred.multipart.wiring.gates.GuiCounter;
import mrtjp.projectred.multipart.wiring.gates.GuiTimer;
import mrtjp.projectred.renderstuffs.GuiAlloySmelter;
import mrtjp.projectred.renderstuffs.GuiBackpack;
import mrtjp.projectred.renderstuffs.GuiTurbineRotary;
import mrtjp.projectred.tiles.TileAlloySmelter;
import mrtjp.projectred.tiles.TileTurbineRotary;
import mrtjp.projectred.utils.BasicUtils;
import mrtjp.projectred.utils.Coords;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import cpw.mods.fml.common.network.IGuiHandler;

public class GuiHandler implements IGuiHandler {

	@Override
	public Object getServerGuiElement(int ID, EntityPlayer player, World world, int x, int y, int z) {
		if (ID == GuiIDs.ID_Timer) {
			TileGate tile = (TileGate) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileGate.class);
			if (tile != null) {
				return new ContainerTimer(player, tile);
			}
		}
		if (ID == GuiIDs.ID_Counter) {
			TileGate tile = (TileGate) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileGate.class);
			if (tile != null) {
				return new ContainerCounter(player, tile);
			}
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
		if (ID == GuiIDs.ID_Timer) {
			TileGate tile = (TileGate) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileGate.class);
			if (tile != null) {
				return new GuiTimer(new ContainerTimer(player, tile));
			}
		}
		if (ID == GuiIDs.ID_Counter) {
			TileGate tile = (TileGate) BasicUtils.getTileEntity(world, new Coords(x, y, z), TileGate.class);
			if (tile != null) {
				return new GuiCounter(new ContainerCounter(player, tile));
			}
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
