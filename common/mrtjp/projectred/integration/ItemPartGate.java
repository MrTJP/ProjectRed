package mrtjp.projectred.integration;

import java.util.List;

import mrtjp.projectred.multipart.wiring.gates.GateRenderBridge;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.JItemMultiPart;
import codechicken.multipart.TMultiPart;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class ItemPartGate extends JItemMultiPart {

	public ItemPartGate(int id) {
		super(id);
		setHasSubtypes(true);
		setUnlocalizedName("projred.itempartgate");
	}

	@Override
	public TMultiPart newPart(ItemStack item, EntityPlayer player, World world, BlockCoord pos, int side, Vector3 vhit) {
		BlockCoord onPos = pos.copy().offset(side ^ 1);
		if (!world.isBlockSolidOnSide(onPos.x, onPos.y, onPos.z, ForgeDirection.getOrientation(side))) {
			return null;
		}
		TileGate gate = new TileGate(EnumGate.get(item.getItemDamage()));
		gate.setupPlacement(player, side);
		return gate;
	}

	@Override
	@SideOnly(Side.CLIENT)
	public void getSubItems(int id, CreativeTabs tab, List list) {
		for (EnumGate g : EnumGate.VALUES) {
			list.add(g.getItemStack());
		}
	}

	public String getUnlocalizedName(ItemStack stack) {
		return super.getUnlocalizedName() + "|" + stack.getItemDamage();
	}

	@Override
	public void registerIcons(IconRegister reg) {
		GateRenderBridge.registerAllIcons(reg);

	}

	@Override
	@SideOnly(Side.CLIENT)
	public int getSpriteNumber() {
		return 0;
	}

}
