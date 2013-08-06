package mrtjp.projectred.transmission;

import java.util.List;

import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.ProjectRedTabs;
import net.minecraft.block.Block;
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
import codechicken.multipart.TileMultipart;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class ItemPartJacketedWire extends JItemMultiPart {

	public ItemPartJacketedWire(int id) {
		super(id);
		setHasSubtypes(true);
		setCreativeTab(ProjectRedTabs.tabTransmission);
		setUnlocalizedName("projred.transmission.jacwire");
	}

	@Override
	public boolean onItemUse(ItemStack stack, EntityPlayer player, World w, int x, int y, int z, int side, float hitX, float hitY, float hitZ) {
		if (super.onItemUse(stack, player, w, x, y, z, side, hitX, hitY, hitZ)) {
			w.playSoundEffect(x + 0.5, y + 0.5, z + 0.5, Block.soundGlassFootstep.getPlaceSound(), (Block.soundGlassFootstep.getVolume() * 5.0F), Block.soundGlassFootstep.getPitch() * .9F);
			return true;
		}
		return false;
	}

	@Override
	public TMultiPart newPart(ItemStack item, EntityPlayer player, World world, BlockCoord pos, int side, Vector3 vhit) {
		EnumWire w = EnumWire.VALID_WIRE[item.getItemDamage()];
		try {
			return (TMultiPart) w.teclass.getConstructors()[0].newInstance(w, true, side ^ 1);
		} catch (Throwable e) {
			return null;
		}
	}

	@Override
	@SideOnly(Side.CLIENT)
	public void getSubItems(int id, CreativeTabs tab, List list) {
		for (EnumWire w : EnumWire.VALID_WIRE) {
			list.add(w.getJacketedItemStack());
		}
	}

	public String getUnlocalizedName(ItemStack stack) {
		return super.getUnlocalizedName() + ".jac|" + stack.getItemDamage();
	}

	@Override
	public void registerIcons(IconRegister reg) {}

	@Override
	@SideOnly(Side.CLIENT)
	public int getSpriteNumber() {
		return 0;
	}

}
