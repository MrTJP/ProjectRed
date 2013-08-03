package mrtjp.projectred.expansion;

import java.util.List;

import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.core.ProjectRedTabs;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class ItemVAWT extends Item {
	
	public ItemVAWT(int id) {
		super(id);
		maxStackSize = 1;
		setMaxDamage(1280);
		setNoRepair();
		setUnlocalizedName("projectred.items.turbine.VAWT");
		setCreativeTab(ProjectRedTabs.tabExpansion);
	}

	@Override
	@SideOnly(Side.CLIENT)
	public void registerIcons(IconRegister reg) {
		itemIcon = reg.registerIcon("projectred:vawt");
	}

	public static ItemStack initNBT(ItemStack stack) {
		if (stack == null) {
			return null;
		}
		if (stack.hasTagCompound() && stack.getTagCompound().getIntArray("colors").length == 6) {
			// Undamaged NBT, dont give new one.
			return stack;
		}
		NBTTagCompound nbt = new NBTTagCompound();
		int[] colors = new int[6];
		for (int i = 0; i < colors.length; i++) {
			colors[i] = 0;
		}
		nbt.setIntArray("colors", colors);
		stack.setTagCompound(nbt);
		return stack;
	}
	
	@Override
    public void addInformation(ItemStack stack, EntityPlayer player, List list, boolean par4) {
    	initNBT(stack);
    	String[] names = {
    			"N:  ",
    			"NE: ",
    			"SE: ",
    			"S:  ",
    			"SW: ",
    			"NW: "
    	};
    	int[] colors = stack.getTagCompound().getIntArray("colors");
    	for (int i = 0; i < colors.length; i++) {
    		list.add(names[i] + PRColors.get(colors[i]).name);
    	}
    }
}
