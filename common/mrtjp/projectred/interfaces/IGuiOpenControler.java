package mrtjp.projectred.interfaces;

import net.minecraft.entity.player.EntityPlayer;

public interface IGuiOpenControler {
	public void onGuiOpenedBy(EntityPlayer player);
	
	public void onGuiClosedBy(EntityPlayer player);
}
