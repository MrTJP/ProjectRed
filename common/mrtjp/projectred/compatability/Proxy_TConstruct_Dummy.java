package mrtjp.projectred.compatability;

import mrtjp.projectred.compatability.Services.ITConstructProxy;
import net.minecraft.item.ItemStack;
import net.minecraftforge.fluids.FluidStack;


public class Proxy_TConstruct_Dummy implements ITConstructProxy {
	public Proxy_TConstruct_Dummy() {
		System.out.println("[ProjectRedCompat] Loaded TConstruct Dummy Proxy");
	}

	@Override
	public void addSmeltingRecipe(ItemStack input, int blockID, int metadata, int temperature, FluidStack liquid) {}

	@Override
	public void addAlloyMixing(FluidStack result, FluidStack... mixers) {}

	@Override
	public void addTableCastingRecipe(ItemStack output, FluidStack metal, ItemStack cast, boolean consume, int delay) {}
	
	@Override
	public void addBasinCastingRecipe(ItemStack output, FluidStack metal, ItemStack cast, boolean consume, int delay) {}

	@Override
	public ItemStack getCastIngot() {return null;}
	
	@Override
	public void loadTCInteractions() {}



}
