package mrtjp.projectred.compatability;

import net.minecraft.item.ItemStack;
import net.minecraftforge.fluids.FluidStack;
import cpw.mods.fml.common.Loader;

public class Services {
	public static interface ITConstructProxy {
		public void addSmeltingRecipe(ItemStack input, int blockID, int metadata, int temperature, FluidStack liquid);
		public void addAlloyMixing(FluidStack result, FluidStack... mixers);
		public void addTableCastingRecipe(ItemStack output, FluidStack metal, ItemStack cast, boolean consume, int delay);
		public void addBasinCastingRecipe(ItemStack output, FluidStack metal, ItemStack cast, boolean consume, int delay);
		public ItemStack getCastIngot();
		public void loadTCInteractions();
	}
	
	public static void loadServices() {
		if (Loader.isModLoaded("TConstruct")||false)
			tcProxy = new Proxy_TConstruct();
		else
			tcProxy = new Proxy_TConstruct_Dummy();
	}
	
	private static ITConstructProxy tcProxy;
	public static ITConstructProxy getTCProxy(){return tcProxy;}
}
