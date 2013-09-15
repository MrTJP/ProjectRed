package tconstruct.library.crafting;

import net.minecraft.item.ItemStack;
import net.minecraftforge.fluids.FluidStack;

public class Smeltery {

    /** Adds mappings between an input and its liquid.
     * Renders with the given input's block ID and metadata
     * Example: Smeltery.addMelting(Block.oreIron, 0, 600, new FluidStack(liquidMetalStill.blockID, TConstruct.ingotLiquidValue * 2, 0));
     * 
     * @param input The item to liquify
     * @param blockID The ID of the block to render
     * @param metadata The metadata of the block to render
     * @param temperature How hot the block should be before liquifying
     * @param liquid The result of the process
     */
	public static void addMelting(ItemStack input, int blockID, int metadata,int temperature, FluidStack liquid) {}

	
    /** Adds an alloy mixing recipe.
     * Example: Smeltery.addAlloyMixing(new FluidStack(bronzeID, 2, 0), new FluidStack(copperID, 3, 0), new FluidStack(tinID, 1, 0));
     * The example mixes 3 copper with 1 tin to make 2 bronze
     * 
     * @param result The output of the combination of mixers. The quantity is used for amount of a successful mix
     * @param mixers the liquids to be mixed. Quantities are used as ratios
     */
	public static void addAlloyMixing(FluidStack result, FluidStack... mixers) {}

}
