package tconstruct.library.crafting;

import net.minecraft.item.ItemStack;
import net.minecraftforge.fluids.FluidStack;

public class LiquidCasting {

    /** Adds a casting recipe
     * 
     * @param output Result of the cast
     * @param metal Liquid to be used in casting. This also decides how much metal is consumed
     * @param cast The empty item to cast with. ex Ingot Cast
     * @param consume Whether the item should be consumed while casting
     * @param delay Time to cast in ticks
     */
    public void addCastingRecipe(ItemStack output, FluidStack metal, ItemStack cast, boolean consume, int delay) {}

}
