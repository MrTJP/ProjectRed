package mrtjp.projectred.crafting;

import net.minecraft.item.ItemStack;

public class AlloySmelterRecipe {

	public final ItemStack[] _matrix;

	public final ItemStack _result;

	public final ICraftingHandler _handler;

	public final int _burnTime;

	public abstract class ICraftingHandler {
		/**
		 * This is called with the result after an item has been crafted.
		 * 
		 * @param stack
		 *            The crafting result.
		 */
		public abstract void onItemCrafted(ItemStack stack);
	}

	public AlloySmelterRecipe(ItemStack[] matrix, ItemStack result, ICraftingHandler handler) {
		this(matrix, result, handler, 200);
	}

	public AlloySmelterRecipe(ItemStack[] matrix, ItemStack result, ICraftingHandler handler, int burnTime) {
		_matrix = matrix;
		_result = result;
		if (handler == null) {
			_handler = new ICraftingHandler() {
				@Override
				public void onItemCrafted(ItemStack stack) {
				}
			};
		} else {
			_handler = handler;
		}
		_burnTime = burnTime;
	}

	public AlloySmelterRecipe(ItemStack[] matrix, ItemStack result) {
		this(matrix, result, null, 200);
	}

	public AlloySmelterRecipe(ItemStack[] matrix, ItemStack result, int burnTime) {
		this(matrix, result, null, burnTime);
	}

}
