package mrtjp.projectred.crafting;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import scala.Array;

import mrtjp.projectred.utils.BasicUtils;
import net.minecraft.item.ItemStack;

public class AlloySmelterRecipe {

	private final ItemStack[] _matrix;
	private final ItemStack _result;
	public final ICraftingHandler _handler;
	private final int _burnTime;

	private static final ArrayList<AlloySmelterRecipe> alloyRecipes = new ArrayList<AlloySmelterRecipe>();

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

	public ItemStack getResult() {
		return _result.copy();
	}

	public ItemStack[] getMatrix() {
		return _matrix.clone();
	}

	public int getBurnTime() {
		return _burnTime;
	}

	/**
	 * Pass in array of 9 itemstacks, and this checks if it matches the current
	 * recipe.
	 * 
	 * @param inv
	 * @return
	 */
	public boolean calculateMatch(ItemStack[] inv) {
		if (inv.length != 9) {
			return false;
		}
		for (ItemStack ingredient : _matrix) {
			int missing = ingredient.stackSize;
			for (ItemStack itemInGrid : inv) {
				if (BasicUtils.areStacksTheSame(ingredient, itemInGrid)) {
					missing = missing - itemInGrid.stackSize;
				}
				if (missing <= 0) {
					break;
				}
			}
			if (missing > 0) {
				return false;
			}
		}
		return true;
	}

	public AlloySmelterRecipe copy() {
		return new AlloySmelterRecipe(_matrix, _result, _handler, _burnTime);
	}
}
