package mrtjp.projectred.crafting;

import java.awt.Rectangle;

import mrtjp.projectred.renderstuffs.GuiAlloySmelter;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.inventory.Container;
import net.minecraft.item.ItemStack;
import codechicken.nei.NEIClientUtils;
import codechicken.nei.PositionedStack;
import codechicken.nei.forge.GuiContainerManager;
import codechicken.nei.recipe.ShapedRecipeHandler;

public class NEIAlloySmelterRecipeManager extends ShapedRecipeHandler {

	private ShapedRecipeHandler.CachedShapedRecipe getShape(AlloySmelterRecipe r) {
		ShapedRecipeHandler.CachedShapedRecipe shape = new ShapedRecipeHandler.CachedShapedRecipe(0, 0, null, r.getResult());
		for (int x = 0; x < 3; x++) {
			for (int y = 0; y < 3; y++) {
				final int slot = x * 3 + y;
				final int xPos = 44 - 5 + (y * 18);
				final int yPos = 17 - 11 + (x * 18);
				if (slot > r.getMatrix().length - 1) {
					return shape;
				}
				ItemStack ingredient = r.getMatrix()[slot];
				if (ingredient != null) {
					PositionedStack stack = new PositionedStack(ingredient, xPos, yPos);
					stack.setMaxSize(64);
					shape.ingredients.add(stack);
					shape.result.relx = 141 - 5;
					shape.result.rely = 47 - 11;
				}
			}
		}
		return shape;
	}

	@Override
	public void loadCraftingRecipes(ItemStack result) {
		for (AlloySmelterRecipe r : AlloySmelterRecipe.getAlloyRecipes()) {
			if (NEIClientUtils.areStacksSameTypeCrafting(r.getResult(), result)) {
				this.arecipes.add(getShape(r));
			}
		}
	}

	@Override
	public Class<? extends GuiContainer> getGuiClass() {
		return GuiAlloySmelter.class;
	}

	@Override
	public String getRecipeName() {
		return "Alloy Smelter";
	}

	@Override
	public String getGuiTexture() {
		return "/mods/projectred/textures/gui/alloysmelter.png";
	}

	@Override
	public boolean hasOverlay(GuiContainer gui, Container container, int recipe) {
		return false;
	}

	@Override
	public void loadUsageRecipes(ItemStack ingredient) {
		for (AlloySmelterRecipe r : AlloySmelterRecipe.getAlloyRecipes()) {
			for (ItemStack ingredientInRecipe : r.getMatrix()) {
				if (NEIClientUtils.areStacksSameTypeCrafting(ingredientInRecipe, ingredient)) {
					this.arecipes.add(getShape(r));
					break;
				}
			}
		}
	}

	@Override
	public void loadCraftingRecipes(String outputId, Object... results) {
		if (outputId.equals("alloysmelter") && getClass() == NEIAlloySmelterRecipeManager.class) {
			for (AlloySmelterRecipe r : AlloySmelterRecipe.getAlloyRecipes()) {
				this.arecipes.add(getShape(r));
			}
		} else {
			super.loadCraftingRecipes(outputId, results);
		}
	}

	@Override
	public void drawExtras(int recipe) {
		AlloySmelterRecipe r = AlloySmelterRecipe.getAlloyRecipes().get(recipe);
		if (r == null) {
			return;
		}
		drawProgressBar(126, 7, 176, 0, 14, 14, 80, 3);
		drawProgressBar(102, 28, 176, 15, 13, 30, r.getBurnTime(), 1);
		drawProgressBar(102, 28, 176, 15, 25, 30, r.getBurnTime(), 0);
		//gui.window.fontRenderer.drawString("" + r.getBurnTime(), 103, 53, 0x404040);
	}

	@Override
	public void loadTransferRects() {
		transferRects.add(new RecipeTransferRect(new Rectangle(102, 28, 25, 30), "alloysmelter"));
	}
}
