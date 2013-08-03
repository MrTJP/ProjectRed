package mrtjp.projectred.transmission;

import org.lwjgl.opengl.GL11;

import mrtjp.projectred.core.BasicRenderUtils;
import net.minecraft.item.ItemStack;
import net.minecraftforge.client.IItemRenderer;
import codechicken.lib.render.CCRenderState;

public class JacketedWireItemRenderer implements IItemRenderer {

	public static JacketedWireItemRenderer instance = new JacketedWireItemRenderer();
	WireRenderAssistant wra = new WireRenderAssistant();

	@Override
	public boolean handleRenderType(ItemStack item, ItemRenderType type) {
		return true;
	}

	@Override
	public boolean shouldUseRenderHelper(ItemRenderType type, ItemStack item, ItemRendererHelper helper) {
		return true;
	}

	@Override
	public void renderItem(ItemRenderType type, ItemStack item, Object... data) {
		int damage = item.getItemDamage();
		switch (type) {
		case ENTITY:
			renderWireInventory(damage, -.5f, 0f, -.5f, 1f);
			return;
		case EQUIPPED:
			renderWireInventory(damage, 0f, .0f, 0f, 1f);
			return;
		case EQUIPPED_FIRST_PERSON:
			renderWireInventory(damage, 1f, -.6f, -.4f, 2f);
			return;
		case INVENTORY:
			renderWireInventory(damage, 0f, -.2f, 0f, 1f);
			return;
		default:
			return;
		}
	}

	public void renderWireInventory(int damage, float x, float y, float z, float scale) {
		EnumWire type = EnumWire.VALID_WIRE[damage];
		wra.x = x;
		wra.y = y;
		wra.z = z;
		wra.side = 0;
		wra.model = type.jacketMap;
		wra.wireIcon = type.wireSprites[0];
		GL11.glPushMatrix();
		GL11.glScalef(scale, scale, scale);
		wra.setInventoryJacketRender();
		CCRenderState.startDrawing(7);
		BasicRenderUtils.setFullColor();
		BasicRenderUtils.setFullBrightness();
		wra.pushJacketFrameRender();
		CCRenderState.setColourOpaque(type.itemColour);
		wra.pushJacketWireRender();
		CCRenderState.draw();
		GL11.glPopMatrix();
	}

}
