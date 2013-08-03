package mrtjp.projectred.transmission;

import mrtjp.projectred.core.BasicRenderUtils;
import net.minecraft.item.ItemStack;
import net.minecraftforge.client.IItemRenderer;

import org.lwjgl.opengl.GL11;

import codechicken.lib.render.CCRenderState;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class WireItemRenderer implements IItemRenderer {

	public final static WireItemRenderer instance = new WireItemRenderer();
	private WireRenderAssistant wra = new WireRenderAssistant();


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
			renderWireInventory(damage, -.5f, 0f, -.5f, .6f);
			return;
		case EQUIPPED:
			renderWireInventory(damage, 0f, .15f, 0f, 1f);
			return;
		case EQUIPPED_FIRST_PERSON:
			renderWireInventory(damage, 1f, -.2f, -.4f, 2f);
			return;
		case INVENTORY:
			renderWireInventory(damage, 0f, .15f, 0f, 1f);
			return;
		default:
			return;
		}
	}

	public void renderWireInventory(int meta, float x, float y, float z, float scale) {
		EnumWire type = EnumWire.VALID_WIRE[meta];
		if (type == null) {
			return;
		}
		wra = new WireRenderAssistant();
		wra.x = x;
		wra.y = y;
		wra.z = z;
		wra.side = 0;
		wra.wireIcon = type.wireSprites[0];
		wra.model = type.wireMap;
		BasicRenderUtils.bindTerrainResource();
		CCRenderState.reset();
		CCRenderState.useNormals(true);
		GL11.glPushMatrix();
		GL11.glScalef(scale, scale, scale);
		CCRenderState.startDrawing(7);
		BasicRenderUtils.setFullColor();
		CCRenderState.setColourOpaque(type.itemColour);
		wra.connectsN = true;
		wra.connectsS = true;
		wra.connectsW = true;
		wra.connectsE = true;
		wra.connectsInsideConnectorN = true;
		wra.connectsInsideConnectorS = true;
		wra.connectsInsideConnectorW = true;
		wra.connectsInsideConnectorE = true;
		wra.isCenterCrossed = true;
		wra.pushRender();
		CCRenderState.draw();
		GL11.glPopMatrix();
	}
}
