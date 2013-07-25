package mrtjp.projectred.multipart.microblocks;

import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.item.ItemStack;
import net.minecraftforge.client.IItemRenderer;

import org.lwjgl.opengl.GL11;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class MicroblockItemRenderer implements IItemRenderer {

	public static final MicroblockItemRenderer instance = new MicroblockItemRenderer();
	@Override
	public boolean handleRenderType(ItemStack item, ItemRenderType type) {
		switch (type) {
		case ENTITY:
			return true;
		case EQUIPPED:
			return true;
		case INVENTORY:
			return true;
		case FIRST_PERSON_MAP:
			return false;
		case EQUIPPED_FIRST_PERSON:
			return true;
		}
		return false;
	}

	@Override
	public boolean shouldUseRenderHelper(ItemRenderType type, ItemStack item, ItemRendererHelper helper) {
		switch (helper) {
		case ENTITY_BOBBING:
			return true;
		case ENTITY_ROTATION:
			return true;
		case BLOCK_3D:
			return true;
		case EQUIPPED_BLOCK:
			return true;
		case INVENTORY_BLOCK:
			return true;
		}
		return false;
	}

	@Override
	public void renderItem(ItemRenderType type, ItemStack item, Object... data) {
		int typen = ItemBlockMicroblock.getPartTypeID(item);
		PartType<?> pt = MicroblockLibrary.parts.get(typen);
		// System.out.println(type+", "+typen+", "+pt);
		if (pt == null)
			return;

		switch (type) {
		case INVENTORY:
		case EQUIPPED:
		case EQUIPPED_FIRST_PERSON:
			pt.renderPartInv((RenderBlocks) data[0], item);
			break;
		case ENTITY:
			GL11.glTranslatef(-0.5f, -0.5f, -0.5f);
			pt.renderPartInv((RenderBlocks) data[0], item);
			GL11.glTranslatef(0.5f, 0.5f, 0.5f);
			break;
		case FIRST_PERSON_MAP:
			break;
		default:
			break;
		}
	}

}
