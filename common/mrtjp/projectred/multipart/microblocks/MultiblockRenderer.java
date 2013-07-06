package mrtjp.projectred.multipart.microblocks;

import java.util.Map;

import mrtjp.projectred.multipart.BlockMultipartBase;
import mrtjp.projectred.multipart.ICoverSystem;
import mrtjp.projectred.multipart.IMultipartTile;
import mrtjp.projectred.multipart.PartCoordinates;
import mrtjp.projectred.renderstuffs.RenderIDs;
import mrtjp.projectred.utils.BasicUtils;
import mrtjp.projectred.utils.Coords;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.world.IBlockAccess;
import cpw.mods.fml.client.registry.ISimpleBlockRenderingHandler;

public class MultiblockRenderer implements ISimpleBlockRenderingHandler {

	public static final MultiblockRenderer instance = new MultiblockRenderer();
	
	@Override
	public void renderInventoryBlock(Block block, int meta, int model, RenderBlocks render) {
		if (block instanceof BlockMultipartBase)
			((BlockMultipartBase) block).renderInvBlock(render, meta);
		else
			BlockMultipartBase.renderInvBlockStatic(render, block, meta);
	}

	@Override
	public boolean renderWorldBlock(IBlockAccess world, int x, int y, int z, Block block, int modelId, RenderBlocks render) {
		IMultipartTile te = (IMultipartTile) BasicUtils.getTileEntity(world, new Coords(x, y, z), IMultipartTile.class);

		if (te == null) {
			BlockMultipartBase.renderBlockStatic(render, block, x, y, z);
			return true;
		}

		ICoverSystem ci = te.getCoverSystem();

		boolean damageLayer = render.overrideBlockTexture != null;

		if (!damageLayer) {
			te.render(render);

			if (ci != null)
				ci.render(render);
		} else {
			for (Map.Entry<EntityPlayer, PartCoordinates> breaking : BlockMultipartBase.getBreakingParts()) {
				if (!breaking.getKey().worldObj.isRemote)
					continue;

				PartCoordinates pc = breaking.getValue();
				if (pc.x == x && pc.y == y && pc.z == z) {
					if (!pc.isCoverSystemPart)
						te.renderPart(render, pc.part);
					else if (ci != null)
						ci.renderPart(render, pc.part);
				}
			}
		}
		return true;
	}

	@Override
	public boolean shouldRender3DInInventory() {
		return true;
	}

	@Override
	public int getRenderId() {
		return RenderIDs.renderIdMicroblock;
	}

}
