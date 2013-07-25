package mrtjp.projectred.multipart.microblocks;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.utils.Dir;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.AxisAlignedBB;
import net.minecraft.util.EnumMovingObjectType;
import net.minecraft.util.MovingObjectPosition;
import net.minecraft.util.Vec3;
import net.minecraft.world.World;
import net.minecraftforge.client.event.DrawBlockHighlightEvent;
import net.minecraftforge.common.ForgeDirection;
import net.minecraftforge.event.ForgeSubscribe;

import org.lwjgl.opengl.GL11;

public class MicroblockPlacementHighlightHandler {

	static final double SELECTOR_OUTER_SIZE = 0.25;
	// static final double SELECTOR_INNER_SIZE = (1 - SELECTOR_OUTER_SIZE*2);
	static final double SELECTOR_STRIP_OUTER_SIZE = 0.4;

	@ForgeSubscribe(receiveCanceled = true)
	public void onHighlight(DrawBlockHighlightEvent evt) {
		MovingObjectPosition pos = evt.target;
		int pass = evt.subID;
		ItemStack holding = evt.currentItem;
		EntityPlayer ply = evt.player;
		float partialTick = evt.partialTicks;

		if (pos.typeOfHit != EnumMovingObjectType.TILE)
			return;

		if (pass != 0)
			return;

		EnumPosition hitPos = null;
		AxisAlignedBB hitBB = null;

		TileEntity te = ply.worldObj.getBlockTileEntity(pos.blockX, pos.blockY, pos.blockZ);
		IMicroblockCoverSystem ci = null;
		if (te instanceof IMicroblockSupporterTile) {
			IMicroblockSupporterTile ict = (IMicroblockSupporterTile) te;
			ci = ict.getCoverSystem();
			if (pos.subHit >= 0) {
				hitPos = ict.getPartPosition(pos.subHit);
				hitBB = ict.getPartAABBFromPool(pos.subHit);

			} else if (ci != null) {
				hitPos = ci.getPartPosition(-1 - pos.subHit);
				hitBB = ci.getPartAABBFromPool(-1 - pos.subHit);
			}
		}

		if (holding != null && holding.itemID == ProjectRed.blockMicrocontainer.blockID) {
			PartType<?> type = MicroblockLibrary.parts.get(ItemBlockMicroblock.getPartTypeID(holding));
			if (type != null) {
				World world = ply.worldObj;
				boolean ok = false;
				int x = pos.blockX;
				int y = pos.blockY;
				int z = pos.blockZ;
				int dx = 0, dy = 0, dz = 0;
				switch (pos.sideHit) {
				case Dir.NX:
					if (hitPos == null || hitPos.x.touchesNegative())
						dx = -1;
					break;
				case Dir.PX:
					if (hitPos == null || hitPos.x.touchesPositive())
						dx = 1;
					break;
				case Dir.NY:
					if (hitPos == null || hitPos.y.touchesNegative())
						dy = -1;
					break;
				case Dir.PY:
					if (hitPos == null || hitPos.y.touchesPositive())
						dy = 1;
					break;
				case Dir.NZ:
					if (hitPos == null || hitPos.z.touchesNegative())
						dz = -1;
					break;
				case Dir.PZ:
					if (hitPos == null || hitPos.z.touchesPositive())
						dz = 1;
					break;
				}

				ok = canPlaceInBlock(world, x + dx, y + dy, z + dz);
				TileEntity tePlacingIn = world.getBlockTileEntity(x + dx, y + dy, z + dz);
				if (ok) {
					GL11.glDisable(GL11.GL_TEXTURE_2D);
					GL11.glDepthMask(false);
					GL11.glColor4f(0.0f, 0.0f, 0.0f, 0.4f);
					GL11.glPushMatrix();
					GL11.glTranslated(pos.blockX - ply.lastTickPosX - (ply.posX - ply.lastTickPosX) * partialTick - 0.5, pos.blockY - ply.lastTickPosY - (ply.posY - ply.lastTickPosY) * partialTick - 0.5, pos.blockZ - ply.lastTickPosZ - (ply.posZ - ply.lastTickPosZ) * partialTick - 0.5);
					GL11.glScalef(1.002f, 1.002f, 1.002f);
					GL11.glTranslated(0.5, 0.5, 0.5);

					EnumPosition placement = null;

					double inset = 0;

					if (hitPos != null) {
						switch (pos.sideHit) {
						case Dir.NX:
							inset = hitBB.minX;
							break;
						case Dir.PX:
							inset = 1 - hitBB.maxX;
							break;
						case Dir.NY:
							inset = hitBB.minY;
							break;
						case Dir.PY:
							inset = 1 - hitBB.maxY;
							break;
						case Dir.NZ:
							inset = hitBB.minZ;
							break;
						case Dir.PZ:
							inset = 1 - hitBB.maxZ;
							break;
						}
						inset -= 0.01;
					}

					EnumPartClass clazz = type.getPartClass();
					if (clazz == EnumPartClass.Panel || clazz == EnumPartClass.HollowPanel) {
						renderEdgeSelector(pos, ply, SELECTOR_OUTER_SIZE, inset);
						placement = getPanelPlacement(ply, pos, dx != 0 || dy != 0 || dz != 0 ? null : hitPos);
					} else if (clazz == EnumPartClass.Strip) {
						renderEdgeSelector(pos, ply, SELECTOR_STRIP_OUTER_SIZE, inset);
						placement = getStripPlacement(ply, pos, dx != 0 || dy != 0 || dz != 0 ? null : hitPos);
					} else if (clazz == EnumPartClass.Corner) {
						renderCornerSelector(pos, ply, inset);
						placement = getCornerPlacement(ply, pos, dx != 0 || dy != 0 || dz != 0 ? null : hitPos);
					}

					GL11.glTranslatef(dx, dy, dz);
					GL11.glEnable(GL11.GL_TEXTURE_2D);

					if (placement != null && (tePlacingIn == null || (tePlacingIn instanceof IMicroblockSupporterTile && ((MicroblockCoverSystem) ((IMicroblockSupporterTile) tePlacingIn).getCoverSystem()).canPlace(type, placement)))) {

						type.renderPreview(evt.context, placement, holding);
					}

					GL11.glPopMatrix();
					GL11.glColor4f(1.0f, 1.0f, 1.0f, 1.0f);
					GL11.glDepthMask(true);
				}
			}
		}
	}

	static EnumPosition getPanelPlacement(EntityPlayer ply, MovingObjectPosition pos, EnumPosition placingOn) {
		Vec3 hv = pos.hitVec.addVector(-pos.blockX, -pos.blockY, -pos.blockZ);
		double x = hv.xCoord - 0.5;
		double y = hv.yCoord - 0.5;
		double z = hv.zCoord - 0.5;
		final double BORDER = 0.5 - SELECTOR_OUTER_SIZE;
		EnumPosition result = null;
		switch (pos.sideHit) {
		case Dir.NX:
		case Dir.PX:
			if (y >= -BORDER && y <= BORDER && z >= -BORDER && z <= BORDER)
				result = (pos.sideHit == Dir.NX ? EnumPosition.FacePX : EnumPosition.FaceNX);
			else if (Math.abs(y) < Math.abs(z))
				result = (z < 0 ? EnumPosition.FaceNZ : EnumPosition.FacePZ);
			else
				result = (y < 0 ? EnumPosition.FaceNY : EnumPosition.FacePY);
			break;
		case Dir.NY:
		case Dir.PY:
			if (x >= -BORDER && x <= BORDER && z >= -BORDER && z <= BORDER)
				result = (pos.sideHit == Dir.NY ? EnumPosition.FacePY : EnumPosition.FaceNY);
			else if (Math.abs(z) < Math.abs(x))
				result = (x < 0 ? EnumPosition.FaceNX : EnumPosition.FacePX);
			else
				result = (z < 0 ? EnumPosition.FaceNZ : EnumPosition.FacePZ);
			break;
		case Dir.NZ:
		case Dir.PZ:
			if (x >= -BORDER && x <= BORDER && y >= -BORDER && y <= BORDER)
				result = (pos.sideHit == Dir.NZ ? EnumPosition.FacePZ : EnumPosition.FaceNZ);
			else if (Math.abs(x) < Math.abs(y))
				result = (y < 0 ? EnumPosition.FaceNY : EnumPosition.FacePY);
			else
				result = (x < 0 ? EnumPosition.FaceNX : EnumPosition.FacePX);
			break;
		}
		if (placingOn != null) {
			switch (result) {
			case FaceNX:
				if (pos.sideHit == Dir.PX && (placingOn.x == EnumAxisPosition.Negative || placingOn.x == EnumAxisPosition.Centre))
					result = EnumPosition.FacePX;
				break;
			case FaceNY:
				if (pos.sideHit == Dir.PY && (placingOn.y == EnumAxisPosition.Negative || placingOn.y == EnumAxisPosition.Centre))
					result = EnumPosition.FacePY;
				break;
			case FaceNZ:
				if (pos.sideHit == Dir.PZ && (placingOn.z == EnumAxisPosition.Negative || placingOn.z == EnumAxisPosition.Centre))
					result = EnumPosition.FacePZ;
				break;
			case FacePX:
				if (pos.sideHit == Dir.NX && (placingOn.x == EnumAxisPosition.Positive || placingOn.x == EnumAxisPosition.Centre))
					result = EnumPosition.FaceNX;
				break;
			case FacePY:
				if (pos.sideHit == Dir.NY && (placingOn.y == EnumAxisPosition.Positive || placingOn.y == EnumAxisPosition.Centre))
					result = EnumPosition.FaceNY;
				break;
			case FacePZ:
				if (pos.sideHit == Dir.NZ && (placingOn.z == EnumAxisPosition.Positive || placingOn.z == EnumAxisPosition.Centre))
					result = EnumPosition.FaceNZ;
				break;
			default:
				throw new AssertionError("can't get here");
			}
		}
		return result;
	}

	static EnumPosition getStripPlacement(EntityPlayer player, MovingObjectPosition pos, EnumPosition placingOn) {
		Vec3 hv = pos.hitVec.addVector(-pos.blockX, -pos.blockY, -pos.blockZ);
		double x = hv.xCoord - 0.5;
		double y = hv.yCoord - 0.5;
		double z = hv.zCoord - 0.5;
		final double BORDER = 0.5 - SELECTOR_STRIP_OUTER_SIZE;
		EnumPosition result = EnumPosition.EdgePXPY;
		int sideHit = pos.sideHit;
		if (placingOn != null) {
			sideHit ^= 1;
		}
		if (player.isSneaking()) {
			// If player is sneaking, place allow placement after the hit vector.
			sideHit = ForgeDirection.getOrientation(sideHit).getOpposite().ordinal();
		}
		switch (sideHit) {
		case Dir.PX:
			if (y >= -BORDER && y <= BORDER && z >= -BORDER && z <= BORDER)
				result = EnumPosition.PostX;
			else if (Math.abs(y) < Math.abs(z))
				result = (z < 0 ? EnumPosition.EdgeNXNZ : EnumPosition.EdgeNXPZ);
			else
				result = (y < 0 ? EnumPosition.EdgeNXNY : EnumPosition.EdgeNXPY);
			break;
		case Dir.NX:
			if (y >= -BORDER && y <= BORDER && z >= -BORDER && z <= BORDER)
				result = EnumPosition.PostX;
			else if (Math.abs(y) < Math.abs(z))
				result = (z < 0 ? EnumPosition.EdgePXNZ : EnumPosition.EdgePXPZ);
			else
				result = (y < 0 ? EnumPosition.EdgePXNY : EnumPosition.EdgePXPY);
			break;
		case Dir.PY:
			if (x >= -BORDER && x <= BORDER && z >= -BORDER && z <= BORDER)
				result = EnumPosition.PostY;
			else if (Math.abs(x) < Math.abs(z))
				result = (z < 0 ? EnumPosition.EdgeNYNZ : EnumPosition.EdgeNYPZ);
			else
				result = (x < 0 ? EnumPosition.EdgeNXNY : EnumPosition.EdgePXNY);
			break;
		case Dir.NY:
			if (x >= -BORDER && x <= BORDER && z >= -BORDER && z <= BORDER)
				result = EnumPosition.PostY;
			else if (Math.abs(x) < Math.abs(z))
				result = (z < 0 ? EnumPosition.EdgePYNZ : EnumPosition.EdgePYPZ);
			else
				result = (x < 0 ? EnumPosition.EdgeNXPY : EnumPosition.EdgePXPY);
			break;
		case Dir.PZ:
			if (x >= -BORDER && x <= BORDER && y >= -BORDER && y <= BORDER)
				result = EnumPosition.PostZ;
			else if (Math.abs(x) < Math.abs(y))
				result = (y < 0 ? EnumPosition.EdgeNYNZ : EnumPosition.EdgePYNZ);
			else
				result = (x < 0 ? EnumPosition.EdgeNXNZ : EnumPosition.EdgePXNZ);
			break;
		case Dir.NZ:
			if (x >= -BORDER && x <= BORDER && y >= -BORDER && y <= BORDER)
				result = EnumPosition.PostZ;
			else if (Math.abs(x) < Math.abs(y))
				result = (y < 0 ? EnumPosition.EdgeNYPZ : EnumPosition.EdgePYPZ);
			else
				result = (x < 0 ? EnumPosition.EdgeNXPZ : EnumPosition.EdgePXPZ);
			break;
		}
		return result;
	}

	static EnumPosition getCornerPlacement(EntityPlayer ply, MovingObjectPosition pos, EnumPosition placingOn) {
		Vec3 hv = pos.hitVec.addVector(-pos.blockX, -pos.blockY, -pos.blockZ);
		double x = hv.xCoord - 0.5;
		double y = hv.yCoord - 0.5;
		double z = hv.zCoord - 0.5;
		EnumPosition result = EnumPosition.CornerPXPYPZ;
		switch (pos.sideHit) {
		case Dir.NX:
			result = EnumPosition.getCornerPosition((placingOn == null || placingOn.x != EnumAxisPosition.Positive ? 1 : -1), (y < 0 ? -1 : 1), (z < 0 ? -1 : 1));
			break;
		case Dir.PX:
			result = EnumPosition.getCornerPosition((placingOn == null || placingOn.x != EnumAxisPosition.Negative ? -1 : 1), (y < 0 ? -1 : 1), (z < 0 ? -1 : 1));
			break;
		case Dir.NY:
			result = EnumPosition.getCornerPosition((x < 0 ? -1 : 1), (placingOn == null || placingOn.y != EnumAxisPosition.Positive ? 1 : -1), (z < 0 ? -1 : 1));
			break;
		case Dir.PY:
			result = EnumPosition.getCornerPosition((x < 0 ? -1 : 1), (placingOn == null || placingOn.y != EnumAxisPosition.Negative ? -1 : 1), (z < 0 ? -1 : 1));
			break;
		case Dir.NZ:
			result = EnumPosition.getCornerPosition((x < 0 ? -1 : 1), (y < 0 ? -1 : 1), (placingOn == null || placingOn.z != EnumAxisPosition.Positive ? 1 : -1));
			break;
		case Dir.PZ:
			result = EnumPosition.getCornerPosition((x < 0 ? -1 : 1), (y < 0 ? -1 : 1), (placingOn == null || placingOn.z != EnumAxisPosition.Negative ? -1 : 1));
			break;
		}
		return result;
	}

	private static void renderEdgeSelector(MovingObjectPosition pos, EntityPlayer ply, double outer_size, double inset) {
		Tessellator t = Tessellator.instance;
		GL11.glLineWidth(4);
		t.startDrawing(GL11.GL_LINES);
		switch (pos.sideHit) {
		case Dir.NX:
		case Dir.PX:
			double x = (pos.sideHit == Dir.NX ? inset : 1 - inset);
			t.addVertex(x, 0, 0);
			t.addVertex(x, 0, 1);
			t.addVertex(x, 0, 1);
			t.addVertex(x, 1, 1);
			t.addVertex(x, 1, 1);
			t.addVertex(x, 1, 0);
			t.addVertex(x, 1, 0);
			t.addVertex(x, 0, 0);

			t.addVertex(x, outer_size, outer_size);
			t.addVertex(x, outer_size, 1 - outer_size);
			t.addVertex(x, outer_size, 1 - outer_size);
			t.addVertex(x, 1 - outer_size, 1 - outer_size);
			t.addVertex(x, 1 - outer_size, 1 - outer_size);
			t.addVertex(x, 1 - outer_size, outer_size);
			t.addVertex(x, 1 - outer_size, outer_size);
			t.addVertex(x, outer_size, outer_size);

			t.addVertex(x, 0, 0);
			t.addVertex(x, outer_size, outer_size);
			t.addVertex(x, 1, 0);
			t.addVertex(x, 1 - outer_size, outer_size);
			t.addVertex(x, 1, 1);
			t.addVertex(x, 1 - outer_size, 1 - outer_size);
			t.addVertex(x, 0, 1);
			t.addVertex(x, outer_size, 1 - outer_size);
			break;
		case Dir.NY:
		case Dir.PY:
			double y = (pos.sideHit == Dir.NY ? inset : 1 - inset);
			t.addVertex(0, y, 0);
			t.addVertex(0, y, 1);
			t.addVertex(0, y, 1);
			t.addVertex(1, y, 1);
			t.addVertex(1, y, 1);
			t.addVertex(1, y, 0);
			t.addVertex(1, y, 0);
			t.addVertex(0, y, 0);

			t.addVertex(outer_size, y, outer_size);
			t.addVertex(outer_size, y, 1 - outer_size);
			t.addVertex(outer_size, y, 1 - outer_size);
			t.addVertex(1 - outer_size, y, 1 - outer_size);
			t.addVertex(1 - outer_size, y, 1 - outer_size);
			t.addVertex(1 - outer_size, y, outer_size);
			t.addVertex(1 - outer_size, y, outer_size);
			t.addVertex(outer_size, y, outer_size);

			t.addVertex(0, y, 0);
			t.addVertex(outer_size, y, outer_size);
			t.addVertex(1, y, 0);
			t.addVertex(1 - outer_size, y, outer_size);
			t.addVertex(1, y, 1);
			t.addVertex(1 - outer_size, y, 1 - outer_size);
			t.addVertex(0, y, 1);
			t.addVertex(outer_size, y, 1 - outer_size);
			break;
		case Dir.NZ:
		case Dir.PZ:
			double z = (pos.sideHit == Dir.NZ ? inset : 1 - inset);
			t.addVertex(0, 0, z);
			t.addVertex(0, 1, z);
			t.addVertex(0, 1, z);
			t.addVertex(1, 1, z);
			t.addVertex(1, 1, z);
			t.addVertex(1, 0, z);
			t.addVertex(1, 0, z);
			t.addVertex(0, 0, z);

			t.addVertex(outer_size, outer_size, z);
			t.addVertex(outer_size, 1 - outer_size, z);
			t.addVertex(outer_size, 1 - outer_size, z);
			t.addVertex(1 - outer_size, 1 - outer_size, z);
			t.addVertex(1 - outer_size, 1 - outer_size, z);
			t.addVertex(1 - outer_size, outer_size, z);
			t.addVertex(1 - outer_size, outer_size, z);
			t.addVertex(outer_size, outer_size, z);

			t.addVertex(0, 0, z);
			t.addVertex(outer_size, outer_size, z);
			t.addVertex(1, 0, z);
			t.addVertex(1 - outer_size, outer_size, z);
			t.addVertex(1, 1, z);
			t.addVertex(1 - outer_size, 1 - outer_size, z);
			t.addVertex(0, 1, z);
			t.addVertex(outer_size, 1 - outer_size, z);
			break;
		}
		t.draw();
	}

	private static void renderCornerSelector(MovingObjectPosition pos, EntityPlayer ply, double inset) {
		Tessellator t = Tessellator.instance;
		GL11.glLineWidth(4);
		t.startDrawing(GL11.GL_LINES);
		switch (pos.sideHit) {
		case Dir.NX:
		case Dir.PX:
			double x = (pos.sideHit == Dir.NX ? inset : 1 - inset);
			t.addVertex(x, 0, 0);
			t.addVertex(x, 0, 1);
			t.addVertex(x, 0, 1);
			t.addVertex(x, 1, 1);
			t.addVertex(x, 1, 1);
			t.addVertex(x, 1, 0);
			t.addVertex(x, 1, 0);
			t.addVertex(x, 0, 0);
			t.addVertex(x, 0.5, 0);
			t.addVertex(x, 0.5, 1);
			t.addVertex(x, 0, 0.5);
			t.addVertex(x, 1, 0.5);
			break;
		case Dir.NY:
		case Dir.PY:
			double y = (pos.sideHit == Dir.NY ? inset : 1 - inset);
			t.addVertex(0, y, 0);
			t.addVertex(0, y, 1);
			t.addVertex(0, y, 1);
			t.addVertex(1, y, 1);
			t.addVertex(1, y, 1);
			t.addVertex(1, y, 0);
			t.addVertex(1, y, 0);
			t.addVertex(0, y, 0);
			t.addVertex(0.5, y, 0);
			t.addVertex(0.5, y, 1);
			t.addVertex(0, y, 0.5);
			t.addVertex(1, y, 0.5);
			break;
		case Dir.NZ:
		case Dir.PZ:
			double z = (pos.sideHit == Dir.NZ ? inset : 1 - inset);
			t.addVertex(0, 0, z);
			t.addVertex(0, 1, z);
			t.addVertex(0, 1, z);
			t.addVertex(1, 1, z);
			t.addVertex(1, 1, z);
			t.addVertex(1, 0, z);
			t.addVertex(1, 0, z);
			t.addVertex(0, 0, z);
			t.addVertex(0.5, 0, z);
			t.addVertex(0.5, 1, z);
			t.addVertex(0, 0.5, z);
			t.addVertex(1, 0.5, z);
			break;
		}
		t.draw();
	}

	private static boolean canPlaceInBlock(World world, int x, int y, int z) {
		int blockid = world.getBlockId(x, y, z);
		if (blockid == 0)
			return true;
		TileEntity te = world.getBlockTileEntity(x, y, z);
		return te instanceof IMicroblockSupporterTile;
	}
}
