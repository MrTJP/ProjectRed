package mrtjp.projectred.transmission;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.Messenger;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.ChatMessageComponent;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;
import buildcraft.api.power.IPowerReceptor;
import buildcraft.api.power.PowerHandler;
import buildcraft.api.power.PowerHandler.PowerReceiver;
import buildcraft.api.power.PowerHandler.Type;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.PartMap;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

public class MJXPart extends WirePart {
	public float MAX_MJ = 100;
	public float saturation = 0;
	public float latency = 10f;

	public MJXPart(EnumWire type, boolean isJacketedWire, int onside) {
		super(type, isJacketedWire, onside);
	}

	@Override
	public boolean getExternalConnectionOveride(int absDir) {
		if (isJacketed) {
			BlockCoord b = new BlockCoord(tile());
			b.offset(absDir);
			TileEntity t = BasicUtils.getTileEntity(world(), b, TileEntity.class);
			if (t instanceof IPowerReceptor) {
				return true;
			}
		}
		return false;
	}

	@Override
	public boolean connectsToWireType(WirePart wire) {
		if (getWireType() == EnumWire.MJX_N) {
			return wire instanceof MJXPart;
		} else {
			return getWireType() == wire.getWireType();
		}
	}

	@Override
	public void update() {
		super.update();
		if (BasicUtils.isServer(world()))
			Messenger.addMessage(new BlockCoord(tile()), "" + saturation);
		propegateToNeighbors();
	}

	public float addPower(float power) {
		float previousPower = saturation;
		saturation += power;
		if (saturation > MAX_MJ) {
			saturation = MAX_MJ;
		}
		float changed = saturation - previousPower;
		return changed;
	}

	public float subtract(float power) {
		float previousPower = saturation;
		saturation -= power;
		if (saturation < 0) {
			saturation = 0;
		}
		float changed = previousPower - saturation;
		return changed;
	}

	public float getPull() {
		return (MAX_MJ - saturation);
	}

	public void propegateToNeighbors() {
		if (isJacketed) {
			for (int i = 0; i < 6; i++) {
				if (maskConnectsJacketed(i)) {
					TMultiPart t = null;
					if (tile().partMap(i) != null) {
						t = tile().partMap(i);
					} else {
						ForgeDirection fd = ForgeDirection.VALID_DIRECTIONS[i];
						int x = x() + fd.offsetX;
						int y = y() + fd.offsetY;
						int z = z() + fd.offsetZ;
						TileMultipart tile = BasicUtils.getTileEntity(world(), new BlockCoord(x, y, z), TileMultipart.class);
						if (tile != null) {
							t = tile.partMap(PartMap.CENTER.i);
						}
					}
					if (t instanceof MJXPart) {
						if (((MJXPart) t).getPull() > getPull()) {
							float change = ((MJXPart) t).addPower(subtract(Math.min(latency, saturation)));
							return;
						} else if (((MJXPart) t).getPull() < getPull()) {
							float change = addPower(((MJXPart) t).subtract(Math.min(((MJXPart) t).latency, ((MJXPart) t).saturation)));
							return;
						}
					}
				}
			}
		} else {
			if (localJacketedConnection) {
				TMultiPart t = tile().partMap(PartMap.CENTER.i);
				if (t instanceof MJXPart) {
					if (((MJXPart) t).getPull() > getPull()) {
						float change = ((MJXPart) t).addPower(subtract(Math.min(latency, saturation)));
					} else if (((MJXPart) t).getPull() < getPull()) {
						float change = addPower(((MJXPart) t).subtract(Math.min(((MJXPart) t).latency, ((MJXPart) t).saturation)));
					}
				}
			}
			for (int dir = 0; dir < 6; dir++) {
				ForgeDirection fd = ForgeDirection.VALID_DIRECTIONS[dir];
				int x = x() + fd.offsetX;
				int y = y() + fd.offsetY;
				int z = z() + fd.offsetZ;
				if (maskConnectsAroundCorner(dir)) {
					fd = ForgeDirection.VALID_DIRECTIONS[side];
					x += fd.offsetX;
					y += fd.offsetY;
					z += fd.offsetZ;
					if (world().getBlockId(x, y, z) == tile().getBlockType().blockID) {
						TileMultipart tile = BasicUtils.getTileEntity(world(), new BlockCoord(x, y, z), TileMultipart.class);
						if (tile != null) {
							TMultiPart t = tile.partMap(dir ^ 1);
							if (t instanceof MJXPart) {
								if (((MJXPart) t).getPull() > getPull()) {
									float change = ((MJXPart) t).addPower(subtract(Math.min(latency, saturation)));
								} else if (((MJXPart) t).getPull() < getPull()) {
									float change = addPower(((MJXPart) t).subtract(Math.min(((MJXPart) t).latency*2, ((MJXPart) t).saturation)));
								}
							}
						}
						continue;
					}
				} else if (maskConnectsInternally(dir)) {
					TMultiPart t = tile().partMap(dir);
					if (t instanceof MJXPart) {
						if (((MJXPart) t).getPull() > getPull()) {
							float change = ((MJXPart) t).addPower(subtract(Math.min(latency, saturation)));
						} else if (((MJXPart) t).getPull() < getPull()) {
							float change = addPower(((MJXPart) t).subtract(Math.min(((MJXPart) t).latency, ((MJXPart) t).saturation)));
						}
						continue;
					}
				} else if (maskConnects(dir)) {
					if (world().getBlockId(x, y, z) == tile().getBlockType().blockID) {
						TileMultipart tile = BasicUtils.getTileEntity(world(), new BlockCoord(x, y, z), TileMultipart.class);
						if (tile != null) {
							TMultiPart t = tile.partMap(side);
							if (t instanceof MJXPart) {
								if (((MJXPart) t).getPull() > getPull()) {
									float change = ((MJXPart) t).addPower(subtract(Math.min(latency, saturation)));
								} else if (((MJXPart) t).getPull() < getPull()) {
									float change = addPower(((MJXPart) t).subtract(Math.min(((MJXPart) t).latency, ((MJXPart) t).saturation)));
								}
							}
						}
					}
					continue;
				}
			}
		}
	}

	@Override
	protected boolean debug(EntityPlayer ply) {
		if (ply.getHeldItem() != null && ply.getHeldItem().getItem() == ProjectRed.itemScrewdriver) {
			if (ply.isSneaking()) {
				subtract(100);
			} else {
				addPower(100);
			}
		}
		// ply.sendChatToPlayer(ChatMessageComponent.func_111077_e((world().isRemote
		// ? "Client" : "Server") + " saturation: " + saturation));
		return true;
	}
}
