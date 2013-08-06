package mrtjp.projectred.illumination;

import java.util.Arrays;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.transmission.BasicWireUtils;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.MovingObjectPosition;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.lighting.LazyLightMatrix;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.IRedstonePart;
import codechicken.multipart.JCuboidPart;
import codechicken.multipart.JNormalOcclusion;
import codechicken.multipart.NormalOcclusionTest;
import codechicken.multipart.PartMap;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TSlottedPart;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class LanternPart extends JCuboidPart implements TSlottedPart, JNormalOcclusion, IRedstonePart {

	EnumLantern type;
	private boolean isInverted;
	public boolean powered;
	public boolean updateNextTick = true;
	public boolean updateStateNextTick = true;
	public int rotation = 1;
	public boolean initialized = false;

	
	public LanternPart(EnumLantern type, boolean isInverted, int sideAttached) {
		this.type = type;
		this.isInverted = isInverted;
		this.rotation = sideAttached;
	}
	
	@Override
	public void writeDesc(MCDataOutput out) {
		out.writeByte(type.meta);
		out.writeBoolean(isInverted);
		out.writeByte(rotation);
		out.writeBoolean(powered);
	}
	
	@Override
	public void readDesc(MCDataInput in) {
		type = EnumLantern.get(in.readByte());
		isInverted = in.readBoolean();
		rotation = in.readByte();
		powered = in.readBoolean();
		updateStateNextTick = true;
		updateNextTick = true;
	}
	
	@Override
	public void save(NBTTagCompound nbt) {
		nbt.setBoolean("inverted", isInverted);
		nbt.setInteger("meta", type.ordinal());
		nbt.setInteger("rot", rotation);
		nbt.setBoolean("powered", powered);
	}
	
	@Override
	public void load(NBTTagCompound nbt) {
		isInverted = nbt.getBoolean("inverted");
		type = EnumLantern.get(nbt.getInteger("meta"));
		rotation = nbt.getInteger("rot");
		powered = nbt.getBoolean("powered");
		updateStateNextTick = true;
		updateNextTick = true;
	}
	
	
	@Override
	public void onNeighborChanged(){
		checkSupport();
		updateNextTick = true;
		updateStateNextTick = true;
	}
	
	private boolean isBeingPowered() {
		return world().isBlockIndirectlyGettingPowered(x(), y(), z());
	}

	public void updateState() {
		if(BasicUtils.isClient(world())) {
			return;
		}
		if (isBeingPowered()) {
			if (powered) {
				return;
			}
			powered = true;
			updateNextTick = true;
		} else {
			if (!powered) {
				return;
			}
			powered = false;
			updateNextTick = true;
		}
	}

	
	public void checkSupport() {
		if (!initialized ||BasicUtils.isClient(world())) {
			return;
		}
		int x = x() + ForgeDirection.getOrientation(rotation).offsetX;
		int y = y() + ForgeDirection.getOrientation(rotation).offsetY;
		int z = z() + ForgeDirection.getOrientation(rotation).offsetZ;
		if (!BasicWireUtils.canPlaceWireOnSide(world(), x, y, z, ForgeDirection.getOrientation(rotation ^ 1), false)) {
			BasicUtils.dropItemFromLocation(world(), getItem(), false, null, rotation, 10, new BlockCoord(x(), y(), z()));
			tile().remPart(this);
		}
	}

	
	@Override
	public String getType() {
		return (isInverted ? "inv." : "") + "Lantern";
	}
	
	@Override
	public void update(){
		if (updateStateNextTick) {
			updateStateNextTick = false;
			updateState();
		}
		if (updateNextTick) {
			updateNextTick = false;
			world().markBlockForUpdate(x(), y(), z());
			world().updateAllLightTypes(x(), y(), z());
			if (BasicUtils.isServer(world())) {
				sendDescUpdate();
			}
		}

		if (!initialized) {
			initialized = true;
		}
	}

	@Override
	public int getLightValue() {
		if (powered != isInverted) {
			return 15;
		} else {
			return 0;
		}
	}
	
	@Override
	@SideOnly(Side.CLIENT)
	public void renderStatic(Vector3 pos, LazyLightMatrix olm, int pass) {
		LanternRenderer.instance.renderLamp(this, null);
	}

	@Override
	@SideOnly(Side.CLIENT) 
	public void drawBreaking(RenderBlocks r){
		LanternRenderer.instance.renderLamp(this, r);
	}
	
	@Override
	public float getStrength(MovingObjectPosition hit, EntityPlayer player) {
		return 2;
	}

	public ItemStack getItem() {
		return new ItemStack(isInverted ? ProjectRed.itemPartInvLantern : ProjectRed.itemPartLantern, 1, type.meta);
	}

	@Override
	public Iterable<ItemStack> getDrops() {
		return Arrays.asList(getItem());
	}

	@Override
	public ItemStack pickItem(MovingObjectPosition hit) {
		return getItem();
	}


	@Override
	public Cuboid6 getBounds() {
		return new Cuboid6(.34f, .25f, .34f, .66f, .75f, .66f);
	}
	
	@Override
	public boolean occlusionTest(TMultiPart npart) {
		return NormalOcclusionTest.apply(this, npart);
	}

	@Override
	public int getSlotMask() {
		return 1 << PartMap.CENTER.i;
	}

	@Override
	public Iterable<Cuboid6> getOcclusionBoxes() {
		return Arrays.asList(getBounds());
	}

	@Override
	public boolean canConnectRedstone(int arg0) {
		return true;
	}

	@Override
	public int strongPowerLevel(int arg0) {
		return 0;
	}

	@Override
	public int weakPowerLevel(int arg0) {
		return 0;
	}
	
}
