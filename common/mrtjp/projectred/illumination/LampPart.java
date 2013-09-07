package mrtjp.projectred.illumination;

import java.util.Arrays;

import mrtjp.projectred.ProjectRedIllumination;
import mrtjp.projectred.core.BasicUtils;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.MovingObjectPosition;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.lighting.LazyLightMatrix;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.IRedstonePart;
import codechicken.multipart.JCuboidPart;
import codechicken.multipart.JNormalOcclusion;
import codechicken.multipart.NormalOcclusionTest;
import codechicken.multipart.PartMap;
import codechicken.multipart.TFacePart;
import codechicken.multipart.TMultiPart;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class LampPart extends JCuboidPart implements TFacePart, JNormalOcclusion, IRedstonePart, ILight {

	EnumLamp type;
	private boolean isInverted;
	public boolean powered;
	public boolean updateNextTick = true;
	public boolean updateStateNextTick = true;

	public LampPart(EnumLamp type, boolean isInverted) {
		this.type = type;
		this.isInverted = isInverted;
	}

	@Override
	public void writeDesc(MCDataOutput out) {
		out.writeByte(type.meta);
		out.writeBoolean(isInverted);
		out.writeBoolean(powered);
	}

	@Override
	public void readDesc(MCDataInput in) {
		type = EnumLamp.get(in.readByte());
		isInverted = in.readBoolean();
		powered = in.readBoolean();
		updateStateNextTick = true;
		updateNextTick = true;
	}

	@Override
	public void save(NBTTagCompound nbt) {
		nbt.setBoolean("inverted", isInverted);
		nbt.setInteger("meta", type.ordinal());
		nbt.setBoolean("powered", powered);
	}

	@Override
	public void load(NBTTagCompound nbt) {
		isInverted = nbt.getBoolean("inverted");
		type = EnumLamp.get(nbt.getInteger("meta"));
		powered = nbt.getBoolean("powered");
		updateStateNextTick = true;
		updateNextTick = true;
	}

	@Override
	public void onNeighborChanged() {
		updateStateNextTick = true;
	}

	private boolean isBeingPowered() {
		return world().isBlockIndirectlyGettingPowered(x(), y(), z());
	}

	public void updateState() {
		if (BasicUtils.isClient(world())) {
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

	@Override
	public String getType() {
		return (isInverted ? "inv." : "") + "Lamp";
	}

	@Override
	public void update() {
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
	    if (pass == 0)
	        LampRenderer.instance.renderLamp(this, null);
	}

	@Override
	@SideOnly(Side.CLIENT)
	public void drawBreaking(RenderBlocks r) {
		LampRenderer.instance.renderLamp(this, r);
	}

	@Override
	public float getStrength(MovingObjectPosition hit, EntityPlayer player) {
		return 2;
	}

	public ItemStack getItem() {
		return new ItemStack(isInverted ? ProjectRedIllumination.itemPartInvLamp : ProjectRedIllumination.itemPartLamp, 1, type.meta);
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
		return Cuboid6.full;
	}

	@Override
	public boolean occlusionTest(TMultiPart npart) {
		return NormalOcclusionTest.apply(this, npart);
	}

	@Override
	public int getSlotMask() {
		return 1 << 6 | 1 << 0 | 1 << 1 | 1 << 2 | 1 << 3 | 1 << 4 | 1 << 5;
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
	public int weakPowerLevel(int s) {
		return world().getBlockPowerInput(x(), y(), z());
	}

	@Override
	public int redstoneConductionMap() {
		return 1 << 6 | 1 << 0 | 1 << 1 | 1 << 2 | 1 << 3 | 1 << 4 | 1 << 5;
	}

	@Override
	public boolean solid(int arg0) {
        return true;
    }

    @Override
    public boolean isOn() {
        return getLightValue() == 15;
    }

}
