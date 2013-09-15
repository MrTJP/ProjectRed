package mrtjp.projectred.illumination;

import java.util.Arrays;

import mrtjp.projectred.ProjectRedIllumination;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.BasicWireUtils;
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

public abstract class BaseLightPart extends JCuboidPart implements TSlottedPart, JNormalOcclusion, IRedstonePart, ILight {

    protected byte type;
    protected boolean isInverted;
    protected boolean powered;
    protected int side;
    protected boolean initialized = false;

    public BaseLightPart() {}
    
    public void preparePlacement(int side, int meta, boolean inv) {
        this.isInverted = inv;
        this.side = side;
        this.type = (byte) meta;
    }
    
    @Override
    public void writeDesc(MCDataOutput out) {
        out.writeByte(type);
        out.writeBoolean(isInverted);
        out.writeByte(side);
        out.writeBoolean(powered);
    }
    
    @Override
    public void readDesc(MCDataInput in) {
        type = in.readByte();
        isInverted = in.readBoolean();
        side = in.readByte();
        powered = in.readBoolean();
    }
    
    @Override
    public void save(NBTTagCompound nbt) {
        nbt.setBoolean("inverted", isInverted);
        nbt.setByte("meta", type);
        nbt.setInteger("rot", side);
        nbt.setBoolean("powered", powered);
    }
    
    @Override
    public void load(NBTTagCompound nbt) {
        isInverted = nbt.getBoolean("inverted");
        type = nbt.getByte("meta");
        side = nbt.getInteger("rot");
        powered = nbt.getBoolean("powered");
    }
    
    
    @Override
    public void onNeighborChanged(){
        if (checkSupport())
            return;
        updateState(false);
    }
    
    @Override
    public void onPartChanged(TMultiPart t){
        updateState(false);
    }
    
    @Override
    public void onAdded() {
        updateState(true);
    }
        
    private boolean isBeingPowered() {
        return world().isBlockIndirectlyGettingPowered(x(), y(), z()) || world().getBlockPowerInput(x(), y(), z()) > 0;
    }

    public void updateState(boolean forceRender) {
        boolean updated = false;
        if (!powered && isBeingPowered()) {
            powered = true;
            updateRender();
            updated = true;
        } else if (powered && !isBeingPowered()){
            powered = false;
            updateRender();
            updated = true;
        }
        if (forceRender && !updated)
            updateRender();
    }
    
    public void updateRender() {
        world().markBlockForUpdate(x(), y(), z());
        world().updateAllLightTypes(x(), y(), z());
        if (BasicUtils.isServer(world()))
            sendDescUpdate();
    }

    
    public boolean checkSupport() {
        if (BasicUtils.isClient(world()))
            return false;
        BlockCoord bc = new BlockCoord(x(), y(), z());
        bc.offset(side);
        if (!BasicWireUtils.canPlaceWireOnSide(world(), bc.x, bc.y, bc.z, ForgeDirection.getOrientation(side ^ 1), false)) {
            BasicUtils.dropItemFromLocation(world(), getItem(), false, null, side, 10, new BlockCoord(x(), y(), z()));
            tile().remPart(this);
            return true;
        }
        return false;
    }

    
    @Override
    public abstract String getType();
    
    @Override
    public void update() {
        if (!initialized)
            initialized = true;
    }

    @Override
    public int getLightValue() {
        if (powered != isInverted)
            return 15;
        else
            return 0;
    }
    
    @Override
    @SideOnly(Side.CLIENT)
    public abstract void renderStatic(Vector3 pos, LazyLightMatrix olm, int pass);

    @Override
    @SideOnly(Side.CLIENT) 
    public abstract void drawBreaking(RenderBlocks r);
    
    @Override
    public float getStrength(MovingObjectPosition hit, EntityPlayer player) {
        return 2;
    }

    public abstract ItemStack getItem();

    @Override
    public Iterable<ItemStack> getDrops() {
        return Arrays.asList(getItem());
    }

    @Override
    public ItemStack pickItem(MovingObjectPosition hit) {
        return getItem();
    }


    @Override
    public abstract Cuboid6 getBounds();
    
    @Override
    public boolean occlusionTest(TMultiPart npart) {
        return NormalOcclusionTest.apply(this, npart);
    }

    @Override
    public abstract int getSlotMask();

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
    
    @Override
    public boolean isOn() {
        return getLightValue() == 15;
    }
}
