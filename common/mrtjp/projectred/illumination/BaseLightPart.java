package mrtjp.projectred.illumination;

import java.util.Arrays;

import mrtjp.projectred.ProjectRedIllumination;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.BasicWireUtils;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagInt;
import net.minecraft.util.MovingObjectPosition;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.lighting.LazyLightMatrix;
import codechicken.lib.render.IconTransformation;
import codechicken.lib.render.RenderUtils;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.IRedstonePart;
import codechicken.multipart.JCuboidPart;
import codechicken.multipart.JNormalOcclusion;
import codechicken.multipart.NormalOcclusionTest;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TSlottedPart;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public abstract class BaseLightPart extends JCuboidPart implements TSlottedPart, JNormalOcclusion, IRedstonePart, ILight
{
    protected byte type;
    protected boolean isInverted;
    protected boolean powered;
    protected byte side;

    public BaseLightPart()
    {
    }

    public void preparePlacement(int side, int meta, boolean inv)
    {
        this.isInverted = inv;
        this.side = (byte) side;
        this.type = (byte) meta;
    }

    @Override
    public void writeDesc(MCDataOutput out)
    {
        out.writeByte(type);
        out.writeBoolean(isInverted);
        out.writeByte(side);
        out.writeBoolean(powered);
    }

    @Override
    public void readDesc(MCDataInput in)
    {
        type = in.readByte();
        isInverted = in.readBoolean();
        side = in.readByte();
        powered = in.readBoolean();
    }

    @Override
    public void save(NBTTagCompound nbt)
    {
        nbt.setBoolean("inverted", isInverted);
        nbt.setByte("meta", type);
        nbt.setByte("rot", side);
        nbt.setBoolean("powered", powered);
    }

    @Override
    public void load(NBTTagCompound nbt)
    {
        isInverted = nbt.getBoolean("inverted");
        type = nbt.getByte("meta");
        if (nbt.getTag("rot") instanceof NBTTagInt)// legacy
            side = (byte) nbt.getInteger("rot");
        else
            side = nbt.getByte("rot");
        powered = nbt.getBoolean("powered");
    }

    @Override
    public void onNeighborChanged()
    {
        if (checkSupport())
            return;
        updateState(false);
    }

    @Override
    public void onPartChanged(TMultiPart t)
    {
        updateState(false);
    }

    @Override
    public void onAdded()
    {
        updateState(true);
    }

    private boolean isBeingPowered()
    {
        return world().isBlockIndirectlyGettingPowered(x(), y(), z());
    }

    public void updateState(boolean forceRender)
    {
        boolean updated = false;
        if (!world().isRemote && powered != isBeingPowered())
        {
            powered = !powered;
            updateRender();
            updated = true;
        }
        if (forceRender && !updated)
            updateRender();
    }

    public void updateRender()
    {
        world().markBlockForUpdate(x(), y(), z());
        world().updateAllLightTypes(x(), y(), z());
        if (!world().isRemote)
            sendDescUpdate();
    }

    public boolean checkSupport()
    {
        if (BasicUtils.isClient(world()))
            return false;

        BlockCoord bc = new BlockCoord(x(), y(), z()).offset(side);
        if (!BasicWireUtils.canPlaceWireOnSide(world(), bc.x, bc.y, bc.z, ForgeDirection.getOrientation(side ^ 1), false) && !(BasicWireUtils.canPlaceTorchOnBlock(world(), bc.x, bc.y, bc.z, false) && (side ^ 1) == 0))
        {
            BasicUtils.dropItemFromLocation(world(), getItem(), false, null, side, 10, new BlockCoord(x(), y(), z()));
            tile().remPart(this);
            return true;
        }
        return false;
    }

    @Override
    public boolean doesTick()
    {
        return isAirous();
    }

    @Override
    public void update()
    {
        if (!world().isRemote && isOn() && isAirous())
        {
            int radius = 16;

            int x = x() + world().rand.nextInt(radius) - world().rand.nextInt(radius);
            int y = y() + world().rand.nextInt(radius) - world().rand.nextInt(radius);
            int z = z() + world().rand.nextInt(radius) - world().rand.nextInt(radius);

            if (y > world().getHeightValue(x, z) + 4)
                y = world().getHeightValue(x, z) + 4;

            if (y < 7)
                y = 7;

            if (world().isAirBlock(x, y, z) && world().getBlockLightValue(x, y, z) < 8)
            {
                world().setBlock(x, y, z, ProjectRedIllumination.blockAirousLight().blockID, getColor(), 3);

                TileAirousLight light = (TileAirousLight) world().getBlockTileEntity(x, y, z);
                if (light != null)
                    light.setSource(new BlockCoord(tile()), getColor(), side);
            }
        }
    }

    public boolean isAirous()
    {
        return false;
    }

    @Override
    public int getLightValue()
    {
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
    public void renderDynamic(Vector3 pos, float frame, int pass)
    {
        if (pass == 0 && isOn())
            RenderHalo.addLight(x(), y(), z(), type, getLightBounds());
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void drawBreaking(RenderBlocks r) {
        RenderUtils.renderBlock(getLightBounds(), 0, new Translation(x(), y(), z()), new IconTransformation(r.overrideBlockTexture), null);
    }

    @Override
    public abstract String getType();

    @Override
    public abstract Cuboid6 getBounds();

    public abstract Cuboid6 getLightBounds();

    public abstract ItemStack getItem();

    @Override
    public int getSlotMask()
    {
        return 1 << side;
    }

    @Override
    public float getStrength(MovingObjectPosition hit, EntityPlayer player)
    {
        return 2;
    }

    @Override
    public Iterable<ItemStack> getDrops()
    {
        return Arrays.asList(getItem());
    }

    @Override
    public ItemStack pickItem(MovingObjectPosition hit)
    {
        return getItem();
    }

    @Override
    public boolean occlusionTest(TMultiPart npart)
    {
        return NormalOcclusionTest.apply(this, npart);
    }

    @Override
    public Iterable<Cuboid6> getOcclusionBoxes()
    {
        return Arrays.asList(getBounds());
    }

    @Override
    public boolean canConnectRedstone(int arg0)
    {
        return true;
    }

    @Override
    public int strongPowerLevel(int arg0)
    {
        return 0;
    }

    @Override
    public int weakPowerLevel(int arg0)
    {
        return 0;
    }

    @Override
    public boolean isOn()
    {
        return getLightValue() == 15;
    }

    @Override
    public int getColor()
    {
        return type;
    }
}
