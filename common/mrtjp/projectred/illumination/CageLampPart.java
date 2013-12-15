package mrtjp.projectred.illumination;

import mrtjp.projectred.ProjectRedIllumination;
import net.minecraft.item.ItemStack;
import codechicken.lib.lighting.LazyLightMatrix;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.TFacePart;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class CageLampPart extends BaseLightPart implements TFacePart
{
    static Cuboid6 bounds[] = new Cuboid6[6];
    static
    {
        for (int i = 0; i < 6; i++)
        {
            Transformation t = Rotation.sideRotations[i].at(Vector3.center);
            bounds[i] = new Cuboid6(2 / 16D, 0, 2 / 16D, 14 / 16D, 11 / 16D, 14 / 16D).apply(t);
        }
    }

    @Override
    public String getType()
    {
        return "pr_cagelamp";
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void renderStatic(Vector3 pos, LazyLightMatrix olm, int pass)
    {
        if (pass == 0)
            RenderCageLamp.instance.renderCageLamp(this);
    }
    
    @Override
    public ItemStack getItem()
    {
        return new ItemStack(isInverted ? ProjectRedIllumination.itemPartInvCageLamp : ProjectRedIllumination.itemPartCageLamp, 1, type);
    }

    @Override
    public Cuboid6 getBounds()
    {
        return bounds[side];
    }

    @Override
    public int redstoneConductionMap()
    {
        return 0;
    }

    @Override
    public boolean solid(int side)
    {
        return false;
    }

    @Override
    public Cuboid6 getLightBounds()
    {
        return RenderCageLamp.lightBounds[side];
    }
}
