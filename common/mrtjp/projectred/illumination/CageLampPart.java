package mrtjp.projectred.illumination;

import mrtjp.projectred.ProjectRedIllumination;
import net.minecraft.client.renderer.RenderBlocks;
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
    static Cuboid6 lightBounds[] = new Cuboid6[6];
    static
    {
        for (int i = 0; i < 6; i++)
        {
            Transformation t = Rotation.sideRotations[i].at(Vector3.center);
            bounds[i] = new Cuboid6(2 / 16D, 0, 2 / 16D, 14 / 16D, 11 / 16D, 14 / 16D).apply(t);
            lightBounds[i] = new Cuboid6(4 / 16D, 0, 4 / 16D, 12 / 16D, 10 / 16D, 12 / 16D).apply(t).expand(-0.001);
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
    @SideOnly(Side.CLIENT)
    public void renderDynamic(Vector3 pos, float frame, int pass)
    {
        if (pass == 0 && isOn())
            RenderHalo.addLight(x(), y(), z(), type, side, lightBounds[side]);
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void drawBreaking(RenderBlocks r)
    {
        RenderCageLamp.instance.renderBreaking(this, r.overrideBlockTexture);
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
    public int getSlotMask()
    {
        return 1 << side;
    }

    @Override
    public int redstoneConductionMap()
    {
        return 0;
    }

    @Override
    public boolean solid(int arg0)
    {
        return false;
    }
}
