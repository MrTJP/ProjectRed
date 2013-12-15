package mrtjp.projectred.illumination;

import mrtjp.projectred.ProjectRedIllumination;
import net.minecraft.item.ItemStack;
import codechicken.lib.lighting.LazyLightMatrix;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Vector3;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class LanternPart extends BaseLightPart
{
    private static final Cuboid6 bounds = new Cuboid6(0.35D, 0.25D, 0.35D, 0.65D, 0.75D, 0.65D);

    @Override
    @SideOnly(Side.CLIENT)
    public void renderStatic(Vector3 pos, LazyLightMatrix olm, int pass)
    {
        if (pass == 0)
            RenderLantern.instance.renderLantern(this);
    }

    @Override
    public ItemStack getItem()
    {
        return new ItemStack(isInverted ? ProjectRedIllumination.itemPartInvLantern : ProjectRedIllumination.itemPartLantern, 1, type);
    }

    @Override
    public Cuboid6 getBounds()
    {
        return bounds;
    }

    @Override
    public int getSlotMask()
    {
        return 1 << 6;
    }

    @Override
    public String getType()
    {
        return "pr_lantern";
    }

    @Override
    public Cuboid6 getLightBounds()
    {
        return RenderLantern.lightBox;
    }
}
