package mrtjp.projectred.illumination;

import net.minecraft.client.renderer.texture.IconRegister;

public class ItemPartLantern extends ItemPartLightBase
{
    public ItemPartLantern(int id, boolean isInverted)
    {
        super(id, isInverted);
        this.setUnlocalizedName("projectred.illumination.lantern");
    }

    @Override
    public String getLightPartID()
    {
        return "pr_lantern";
    }

    @Override
    public void registerIcons(IconRegister reg)
    {
        RenderLantern.registerIcons(reg);
    }
}
