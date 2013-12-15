package mrtjp.projectred.illumination;


public class ItemPartCageLamp extends ItemPartLightBase
{
    public ItemPartCageLamp(int id, boolean isInverted)
    {
        super(id, isInverted);
        this.setUnlocalizedName("projectred.illumination.cagelamp");
    }

    @Override
    public String getLightPartID()
    {
        return "pr_cagelamp";
    }
}
