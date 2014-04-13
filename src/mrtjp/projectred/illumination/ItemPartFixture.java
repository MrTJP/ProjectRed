package mrtjp.projectred.illumination;


public class ItemPartFixture extends ItemPartLightBase
{
    public ItemPartFixture(int id, boolean isInverted)
    {
        super(id, isInverted);
        this.setUnlocalizedName("projectred.illumination.fixture");
    }

    @Override
    public String getLightPartID()
    {
        return "pr_fixture";
    }
}
