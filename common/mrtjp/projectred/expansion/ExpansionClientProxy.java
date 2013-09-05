package mrtjp.projectred.expansion;


public class ExpansionClientProxy extends ExpansionProxy {
    
    @Override
    public void init() {
        super.init();
        /*LanguageRegistry.addName(new ItemStack(itemVAWT, 1, 0), "Vertical-Axis Wind Turbine");
        
        for (EnumTube t : EnumTube.VALID_TUBE) {
            LanguageRegistry.addName(new ItemStack(itemPartTube, 1, t.meta), t.name);
        }*/
    }

    @Override
    public void postinit() {
        super.postinit();
        ExpansionRecipes.initRecipes();
    }
}
