package mrtjp.projectred.expansion;

import static mrtjp.projectred.ProjectRedExpansion.itemPartTube;
import static mrtjp.projectred.ProjectRedExpansion.itemVAWT;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import codechicken.multipart.MultiPartRegistry;
import codechicken.multipart.MultiPartRegistry.IPartFactory;
import codechicken.multipart.TMultiPart;

public class ExpansionProxy implements IProxy, IPartFactory {

    @Override
    public void preinit() {

    }

    @Override
    public void init() {
        MultiPartRegistry.registerParts(this, new String[]{
                "pr_ptube"
        });

        itemVAWT = new ItemVAWT(Configurator.item_vawtID.getInt());
        itemPartTube = new ItemPartPressurizedTube(Configurator.part_tube.getInt());
    }

    @Override
    public void postinit() {
        ExpansionRecipes.initRecipes();
    }

    @Override
    public TMultiPart createPart(String id, boolean arg1) {
        if(id.equals("pr_ptube"))
            return new PressurizedTubePart();
        return null;
    }

}
