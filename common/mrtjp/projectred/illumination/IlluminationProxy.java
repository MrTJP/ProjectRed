package mrtjp.projectred.illumination;

import static mrtjp.projectred.ProjectRedIllumination.itemPartInvLamp;
import static mrtjp.projectred.ProjectRedIllumination.itemPartInvLantern;
import static mrtjp.projectred.ProjectRedIllumination.itemPartLamp;
import static mrtjp.projectred.ProjectRedIllumination.itemPartLantern;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import codechicken.multipart.MultiPartRegistry;
import codechicken.multipart.MultiPartRegistry.IPartFactory;
import codechicken.multipart.TMultiPart;

public class IlluminationProxy implements IProxy, IPartFactory {

    @Override
    public void preinit() {

    }

    @Override
    public void init() {
        MultiPartRegistry.registerParts(this, new String[] { "Lantern", "inv.Lantern", "Lamp", "inv.Lamp" });

        itemPartLantern = new ItemPartLantern(Configurator.part_lantern.getInt(), false);
        itemPartInvLantern = new ItemPartLantern(Configurator.part_invlantern.getInt(), true);

        itemPartLamp = new ItemPartLamp(Configurator.part_lamp.getInt(), false);
        itemPartInvLamp = new ItemPartLamp(Configurator.part_invlamp.getInt(), true);

        EnumLamp.initOreDictDefinitions();
        EnumLantern.initOreDictDefinitions();
    }

    @Override
    public void postinit() {
        IlluminationRecipes.initIlluminationRecipes();
    }

    @Override
    public TMultiPart createPart(String name, boolean arg1) {
        boolean inverted = false;
        if (name.startsWith("inv.")) {
            name = name.substring(4);
            inverted = true;
        }
        if (name.matches("Lantern")) {
            return new LanternPart(EnumLantern.WHITE, inverted, 0);
        } else if (name.matches("Lamp")) {
            return new LampPart(EnumLamp.WHITE, inverted);
        }
        return null;
    }
}
