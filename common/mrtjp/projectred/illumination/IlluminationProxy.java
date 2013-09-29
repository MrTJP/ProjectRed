package mrtjp.projectred.illumination;

import static mrtjp.projectred.ProjectRedIllumination.*;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import codechicken.multipart.MultiPartRegistry;
import codechicken.multipart.MultiPartRegistry.IPartFactory;
import codechicken.multipart.TMultiPart;
import cpw.mods.fml.common.registry.GameRegistry;

public class IlluminationProxy implements IProxy, IPartFactory {

    @Override
    public void preinit() {

    }

    @Override
    public void init() {
        MultiPartRegistry.registerParts(this, new String[] { "pr_lantern", "pr_lightbutton", "pr_cagelamp", "pr_fixture" });

        itemPartLantern = new ItemPartLantern(Configurator.part_lantern.getInt(), false);
        itemPartInvLantern = new ItemPartLantern(Configurator.part_invlantern.getInt(), true);
        itemPartCageLamp = new ItemPartCageLamp(Configurator.part_cagelamp.getInt(), false);
        itemPartInvCageLamp = new ItemPartCageLamp(Configurator.part_invcagelamp.getInt(), true);
        itemPartFixture = new ItemPartFixture(Configurator.part_fixture.getInt(), false);
        itemPartInvFixture = new ItemPartFixture(Configurator.part_invfixture.getInt(), true);
        
        itemPartIllumarButton = new ItemPartIllumarButton(Configurator.part_lightButton.getInt());
        
        blockLamp = new BlockLamp(Configurator.block_lampID.getInt());
        GameRegistry.registerBlock(blockLamp, ItemBlockLamp.class, "projectred.illumination.lamp");
        GameRegistry.registerTileEntity(TileLamp.class, "tile.projectred.illumination.lamp");
        
    }

    @Override
    public void postinit() {
        IlluminationRecipes.initIlluminationRecipes();
    }

    @Override
    public TMultiPart createPart(String name, boolean arg1) {
        if (name.equals("pr_lantern"))
            return new LanternPart();
        else if (name.equals("pr_lightbutton"))
            return new IllumarButtonPart();
        else if (name.equals("pr_cagelamp"))
            return new CageLampPart();
        else if (name.equals("pr_fixture"))
            return new FixturePart();
        return null;
    }
}
