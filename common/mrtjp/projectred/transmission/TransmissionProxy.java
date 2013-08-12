package mrtjp.projectred.transmission;

import static mrtjp.projectred.ProjectRed.itemPartJacketedWire;
import static mrtjp.projectred.ProjectRed.itemPartWire;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import scala.collection.generic.ImmutableSetFactory;
import scala.collection.immutable.HashSet;
import scala.collection.immutable.Set;
import scala.collection.immutable.Set.Set1;
import scala.collection.mutable.StringBuilder;
import scala.reflect.internal.util.Collections;
import codechicken.multipart.MultiPartRegistry;
import codechicken.multipart.MultiPartRegistry.IPartFactory;
import codechicken.multipart.MultipartGenerator;
import codechicken.multipart.TMultiPart;

public class TransmissionProxy implements IProxy, IPartFactory {

    @Override
    public void preinit() {

    }

    @Override
    public void init() {
        String[] jwires = new String[EnumWire.VALID_WIRE.length];

        for (EnumWire w : EnumWire.VALID_WIRE) {
            jwires[w.meta] = "j." + w.name;
        }
        MultiPartRegistry.registerParts(this, new String[]{"pr_redwire", "pr_insulated", "pr_bundled"});
        MultiPartRegistry.registerParts(this, jwires);

        itemPartWire = new ItemPartWire(Configurator.part_wire.getInt());
        itemPartJacketedWire = new ItemPartJacketedWire(Configurator.part_jwire.getInt());

        TransmissionRecipes.initTransmissionRecipes();
        EnumWire.initOreDictDefinitions();
    }

    @Override
    public void postinit() {

    }

    @Override
    public TMultiPart createPart(String id, boolean client) {
        if(id.equals("pr_redwire"))
            return new RedAlloyWirePart(0);
        else if(id.equals("pr_insulated"))
            return new InsulatedRedAlloyPart(0);
        else if(id.equals("pr_bundled"))
            return null;
        
        return null;
    }

}
