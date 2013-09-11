package mrtjp.projectred.compatability;

import static mrtjp.projectred.ProjectRedCompatability.*;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import codechicken.multipart.MultiPartRegistry;
import codechicken.multipart.MultiPartRegistry.IPartFactory;
import codechicken.multipart.MultipartGenerator;
import codechicken.multipart.TMultiPart;

public class CompatabilityProxy implements IProxy, IPartFactory {

    @Override
    public void preinit() {

    }

    @Override
    public void init() {
        
    }

    @Override
    public void postinit() {

    }

    @Override
    public TMultiPart createPart(String id, boolean arg1) {
        return null;
    }

}
