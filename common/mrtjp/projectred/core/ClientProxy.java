package mrtjp.projectred.core;

import static mrtjp.projectred.ProjectRed.initializedModules;

public class ClientProxy extends CommonProxy {

    public static int renderPass;

    @Override
    public void preinit() {
        super.preinit();
        for (IProjectRedModule m : initializedModules) {
            m.getClientProxy().preinit();
        }
    }

    @Override
    public void init() {
        super.init();
        for (IProjectRedModule m : initializedModules) {
            m.getClientProxy().init();
        }
    }

    @Override
    public void postinit() {
        super.postinit();
        for (IProjectRedModule m : initializedModules) {
            m.getCommonProxy().postinit();
        }
    }
}