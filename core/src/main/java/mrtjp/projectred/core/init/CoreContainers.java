package mrtjp.projectred.core.init;

import codechicken.lib.inventory.container.ICCLContainerType;
import mrtjp.projectred.core.inventory.container.ElectrotineGeneratorContainer;

import static mrtjp.projectred.core.ProjectRedCore.CONTAINERS;
import static mrtjp.projectred.core.init.CoreBlocks.ID_ELECTROTINE_GENERATOR;

public class CoreContainers {

    public static void register() {

        CONTAINERS.register(ID_ELECTROTINE_GENERATOR, () -> ICCLContainerType.create(ElectrotineGeneratorContainer.FACTORY));
    }
}
