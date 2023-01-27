package mrtjp.projectred.exploration.init;

import codechicken.lib.inventory.container.ICCLContainerType;
import mrtjp.projectred.exploration.inventory.container.BackpackContainer;

import static mrtjp.projectred.ProjectRedExploration.CONTAINERS;

public class ExplorationContainers {

    public static final String ID_BACKPACK_CONTAINER = "backpack";

    public static void register() {

        CONTAINERS.register(ID_BACKPACK_CONTAINER, () -> ICCLContainerType.create(BackpackContainer.FACTORY));
    }
}
