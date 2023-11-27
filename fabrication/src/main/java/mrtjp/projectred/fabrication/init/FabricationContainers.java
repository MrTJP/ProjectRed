package mrtjp.projectred.fabrication.init;

import codechicken.lib.inventory.container.ICCLContainerType;
import mrtjp.projectred.fabrication.inventory.container.LithographyTableContainer;
import mrtjp.projectred.fabrication.inventory.container.PackagingTableContainer;
import mrtjp.projectred.fabrication.inventory.container.PlottingTableContainer;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.CONTAINERS;
import static mrtjp.projectred.fabrication.init.FabricationBlocks.*;

public class FabricationContainers {

    public static void register() {

        CONTAINERS.register(ID_PLOTTING_TABLE, () -> ICCLContainerType.create(PlottingTableContainer.FACTORY));
        CONTAINERS.register(ID_LITHOGRAPHY_TABLE, () -> ICCLContainerType.create(LithographyTableContainer.FACTORY));
        CONTAINERS.register(ID_PACKAGING_TABLE, () -> ICCLContainerType.create(PackagingTableContainer.FACTORY));
    }
}
