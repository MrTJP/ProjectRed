package mrtjp.projectred.expansion.init;

import codechicken.lib.inventory.container.ICCLContainerType;
import mrtjp.projectred.expansion.inventory.container.AutoCrafterContainer;
import mrtjp.projectred.expansion.inventory.container.BatteryBoxContainer;
import mrtjp.projectred.expansion.inventory.container.ChargingBenchContainer;
import mrtjp.projectred.expansion.inventory.container.ProjectBenchContainer;

import static mrtjp.projectred.expansion.ProjectRedExpansion.CONTAINERS;
import static mrtjp.projectred.expansion.init.ExpansionBlocks.*;

public class ExpansionContainers {

    public static void register() {

        CONTAINERS.register(ID_PROJECT_BENCH, () -> ICCLContainerType.create(ProjectBenchContainer.FACTORY));
        CONTAINERS.register(ID_BATTERY_BOX, () -> ICCLContainerType.create(BatteryBoxContainer.FACTORY));
        CONTAINERS.register(ID_AUTO_CRAFTER, () -> ICCLContainerType.create(AutoCrafterContainer.FACTORY));
        CONTAINERS.register(ID_CHARGING_BENCH, () -> ICCLContainerType.create(ChargingBenchContainer.FACTORY));
    }
}
