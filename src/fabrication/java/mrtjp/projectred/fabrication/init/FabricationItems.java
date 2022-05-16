package mrtjp.projectred.fabrication.init;

import mrtjp.projectred.fabrication.item.ICBlueprintItem;

import static mrtjp.projectred.ProjectRedFabrication.ITEMS;

public class FabricationItems {

    public static final String ID_IC_BLUEPRINT = "ic_blueprint";

    public static void register() {

        // Items
        ITEMS.register(ID_IC_BLUEPRINT, ICBlueprintItem::new);
    }
}
