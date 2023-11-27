package mrtjp.projectred.fabrication.init;

import mrtjp.projectred.fabrication.item.*;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.ITEMS;

public class FabricationItems {

    public static final String ID_IC_BLUEPRINT = "ic_blueprint";
    public static final String ID_BLANK_PHOTOMASK = "blank_photomask";
    public static final String ID_PHOTOMASK_SET = "photomask_set";
    public static final String ID_ROUGH_SILICON_WAFER = "rough_silicon_wafer";
    public static final String ID_ETCHED_SILICON_WAFER = "etched_silicon_wafer";
    public static final String ID_VALID_DIE = "valid_die";
    public static final String ID_INVALID_DIE = "invalid_die";

    public static void register() {

        // Items
        ITEMS.register(ID_IC_BLUEPRINT, ICBlueprintItem::new);
        ITEMS.register(ID_BLANK_PHOTOMASK, BlankPhotomaskItem::new);
        ITEMS.register(ID_PHOTOMASK_SET, PhotomaskSetItem::new);
        ITEMS.register(ID_ROUGH_SILICON_WAFER, RoughSiliconWaferItem::new);
        ITEMS.register(ID_ETCHED_SILICON_WAFER, EtchedSiliconWaferItem::new);
        ITEMS.register(ID_VALID_DIE, ValidDieItem::new);
        ITEMS.register(ID_INVALID_DIE, InvalidDieItem::new);
    }
}