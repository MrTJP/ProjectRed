package mrtjp.projectred.fabrication.engine;

import static mrtjp.projectred.fabrication.init.FabricationUnlocal.*;

public enum ICInterfaceType {
    NC(UL_INTERFACE_NC),
    REDSTONE(UL_INTERFACE_REDSTONE),
    BUNDLED(UL_INTERFACE_BUNDLED),
    ANALOG(UL_INTERFACE_ANALOG),
    ;

    private final String unlocalName;

    ICInterfaceType(String unlocalName) {
        this.unlocalName = unlocalName;
    }

    public int getId() {
        return ordinal();
    }

    public String getUnlocalName() {
        return unlocalName;
    }

    public static ICInterfaceType fromId(int id) {
        return values()[id];
    }
}
