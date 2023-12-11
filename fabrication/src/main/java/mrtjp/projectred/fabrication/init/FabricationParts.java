package mrtjp.projectred.fabrication.init;

import codechicken.multipart.api.MultipartType;
import codechicken.multipart.api.SimpleMultipartType;
import mrtjp.projectred.fabrication.item.FabricatedGatePartItem;
import mrtjp.projectred.fabrication.part.FabricatedGatePart;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.item.GatePartItem;
import mrtjp.projectred.integration.part.GatePart;
import net.minecraftforge.registries.RegistryObject;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.ITEMS;
import static mrtjp.projectred.fabrication.ProjectRedFabrication.PARTS;

@SuppressWarnings("NotNullFieldNotInitialized")
public class FabricationParts {

    public static final String ID_FABRICATED_GATE = "fabricated_gate";

    public static RegistryObject<GatePartItem> FABRICATED_GATE_ITEM;
    public static RegistryObject<MultipartType<GatePart>> FABRICATED_GATE_PART;

    public static void register() {

        // Items
        FABRICATED_GATE_ITEM = ITEMS.register(ID_FABRICATED_GATE, () -> new FabricatedGatePartItem(GateType.FABRICATED_GATE));

        // Parts
        FABRICATED_GATE_PART = PARTS.register(ID_FABRICATED_GATE, () -> new SimpleMultipartType<>(b -> new FabricatedGatePart()));

        // Add to GateType enum
        GateType.FABRICATED_GATE.inject(ID_FABRICATED_GATE, isClient -> new FabricatedGatePart(), FABRICATED_GATE_ITEM, FABRICATED_GATE_PART);
    }
}
