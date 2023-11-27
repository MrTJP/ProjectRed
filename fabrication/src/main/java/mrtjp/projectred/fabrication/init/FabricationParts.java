package mrtjp.projectred.fabrication.init;

import codechicken.multipart.api.MultipartType;
import codechicken.multipart.api.SimpleMultipartType;
import mrtjp.projectred.fabrication.item.FabricatedGatePartItem;
import mrtjp.projectred.fabrication.part.FabricatedGatePart;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.part.GatePart;
import net.minecraft.world.item.Item;
import net.minecraftforge.registries.RegistryObject;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.ITEMS;
import static mrtjp.projectred.fabrication.ProjectRedFabrication.PARTS;

public class FabricationParts {

        public static final String ID_FABRICATED_GATE = "fabricated_gate";

        public static void register() {

            // Items
            RegistryObject<Item> registeredItem = ITEMS.register(ID_FABRICATED_GATE, () -> new FabricatedGatePartItem(GateType.FABRICATED_GATE));

            // Parts
            RegistryObject<MultipartType<GatePart>> registeredPartType = PARTS.register(ID_FABRICATED_GATE, () -> new SimpleMultipartType<>(b -> new FabricatedGatePart()));

            // Add to GateType enum
            GateType.FABRICATED_GATE.inject(ID_FABRICATED_GATE, isClient -> new FabricatedGatePart(), registeredItem, registeredPartType);
        }
}
