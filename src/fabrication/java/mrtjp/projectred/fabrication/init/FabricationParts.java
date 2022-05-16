package mrtjp.projectred.fabrication.init;

import codechicken.multipart.api.SimpleMultiPartType;
import codechicken.multipart.api.part.TMultiPart;
import mrtjp.projectred.fabrication.part.FabricatedGatePart;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.ItemPartGate;

import static mrtjp.projectred.ProjectRedFabrication.ITEMS;
import static mrtjp.projectred.ProjectRedFabrication.PARTS;

public class FabricationParts {

        public static final String ID_FABRICATED_GATE = "fabricated_gate";

        public static void register() {

            GateType.FABRICATED_GATE.inject(
                    () -> () -> FabricationReferences.FABRICATED_GATE_ITEM,
                    () -> () -> FabricationReferences.FABRICATED_GATE_PART);

            // Items
            ITEMS.register(ID_FABRICATED_GATE, () -> new ItemPartGate(GateType.FABRICATED_GATE));

            // Parts
            PARTS.register(ID_FABRICATED_GATE, () -> new SimpleMultiPartType<TMultiPart>(b -> new FabricatedGatePart()));
        }
}
