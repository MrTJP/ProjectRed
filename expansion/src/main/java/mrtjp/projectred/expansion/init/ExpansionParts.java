package mrtjp.projectred.expansion.init;

import codechicken.multipart.api.SimpleMultipartType;
import mrtjp.projectred.expansion.part.FramePart;
import mrtjp.projectred.expansion.part.FramePartConverter;

import static mrtjp.projectred.expansion.ProjectRedExpansion.PARTS;
import static mrtjp.projectred.expansion.ProjectRedExpansion.PART_CONVERTERS;

public class ExpansionParts {

    public static final String ID_FRAME = "frame";

    public static void register() {

        PARTS.register(ID_FRAME, () -> new SimpleMultipartType<>(c -> new FramePart()));

        PART_CONVERTERS.register(ID_FRAME, () -> FramePartConverter.INSTANCE);
    }
}
