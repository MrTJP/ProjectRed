package mrtjp.projectred.expansion.init;

import codechicken.multipart.api.MultipartType;
import codechicken.multipart.api.SimpleMultipartType;
import mrtjp.projectred.expansion.TubeType;
import mrtjp.projectred.expansion.part.FramePart;
import mrtjp.projectred.expansion.part.FramePartConverter;

import java.util.function.Supplier;

import static mrtjp.projectred.expansion.ProjectRedExpansion.*;

@SuppressWarnings("NotNullFieldNotInitialized")
public class ExpansionParts {

    public static final String ID_FRAME = "frame";

    public static final String ID_PNEUMATIC_TUBE = "pneumatic_tube";

    public static Supplier<MultipartType<FramePart>> FRAME_PART;

    public static void register() {

        FRAME_PART = PART_TYPES.register(ID_FRAME, () -> new SimpleMultipartType<>(c -> new FramePart()));

        PART_CONVERTERS.register(ID_FRAME, () -> FramePartConverter.INSTANCE);

        for (TubeType type : TubeType.values()) {
            type.registerParts(PART_TYPES, ITEMS);
        }
    }
}
