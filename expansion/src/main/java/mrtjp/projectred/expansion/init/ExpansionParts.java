package mrtjp.projectred.expansion.init;

import codechicken.multipart.api.MultipartType;
import codechicken.multipart.api.SimpleMultipartType;
import mrtjp.projectred.expansion.part.FramePart;
import mrtjp.projectred.expansion.part.FramePartConverter;
import net.minecraftforge.registries.RegistryObject;

import static mrtjp.projectred.expansion.ProjectRedExpansion.PART_CONVERTERS;
import static mrtjp.projectred.expansion.ProjectRedExpansion.PART_TYPES;

@SuppressWarnings("NotNullFieldNotInitialized")
public class ExpansionParts {

    public static final String ID_FRAME = "frame";

    public static RegistryObject<MultipartType<FramePart>> FRAME_PART;

    public static void register() {

        FRAME_PART = PART_TYPES.register(ID_FRAME, () -> new SimpleMultipartType<>(c -> new FramePart()));

        PART_CONVERTERS.register(ID_FRAME, () -> FramePartConverter.INSTANCE);
    }
}
