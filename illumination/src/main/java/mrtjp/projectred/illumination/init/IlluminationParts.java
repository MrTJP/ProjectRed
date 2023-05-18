package mrtjp.projectred.illumination.init;

import mrtjp.projectred.illumination.MultipartLightType;

import static mrtjp.projectred.illumination.ProjectRedIllumination.ITEMS;
import static mrtjp.projectred.illumination.ProjectRedIllumination.PARTS;

public class IlluminationParts {

    public static void register() {

        for (MultipartLightType type : MultipartLightType.values()) {
            type.registerParts(PARTS, ITEMS);
        }
    }
}
