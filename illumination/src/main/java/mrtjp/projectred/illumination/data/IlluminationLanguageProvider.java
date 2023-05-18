package mrtjp.projectred.illumination.data;

import mrtjp.projectred.illumination.BlockLightType;
import mrtjp.projectred.illumination.MultipartLightType;
import net.minecraft.data.DataGenerator;
import net.minecraftforge.common.data.LanguageProvider;

import static mrtjp.projectred.illumination.ProjectRedIllumination.MOD_ID;

public class IlluminationLanguageProvider extends LanguageProvider {

    private static final String[] LOCAL_COLORS = new String[] {
            "White", "Orange", "Magenta", "Light Blue", "Yellow", "Lime", "Pink", "Gray", "Light Gray", "Cyan", "Purple", "Blue", "Brown", "Green", "Red", "Black"
    };

    public IlluminationLanguageProvider(DataGenerator gen) {
        super(gen, MOD_ID, "en_us");
    }

    @Override
    protected void addTranslations() {

        // Creative tab
        add("itemGroup.projectred-illumination", "Project Red: Illumination");

        // Block lights
        for (BlockLightType type : BlockLightType.values()) {
            for (int color = 0; color < 16; color++) {
                add(type.getBlock(color, false), createLocalizedLightName(color, false, type.getLocalBaseName()));
                add(type.getBlock(color, true), createLocalizedLightName(color, true, type.getLocalBaseName()));
            }
        }

        // Multipart lights
        for (MultipartLightType type : MultipartLightType.values()) {
            for (int color = 0; color < 16; color++) {
                add(type.getItem(color, false), createLocalizedLightName(color, false, type.getLocalBaseName()));
                add(type.getItem(color, true), createLocalizedLightName(color, true, type.getLocalBaseName()));
            }
        }
    }

    public static String createLocalizedLightName(int color, boolean inverted, String lightName) {
        return LOCAL_COLORS[color] + (inverted ? " Inverted " : " ") + lightName;
    }
}
