package mrtjp.projectred.transmission.data;

import mrtjp.projectred.transmission.WireType;
import net.minecraft.data.PackOutput;
import net.minecraftforge.common.data.LanguageProvider;

import static mrtjp.projectred.transmission.ProjectRedTransmission.MOD_ID;

public class TransmissionLanguageProvider extends LanguageProvider {

    private static final String[] LOCAL_COLORS = new String[] {
            "White", "Orange", "Magenta", "Light Blue", "Yellow", "Lime", "Pink", "Gray", "Light Gray", "Cyan", "Purple", "Blue", "Brown", "Green", "Red", "Black"
    };

    public TransmissionLanguageProvider(PackOutput output) {
        super(output, MOD_ID, "en_us");
    }

    @Override
    public String getName() {
        return "ProjectRed-Transmission Language: en_us";
    }

    @Override
    protected void addTranslations() {

        // Creative tab
        add("itemGroup." + MOD_ID, "Project Red: Transmission");

        // Red alloy wire
        add(WireType.RED_ALLOY.getItem(), "Red Alloy Wire");

        // Insulated wires
        for (int i = 0; i < 16; i++) {
            add(WireType.INSULATED_WIRES[i].getItem(), LOCAL_COLORS[i] + " Insulated Wire");
        }

        // Bundled cables
        add(WireType.BUNDLED_NEUTRAL.getItem(), "Bundled Cable");
        for (int i = 0; i < 16; i++) {
            add(WireType.COLOURED_BUNDLED_WIRES[i].getItem(), LOCAL_COLORS[i] + " Bundled Cable");
        }

        // Power lines
        add(WireType.POWER_LOWLOAD.getItem(), "Low Load Power Line");

        // Framed red alloy
        add(WireType.FRAMED_RED_ALLOY.getItem(), "Framed Red Alloy Wire");

        // Framed Insulated wires
        for (int i = 0; i < 16; i++) {
            add(WireType.FRAMED_INSULATED_WIRES[i].getItem(), LOCAL_COLORS[i] + " Framed Insulated Wire");
        }

        // Framed bundled cables
        add(WireType.FRAMED_BUNDLED_NEUTRAL.getItem(), "Framed Bundled Cable");
        for (int i = 0; i < 16; i++) {
            add(WireType.FRAMED_COLOURED_BUNDLED_WIRES[i].getItem(), LOCAL_COLORS[i] + " Framed Bundled Cable");
        }

        // Framed power lines
        add(WireType.FRAMED_POWER_LOWLOAD.getItem(), "Framed Low Load Power Line");
    }
}
