package mrtjp.projectred.expansion.data;

import mrtjp.projectred.expansion.init.ExpansionUnlocal;
import net.minecraft.data.PackOutput;
import net.minecraft.resources.ResourceLocation;
import net.neoforged.neoforge.common.data.ExistingFileHelper;
import net.neoforged.neoforge.common.data.SoundDefinitionsProvider;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;
import static mrtjp.projectred.expansion.init.ExpansionSounds.DEPRESSURIZE;
import static mrtjp.projectred.expansion.init.ExpansionSounds.PRESSURIZE;

public class ExpansionSoundProvider extends SoundDefinitionsProvider {

    // Sound files
    private static final ResourceLocation PRESSURIZE_1 = ResourceLocation.fromNamespaceAndPath(MOD_ID, "pressurize_1");
    private static final ResourceLocation PRESSURIZE_2 = ResourceLocation.fromNamespaceAndPath(MOD_ID, "pressurize_2");
    private static final ResourceLocation PRESSURIZE_3 = ResourceLocation.fromNamespaceAndPath(MOD_ID, "pressurize_3");
    private static final ResourceLocation DEPRESSURIZE_1 = ResourceLocation.fromNamespaceAndPath(MOD_ID, "depressurize_1");
    private static final ResourceLocation DEPRESSURIZE_2 = ResourceLocation.fromNamespaceAndPath(MOD_ID, "depressurize_2");
    private static final ResourceLocation DEPRESSURIZE_3 = ResourceLocation.fromNamespaceAndPath(MOD_ID, "depressurize_3");

    public ExpansionSoundProvider(PackOutput output, ExistingFileHelper helper) {
        super(output, MOD_ID, helper);
    }

    @Override
    public String getName() {
        return "ProjectRed-Expansion Sound Definitions";
    }

    @Override
    public void registerSounds() {

        add(PRESSURIZE, definition()
                .subtitle(ExpansionUnlocal.UL_SUBTITLE_PRESSURIZE)
                .with(
                        sound(PRESSURIZE_1).volume(0.15).pitch(1.0),
                        sound(PRESSURIZE_1).volume(0.15).pitch(0.9),
                        sound(PRESSURIZE_1).volume(0.15).pitch(0.8),
                        sound(PRESSURIZE_2).volume(0.15).pitch(1.0),
                        sound(PRESSURIZE_2).volume(0.15).pitch(0.9),
                        sound(PRESSURIZE_2).volume(0.15).pitch(0.8),
                        sound(PRESSURIZE_3).volume(0.15).pitch(1.0),
                        sound(PRESSURIZE_3).volume(0.15).pitch(0.9),
                        sound(PRESSURIZE_3).volume(0.15).pitch(0.8)
                )
        );

        add(DEPRESSURIZE, definition()
                .subtitle(ExpansionUnlocal.UL_SUBTITLE_DEPRESSURIZE)
                .with(
                        sound(DEPRESSURIZE_1).volume(0.15).pitch(1.0),
                        sound(DEPRESSURIZE_1).volume(0.15).pitch(0.9),
                        sound(DEPRESSURIZE_1).volume(0.15).pitch(0.8),
                        sound(DEPRESSURIZE_2).volume(0.15).pitch(1.0),
                        sound(DEPRESSURIZE_2).volume(0.15).pitch(0.9),
                        sound(DEPRESSURIZE_2).volume(0.15).pitch(0.8),
                        sound(DEPRESSURIZE_3).volume(0.15).pitch(1.0),
                        sound(DEPRESSURIZE_3).volume(0.15).pitch(0.9),
                        sound(DEPRESSURIZE_3).volume(0.15).pitch(0.8)
                )
        );
    }
}
