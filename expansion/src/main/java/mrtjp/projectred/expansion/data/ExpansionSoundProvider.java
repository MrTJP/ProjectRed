package mrtjp.projectred.expansion.data;

import mrtjp.projectred.expansion.init.ExpansionUnlocal;
import net.minecraft.data.DataGenerator;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.common.data.ExistingFileHelper;
import net.minecraftforge.common.data.SoundDefinitionsProvider;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;
import static mrtjp.projectred.expansion.init.ExpansionSounds.DEPRESSURIZE;
import static mrtjp.projectred.expansion.init.ExpansionSounds.PRESSURIZE;

public class ExpansionSoundProvider extends SoundDefinitionsProvider {

    public ExpansionSoundProvider(DataGenerator generator, ExistingFileHelper helper) {
        super(generator, MOD_ID, helper);
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
                        sound(new ResourceLocation(MOD_ID, "pressurize_1"))
                                .volume(0.8F),
                        sound(new ResourceLocation(MOD_ID, "pressurize_2"))
                                .volume(0.8F),
                        sound(new ResourceLocation(MOD_ID, "pressurize_3"))
                                .volume(0.8F)
                )
        );

        add(DEPRESSURIZE, definition()
                .subtitle(ExpansionUnlocal.UL_SUBTITLE_DEPRESSURIZE)
                .with(
                        sound(new ResourceLocation(MOD_ID, "depressurize_1"))
                                .volume(0.8F),
                        sound(new ResourceLocation(MOD_ID, "depressurize_2"))
                                .volume(0.8F),
                        sound(new ResourceLocation(MOD_ID, "depressurize_3"))
                                .volume(0.8F)
                )
        );
    }
}
