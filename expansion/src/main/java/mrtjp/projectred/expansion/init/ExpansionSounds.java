package mrtjp.projectred.expansion.init;

import mrtjp.projectred.expansion.ProjectRedExpansion;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.sounds.SoundEvent;
import net.minecraftforge.registries.RegistryObject;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;

@SuppressWarnings("NotNullFieldNotInitialized")
public class ExpansionSounds {

    // Sound Ids
    public static final String SOUND_PRESSURIZE = "pressurize";
    public static final String SOUND_DEPRESSURIZE = "depressurize";

    // Sound events
    public static RegistryObject<SoundEvent> PRESSURIZE;
    public static RegistryObject<SoundEvent> DEPRESSURIZE;

    public static void register() {

        PRESSURIZE = ProjectRedExpansion.SOUNDS.register(SOUND_PRESSURIZE, () -> new SoundEvent(new ResourceLocation(MOD_ID, SOUND_PRESSURIZE)));
        DEPRESSURIZE = ProjectRedExpansion.SOUNDS.register(SOUND_DEPRESSURIZE, () -> new SoundEvent(new ResourceLocation(MOD_ID, SOUND_DEPRESSURIZE)));
    }
}
