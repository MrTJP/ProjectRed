package mrtjp.projectred.expansion.init;

import mrtjp.projectred.expansion.ProjectRedExpansion;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.sounds.SoundEvent;

import java.util.function.Supplier;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;

@SuppressWarnings("NotNullFieldNotInitialized")
public class ExpansionSounds {

    // Sound Ids
    public static final String SOUND_PRESSURIZE = "pressurize";
    public static final String SOUND_DEPRESSURIZE = "depressurize";

    // Sound events
    public static Supplier<SoundEvent> PRESSURIZE;
    public static Supplier<SoundEvent> DEPRESSURIZE;

    public static void register() {

        PRESSURIZE = ProjectRedExpansion.SOUNDS.register(SOUND_PRESSURIZE, () -> SoundEvent.createFixedRangeEvent(new ResourceLocation(MOD_ID, SOUND_PRESSURIZE), 16F));
        DEPRESSURIZE = ProjectRedExpansion.SOUNDS.register(SOUND_DEPRESSURIZE, () -> SoundEvent.createFixedRangeEvent(new ResourceLocation(MOD_ID, SOUND_DEPRESSURIZE), 16F));
    }
}
