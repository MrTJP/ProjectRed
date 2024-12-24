package mrtjp.projectred.exploration.world.gen;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.exploration.init.ExplorationWorldFeatures;
import net.minecraft.core.Holder;
import net.minecraft.core.HolderSet;
import net.minecraft.world.level.biome.Biome;
import net.minecraft.world.level.levelgen.GenerationStep;
import net.minecraft.world.level.levelgen.carver.ConfiguredWorldCarver;
import net.neoforged.neoforge.common.world.BiomeGenerationSettingsBuilder;
import net.neoforged.neoforge.common.world.BiomeModifier;
import net.neoforged.neoforge.common.world.BiomeModifiers;
import net.neoforged.neoforge.common.world.ModifiableBiomeInfo;

/**
 * For some reason, Forge does not provide this in {@link BiomeModifiers}. This one also allows for
 * dynamic control via ProjectRed's mod config file.
 */
public record ConfigFileControlledAddCarversBiomeModifier(HolderSet<Biome> biomes, HolderSet<ConfiguredWorldCarver<?>> carvers, GenerationStep.Carving step, String configKey) implements BiomeModifier {

    @Override
    public void modify(Holder<Biome> biome, BiomeModifier.Phase phase, ModifiableBiomeInfo.BiomeInfo.Builder builder) {

        if (isEnabled() && phase == Phase.ADD && this.biomes.contains(biome)) {
            BiomeGenerationSettingsBuilder generationSettings = builder.getGenerationSettings();
            this.carvers.forEach(holder -> generationSettings.addCarver(this.step, holder));
        }
    }

    @Override
    public Codec<? extends BiomeModifier> codec() {
        return ExplorationWorldFeatures.ADD_CARVER_BIOME_MODIFIER_CODEC.get();
    }

    private boolean isEnabled() {
        return Configurator.worldFeatures.getOrDefault(configKey, false);
    }

    public static Codec<ConfigFileControlledAddCarversBiomeModifier> createCodec() {
        return RecordCodecBuilder.create(instance -> instance.group(
                Biome.LIST_CODEC.fieldOf("biomes").forGetter(m -> m.biomes),
                ConfiguredWorldCarver.LIST_CODEC.fieldOf("carvers").forGetter(m -> m.carvers),
                GenerationStep.Carving.CODEC.fieldOf("step").forGetter(m -> m.step),
                Codec.STRING.fieldOf("configKey").forGetter(m -> m.configKey)
        ).apply(instance, ConfigFileControlledAddCarversBiomeModifier::new));
    }
}
