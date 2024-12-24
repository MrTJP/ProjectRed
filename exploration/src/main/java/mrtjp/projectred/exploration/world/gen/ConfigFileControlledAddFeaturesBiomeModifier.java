package mrtjp.projectred.exploration.world.gen;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.exploration.init.ExplorationWorldFeatures;
import net.minecraft.core.Holder;
import net.minecraft.core.HolderSet;
import net.minecraft.world.level.biome.Biome;
import net.minecraft.world.level.levelgen.GenerationStep;
import net.minecraft.world.level.levelgen.placement.PlacedFeature;
import net.neoforged.neoforge.common.world.BiomeGenerationSettingsBuilder;
import net.neoforged.neoforge.common.world.BiomeModifier;
import net.neoforged.neoforge.common.world.BiomeModifiers;
import net.neoforged.neoforge.common.world.ModifiableBiomeInfo;

/**
 * A type of {@link BiomeModifiers.AddFeaturesBiomeModifier} that can be disabled by its corresponding
 * field in ProjectRed's Config file.
 */
public record ConfigFileControlledAddFeaturesBiomeModifier(HolderSet<Biome> biomes, HolderSet<PlacedFeature> features, GenerationStep.Decoration step, String configKey) implements BiomeModifier {

    @Override
    public void modify(Holder<Biome> biome, BiomeModifier.Phase phase, ModifiableBiomeInfo.BiomeInfo.Builder builder) {

        if (isEnabled() && phase == Phase.ADD && this.biomes.contains(biome)) {
            BiomeGenerationSettingsBuilder generationSettings = builder.getGenerationSettings();
            this.features.forEach(holder -> generationSettings.addFeature(this.step, holder));
        }
    }

    @Override
    public Codec<? extends BiomeModifier> codec() {
        return ExplorationWorldFeatures.ADD_FEATURES_BIOME_MODIFIER_CODEC.get();
    }

    private boolean isEnabled() {
        return Configurator.worldFeatures.getOrDefault(configKey, false);
    }

    public static Codec<ConfigFileControlledAddFeaturesBiomeModifier> createCodec() {
        return RecordCodecBuilder.create(instance -> instance.group(
                Biome.LIST_CODEC.fieldOf("biomes").forGetter(m -> m.biomes),
                PlacedFeature.LIST_CODEC.fieldOf("features").forGetter(m -> m.features),
                GenerationStep.Decoration.CODEC.fieldOf("step").forGetter(m -> m.step),
                Codec.STRING.fieldOf("configKey").forGetter(m -> m.configKey)
        ).apply(instance, ConfigFileControlledAddFeaturesBiomeModifier::new));
    }
}
