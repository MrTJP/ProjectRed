package mrtjp.projectred.exploration.init;

import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.exploration.world.gen.MarbleCaveWorldCarver;
import net.minecraft.block.Block;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.registry.Registry;
import net.minecraft.util.registry.WorldGenRegistries;
import net.minecraft.world.biome.Biome;
import net.minecraft.world.gen.GenerationStage;
import net.minecraft.world.gen.carver.ConfiguredCarver;
import net.minecraft.world.gen.carver.WorldCarver;
import net.minecraft.world.gen.feature.ConfiguredFeature;
import net.minecraft.world.gen.feature.Feature;
import net.minecraft.world.gen.feature.OreFeatureConfig;
import net.minecraft.world.gen.feature.ProbabilityConfig;
import net.minecraft.world.gen.placement.Placement;
import net.minecraft.world.gen.placement.TopSolidRangeConfig;
import net.minecraftforge.common.world.BiomeGenerationSettingsBuilder;
import net.minecraftforge.event.world.BiomeLoadingEvent;
import net.minecraftforge.fml.RegistryObject;

import static mrtjp.projectred.ProjectRedExploration.MOD_ID;
import static mrtjp.projectred.ProjectRedExploration.WORLD_CARVERS;
import static mrtjp.projectred.exploration.init.ExplorationReferences.*;
import static net.minecraft.world.biome.Biome.Category.*;

public class ExplorationWorldFeatures {

    public static final String ID_MARBLE_CAVE_CARVER = "marble_cave";

    // Marble Cave world carver
    private static RegistryObject<WorldCarver<ProbabilityConfig>> MARBLE_CAVE_CARVER;

    // Ore features
    public static ConfiguredFeature<?, ?> RUBY_ORE_FEATURE;
    public static ConfiguredFeature<?, ?> SAPPHIRE_ORE_FEATURE;
    public static ConfiguredFeature<?, ?> PERIDOT_ORE_FEATURE;
    public static ConfiguredFeature<?, ?> COPPER_ORE_FEATURE;
    public static ConfiguredFeature<?, ?> TIN_ORE_FEATURE;
    public static ConfiguredFeature<?, ?> SILVER_ORE_FEATURE;
    public static ConfiguredFeature<?, ?> ELECTROTINE_ORE_FEATURE;

    // Carver features
    private static ConfiguredCarver<?> MARBLE_CAVE_FEATURE;

    public static void register() {

        MARBLE_CAVE_CARVER = WORLD_CARVERS.register(ID_MARBLE_CAVE_CARVER, () -> new MarbleCaveWorldCarver(ProbabilityConfig.CODEC, 256));
    }

    public static void load() {
        // Load configurations of Ore features and world carvers. Happens during common init because there is no Forge registry for this

        RUBY_ORE_FEATURE        = registerOreConfiguration(RUBY_ORE_BLOCK,          Configurator.gen_RubyVeinSize,        12, 20, 1);
        SAPPHIRE_ORE_FEATURE    = registerOreConfiguration(SAPPHIRE_ORE_BLOCK,      Configurator.gen_SapphireVeinSize,    12, 20, 1);
        PERIDOT_ORE_FEATURE     = registerOreConfiguration(PERIDOT_ORE_BLOCK,       Configurator.gen_PeridotVeinSize,     18, 26, 1);

        COPPER_ORE_FEATURE      = registerOreConfiguration(COPPER_ORE_BLOCK,        Configurator.gen_CopperVeinSize,      0, 64, 16);
        TIN_ORE_FEATURE         = registerOreConfiguration(TIN_ORE_BLOCK,           Configurator.gen_TinVeinSize,         0, 48, 10);
        SILVER_ORE_FEATURE      = registerOreConfiguration(SILVER_ORE_BLOCK,        Configurator.gen_SilverVeinSize,      0, 32, 8);

        ELECTROTINE_ORE_FEATURE = registerOreConfiguration(ELECTROTINE_ORE_BLOCK,   Configurator.gen_ElectrotineVeinSize, 0, 16, 4);

        MARBLE_CAVE_FEATURE = registerMarbleCaveCarverConfiguration(0.02F);
    }

    public static void onBiomeLoadingEvent(final BiomeLoadingEvent event) {
        Biome.Category category = event.getCategory();
        BiomeGenerationSettingsBuilder builder = event.getGeneration();

        if (category != NONE && category != THEEND && category != NETHER) { // if overworld

            if (Configurator.gen_Ruby)        builder.addFeature(GenerationStage.Decoration.UNDERGROUND_ORES, RUBY_ORE_FEATURE);
            if (Configurator.gen_Sapphire)    builder.addFeature(GenerationStage.Decoration.UNDERGROUND_ORES, SAPPHIRE_ORE_FEATURE);
            if (Configurator.gen_Peridot)     builder.addFeature(GenerationStage.Decoration.UNDERGROUND_ORES, PERIDOT_ORE_FEATURE);
            if (Configurator.gen_Copper)      builder.addFeature(GenerationStage.Decoration.UNDERGROUND_ORES, COPPER_ORE_FEATURE);
            if (Configurator.gen_Tin)         builder.addFeature(GenerationStage.Decoration.UNDERGROUND_ORES, TIN_ORE_FEATURE);
            if (Configurator.gen_Silver)      builder.addFeature(GenerationStage.Decoration.UNDERGROUND_ORES, SILVER_ORE_FEATURE);
            if (Configurator.gen_Electrotine) builder.addFeature(GenerationStage.Decoration.UNDERGROUND_ORES, ELECTROTINE_ORE_FEATURE);

            if (Configurator.gen_MarbleCave)  builder.addCarver(GenerationStage.Carving.AIR, MARBLE_CAVE_FEATURE);
        }
    }

    private static ConfiguredFeature<?, ?> registerOreConfiguration(Block block, int veinSize, int minY, int maxY, int count) {
        ConfiguredFeature<?, ?> configuredFeature = Feature.ORE.configured(new OreFeatureConfig(OreFeatureConfig.FillerBlockType.NATURAL_STONE, block.defaultBlockState(), veinSize))
                .squared()
                .count(count)
                .decorated(Placement.RANGE.configured(new TopSolidRangeConfig(minY, minY, maxY)));

        return Registry.register(
                WorldGenRegistries.CONFIGURED_FEATURE,
                new ResourceLocation(MOD_ID, "oregen_" + block.getRegistryName().getPath()),
                configuredFeature);
    }

    private static ConfiguredCarver<?> registerMarbleCaveCarverConfiguration(float probability) {
        ConfiguredCarver<?> configuredCarver = MARBLE_CAVE_CARVER.get().configured(new ProbabilityConfig(probability));

        return Registry.register(WorldGenRegistries.CONFIGURED_CARVER,
                new ResourceLocation(MOD_ID, ID_MARBLE_CAVE_CARVER), configuredCarver);
    }
}
