package mrtjp.projectred.exploration.init;

import com.google.common.collect.ImmutableList;
import com.mojang.serialization.Codec;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.exploration.world.gen.ConfigFileControlledAddCarversBiomeModifier;
import mrtjp.projectred.exploration.world.gen.ConfigFileControlledAddFeaturesBiomeModifier;
import mrtjp.projectred.exploration.world.gen.MarbleCaveWorldCarver;
import net.minecraft.core.Holder;
import net.minecraft.core.HolderGetter;
import net.minecraft.core.HolderSet;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.worldgen.BootstapContext;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.BiomeTags;
import net.minecraft.tags.BlockTags;
import net.minecraft.util.valueproviders.UniformFloat;
import net.minecraft.world.level.biome.Biome;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.levelgen.GenerationStep;
import net.minecraft.world.level.levelgen.VerticalAnchor;
import net.minecraft.world.level.levelgen.carver.CarverDebugSettings;
import net.minecraft.world.level.levelgen.carver.CaveCarverConfiguration;
import net.minecraft.world.level.levelgen.carver.ConfiguredWorldCarver;
import net.minecraft.world.level.levelgen.carver.WorldCarver;
import net.minecraft.world.level.levelgen.feature.ConfiguredFeature;
import net.minecraft.world.level.levelgen.feature.Feature;
import net.minecraft.world.level.levelgen.feature.configurations.OreConfiguration;
import net.minecraft.world.level.levelgen.heightproviders.UniformHeight;
import net.minecraft.world.level.levelgen.placement.*;
import net.minecraft.world.level.levelgen.structure.templatesystem.TagMatchTest;
import net.minecraftforge.common.world.BiomeModifier;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.registries.RegistryObject;

import java.util.List;
import java.util.function.Supplier;

import static mrtjp.projectred.exploration.ProjectRedExploration.*;
import static mrtjp.projectred.exploration.init.ExplorationBlocks.*;

@SuppressWarnings("NotNullFieldNotInitialized")
public class ExplorationWorldFeatures {

    public static final String ID_MARBLE_CAVE_CARVER = "marble_cave";

    /* Static registry entries */
    // World carvers
    public static RegistryObject<WorldCarver<CaveCarverConfiguration>> MARBLE_CAVE_CARVER;

    // Biome Modifier Codecs
    public static RegistryObject<Codec<ConfigFileControlledAddCarversBiomeModifier>> ADD_CARVER_BIOME_MODIFIER_CODEC;
    public static RegistryObject<Codec<ConfigFileControlledAddFeaturesBiomeModifier>> ADD_FEATURES_BIOME_MODIFIER_CODEC;

    /* Dynamic registry entries */
    // Configured carvers
    public static ResourceKey<ConfiguredWorldCarver<?>> MARBLE_CAVE_CONFIGURED_CARVER = createCarverKey(ID_MARBLE_CAVE_CARVER);

    // Configured features
    public static ResourceKey<ConfiguredFeature<?, ?>> RUBY_ORE_CONFIGURED_FEATURE = createFeatureKey("ruby_ore");
    public static ResourceKey<ConfiguredFeature<?, ?>> SAPPHIRE_ORE_CONFIGURED_FEATURE = createFeatureKey("sapphire_ore");
    public static ResourceKey<ConfiguredFeature<?, ?>> PERIDOT_ORE_CONFIGURED_FEATURE = createFeatureKey("peridot_ore");
    public static ResourceKey<ConfiguredFeature<?, ?>> ELECTROTINE_ORE_CONFIGURED_FEATURE = createFeatureKey("electrotine_ore");
    public static ResourceKey<ConfiguredFeature<?, ?>> TIN_ORE_CONFIGURED_FEATURE = createFeatureKey("tin_ore");
    public static ResourceKey<ConfiguredFeature<?, ?>> SILVER_ORE_CONFIGURED_FEATURE = createFeatureKey("silver_ore");

    // Placed features
    public static ResourceKey<PlacedFeature> RUBY_ORE_PLACED_FEATURE = createPlacedFeatureKey("ruby_ore");
    public static ResourceKey<PlacedFeature> SAPPHIRE_ORE_PLACED_FEATURE = createPlacedFeatureKey("sapphire_ore");
    public static ResourceKey<PlacedFeature> PERIDOT_ORE_PLACED_FEATURE = createPlacedFeatureKey("peridot_ore");
    public static ResourceKey<PlacedFeature> ELECTROTINE_ORE_PLACED_FEATURE = createPlacedFeatureKey("electrotine_ore");
    public static ResourceKey<PlacedFeature> TIN_ORE_PLACED_FEATURE = createPlacedFeatureKey("tin_ore");
    public static ResourceKey<PlacedFeature> SILVER_ORE_PLACED_FEATURE = createPlacedFeatureKey("silver_ore");

    public static void register() {

        // Carvers
        MARBLE_CAVE_CARVER = WORLD_CARVERS.register(ID_MARBLE_CAVE_CARVER, () -> new MarbleCaveWorldCarver(CaveCarverConfiguration.CODEC));

        // Features
        // None yet. For custom ores, we register a configured variant of Vanilla Feature.ORE

        // Codecs
        ADD_CARVER_BIOME_MODIFIER_CODEC = BIOME_MODIFIER_SERIALIZERS.register("add_carver", ConfigFileControlledAddCarversBiomeModifier::createCodec);
        ADD_FEATURES_BIOME_MODIFIER_CODEC = BIOME_MODIFIER_SERIALIZERS.register("add_features", ConfigFileControlledAddFeaturesBiomeModifier::createCodec);
    }

    public static ResourceKey<ConfiguredWorldCarver<?>> createCarverKey(String name) {
        return ResourceKey.create(Registries.CONFIGURED_CARVER, new ResourceLocation(MOD_ID, name));
    }

    public static ResourceKey<ConfiguredFeature<?, ?>> createFeatureKey(String name) {
        return ResourceKey.create(Registries.CONFIGURED_FEATURE, new ResourceLocation(MOD_ID, name));
    }

    public static ResourceKey<PlacedFeature> createPlacedFeatureKey(String name) {
        return ResourceKey.create(Registries.PLACED_FEATURE, new ResourceLocation(MOD_ID, name));
    }

    public static ResourceKey<BiomeModifier> createBiomeModifierKey(String name) {
        return ResourceKey.create(ForgeRegistries.Keys.BIOME_MODIFIERS, new ResourceLocation(MOD_ID, name));
    }

    public static void bootstrapCarvers(BootstapContext<ConfiguredWorldCarver<?>> context) {

        HolderGetter<Block> blockGetter = context.lookup(Registries.BLOCK);

        context.register(MARBLE_CAVE_CONFIGURED_CARVER, MARBLE_CAVE_CARVER.get().configured(new CaveCarverConfiguration(
                0.01F, // probability
                UniformHeight.of(VerticalAnchor.aboveBottom(8), VerticalAnchor.absolute(180)), // Max/min heights
                UniformFloat.of(0.1F, 0.9F), // y scale
                VerticalAnchor.aboveBottom(8), // lava level
                CarverDebugSettings.of(false, Blocks.CRIMSON_BUTTON.defaultBlockState()), // debug settings (enable, air state)
                blockGetter.getOrThrow(BlockTags.OVERWORLD_CARVER_REPLACEABLES),
                UniformFloat.of(0.7F, 1.4F), // horizontal radius
                UniformFloat.of(0.8F, 1.3F), // vertical radius
                UniformFloat.of(-1.0F, -0.4F))));
    }

    public static void bootstrapFeatures(BootstapContext<ConfiguredFeature<?, ?>> context) {

        registerOreConfiguration(context, RUBY_ORE_CONFIGURED_FEATURE,        RUBY_ORE_BLOCK, DEEPSLATE_RUBY_ORE_BLOCK,         8);
        registerOreConfiguration(context, SAPPHIRE_ORE_CONFIGURED_FEATURE,    SAPPHIRE_ORE_BLOCK, DEEPSLATE_SAPPHIRE_ORE_BLOCK, 8);
        registerOreConfiguration(context, PERIDOT_ORE_CONFIGURED_FEATURE,     PERIDOT_ORE_BLOCK, DEEPSLATE_PERIDOT_ORE_BLOCK,   10);
        registerOreConfiguration(context, ELECTROTINE_ORE_CONFIGURED_FEATURE, ELECTROTINE_ORE_BLOCK, DEEPSLATE_ELECTROTINE_ORE_BLOCK, 8);
        registerOreConfiguration(context, TIN_ORE_CONFIGURED_FEATURE,         TIN_ORE_BLOCK, DEEPSLATE_TIN_ORE_BLOCK,           8);
        registerOreConfiguration(context, SILVER_ORE_CONFIGURED_FEATURE,      SILVER_ORE_BLOCK, DEEPSLATE_SILVER_ORE_BLOCK,     9);
    }

    public static void bootstrapPlacements(BootstapContext<PlacedFeature> context) {

        HolderGetter<ConfiguredFeature<?, ?>> features = context.lookup(Registries.CONFIGURED_FEATURE);

        registerOrePlacement(context, RUBY_ORE_PLACED_FEATURE,        features.getOrThrow(RUBY_ORE_CONFIGURED_FEATURE),        -80, 80, 1);
        registerOrePlacement(context, SAPPHIRE_ORE_PLACED_FEATURE,    features.getOrThrow(SAPPHIRE_ORE_CONFIGURED_FEATURE),    -80, 80, 1);
        registerOrePlacement(context, PERIDOT_ORE_PLACED_FEATURE,     features.getOrThrow(PERIDOT_ORE_CONFIGURED_FEATURE),     -80, 80, 1);
        registerOrePlacement(context, ELECTROTINE_ORE_PLACED_FEATURE, features.getOrThrow(ELECTROTINE_ORE_CONFIGURED_FEATURE), -32, 32, 4);
        registerOrePlacement(context, TIN_ORE_PLACED_FEATURE,         features.getOrThrow(TIN_ORE_CONFIGURED_FEATURE),         -24, 56, 8);
        registerOrePlacement(context, SILVER_ORE_PLACED_FEATURE,      features.getOrThrow(SILVER_ORE_CONFIGURED_FEATURE),      -64, 32, 6);
    }

    public static void bootstrapBiomeModifiers(BootstapContext<BiomeModifier> context) {

        HolderGetter<Biome> biomes = context.lookup(Registries.BIOME);
        HolderGetter<ConfiguredWorldCarver<?>> worldCarvers = context.lookup(Registries.CONFIGURED_CARVER);
        HolderGetter<PlacedFeature> placedFeatures = context.lookup(Registries.PLACED_FEATURE);

        HolderSet<Biome> overworldBiomes = biomes.getOrThrow(BiomeTags.IS_OVERWORLD);

        // Add overworld ores
        context.register(createBiomeModifierKey("add_ruby_ore_to_overworld"), new ConfigFileControlledAddFeaturesBiomeModifier(overworldBiomes, HolderSet.direct(placedFeatures.getOrThrow(RUBY_ORE_PLACED_FEATURE)), GenerationStep.Decoration.UNDERGROUND_ORES, Configurator.rubyOreKey));
        context.register(createBiomeModifierKey("add_sapphire_ore_to_overworld"), new ConfigFileControlledAddFeaturesBiomeModifier(overworldBiomes, HolderSet.direct(placedFeatures.getOrThrow(SAPPHIRE_ORE_PLACED_FEATURE)), GenerationStep.Decoration.UNDERGROUND_ORES, Configurator.sapphireOreKey));
        context.register(createBiomeModifierKey("add_peridot_ore_to_overworld"), new ConfigFileControlledAddFeaturesBiomeModifier(overworldBiomes, HolderSet.direct(placedFeatures.getOrThrow(PERIDOT_ORE_PLACED_FEATURE)), GenerationStep.Decoration.UNDERGROUND_ORES, Configurator.peridotOreKey));
        context.register(createBiomeModifierKey("add_electrotine_ore_to_overworld"), new ConfigFileControlledAddFeaturesBiomeModifier(overworldBiomes, HolderSet.direct(placedFeatures.getOrThrow(ELECTROTINE_ORE_PLACED_FEATURE)), GenerationStep.Decoration.UNDERGROUND_ORES, Configurator.electrotineOreKey));
        context.register(createBiomeModifierKey("add_tin_ore_to_overworld"), new ConfigFileControlledAddFeaturesBiomeModifier(overworldBiomes, HolderSet.direct(placedFeatures.getOrThrow(TIN_ORE_PLACED_FEATURE)), GenerationStep.Decoration.UNDERGROUND_ORES, Configurator.tinOreKey));
        context.register(createBiomeModifierKey("add_silver_ore_to_overworld"), new ConfigFileControlledAddFeaturesBiomeModifier(overworldBiomes, HolderSet.direct(placedFeatures.getOrThrow(SILVER_ORE_PLACED_FEATURE)), GenerationStep.Decoration.UNDERGROUND_ORES, Configurator.silverOreKey));

        // Add overworld carvers
        context.register(createBiomeModifierKey("add_marble_cave_to_overworld"), new ConfigFileControlledAddCarversBiomeModifier(overworldBiomes, HolderSet.direct(worldCarvers.getOrThrow(MARBLE_CAVE_CONFIGURED_CARVER)), GenerationStep.Carving.AIR, Configurator.marbleCaveKey));
    }

    // Registers the actual ore feature. This describes a single cluster of this specific ore type
    private static void registerOreConfiguration(BootstapContext<ConfiguredFeature<?, ?>> context, ResourceKey<ConfiguredFeature<?, ?>> key, Supplier<Block> standard,  Supplier<Block> deepslate, int veinSize) {
        context.register(key, new ConfiguredFeature<>(Feature.ORE, new OreConfiguration(ImmutableList.of(
                OreConfiguration.target(new TagMatchTest(BlockTags.STONE_ORE_REPLACEABLES), standard.get().defaultBlockState()),
                OreConfiguration.target(new TagMatchTest(BlockTags.STONE_ORE_REPLACEABLES), deepslate.get().defaultBlockState())), veinSize)));
    }

    // Registers a placement for the given feature. This controls how many of said features spawn and where
    private static void registerOrePlacement(BootstapContext<PlacedFeature> context, ResourceKey<PlacedFeature> key, Holder<ConfiguredFeature<?, ?>> configuredFeature, int minY, int maxY, int count) {
        List<PlacementModifier> modifiers = ImmutableList.of(
                CountPlacement.of(count),
                InSquarePlacement.spread(),
                HeightRangePlacement.triangle(VerticalAnchor.absolute(minY), VerticalAnchor.absolute(maxY)));

        context.register(key, new PlacedFeature(configuredFeature, modifiers));
    }
}
