package mrtjp.projectred.exploration.init;

import com.google.common.collect.ImmutableList;
import com.google.gson.JsonElement;
import com.mojang.serialization.Codec;
import com.mojang.serialization.JsonOps;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.exploration.world.gen.AddCarversBiomeModifier;
import mrtjp.projectred.exploration.world.gen.MarbleCaveWorldCarver;
import net.minecraft.core.Holder;
import net.minecraft.core.HolderSet;
import net.minecraft.core.Registry;
import net.minecraft.core.RegistryAccess;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.worldgen.features.OreFeatures;
import net.minecraft.resources.RegistryOps;
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
import net.minecraftforge.common.data.ExistingFileHelper;
import net.minecraftforge.common.data.JsonCodecProvider;
import net.minecraftforge.common.world.BiomeModifier;
import net.minecraftforge.common.world.ForgeBiomeModifiers;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.registries.RegistryObject;

import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

import static mrtjp.projectred.exploration.ProjectRedExploration.*;
import static mrtjp.projectred.exploration.init.ExplorationBlocks.*;

@SuppressWarnings("NotNullFieldNotInitialized")
public class ExplorationWorldFeatures {

    public static final String ID_MARBLE_CAVE_CARVER = "marble_cave";

    // World carvers
    public static RegistryObject<WorldCarver<CaveCarverConfiguration>> MARBLE_CAVE_CARVER;

    // Biome Modifier Codecs
    public static RegistryObject<Codec<AddCarversBiomeModifier>> ADD_CARVER_BIOME_MODIFIER_CODEC;

    public static void register() {

        // Carvers
        MARBLE_CAVE_CARVER = WORLD_CARVERS.register(ID_MARBLE_CAVE_CARVER, () -> new MarbleCaveWorldCarver(CaveCarverConfiguration.CODEC));

        // Features
        // None yet. For custom ores, we register a configured variant of Vanilla Feature.ORE

        // Codecs (since forge doesnt provide a default "Add Carvers" biome modifier)
        ADD_CARVER_BIOME_MODIFIER_CODEC = BIOME_MODIFIER_SERIALIZERS.register("add_carver", () -> AddCarversBiomeModifier.CODEC);
    }

    public static JsonCodecProvider<BiomeModifier> biomeModifiersProvider(DataGenerator dataGenerator, ExistingFileHelper existingFileHelper) {

        final RegistryOps<JsonElement> ops = RegistryOps.create(JsonOps.INSTANCE, RegistryAccess.builtinCopy());
        final HolderSet.Named<Biome> isOverworldBiomes = new HolderSet.Named<>(ops.registry(Registry.BIOME_REGISTRY).get(), BiomeTags.IS_OVERWORLD);
        final HolderSet.Named<Block> overworldCarverReplaceables = new HolderSet.Named<>(ops.registry(Registry.BLOCK_REGISTRY).get(), BlockTags.OVERWORLD_CARVER_REPLACEABLES);

        // Configured carvers
        ConfiguredWorldCarver<CaveCarverConfiguration> marbleCaveConfiguredCarver = MARBLE_CAVE_CARVER.get().configured(new CaveCarverConfiguration(
                0.01F, // probability
                UniformHeight.of(VerticalAnchor.aboveBottom(8), VerticalAnchor.absolute(180)), // Max/min heights
                UniformFloat.of(0.1F, 0.9F), // y scale
                VerticalAnchor.aboveBottom(8), // lava level
                CarverDebugSettings.of(false, Blocks.CRIMSON_BUTTON.defaultBlockState()), // debug settings (enable, air state)
                overworldCarverReplaceables,
                UniformFloat.of(0.7F, 1.4F), // horizontal radius
                UniformFloat.of(0.8F, 1.3F), // vertical radius
                UniformFloat.of(-1.0F, -0.4F))); // floor level

        // Configured ores
        ConfiguredFeature<OreConfiguration, ?> rubyOreConfiguration          = createOreConfiguration(RUBY_ORE_BLOCK, DEEPSLATE_RUBY_ORE_BLOCK,         Configurator.gen_RubyVeinSize);
        ConfiguredFeature<OreConfiguration, ?> sapphireOreConfiguration      = createOreConfiguration(SAPPHIRE_ORE_BLOCK, DEEPSLATE_SAPPHIRE_ORE_BLOCK, Configurator.gen_SapphireVeinSize);
        ConfiguredFeature<OreConfiguration, ?> peridotOreConfiguration       = createOreConfiguration(PERIDOT_ORE_BLOCK, DEEPSLATE_PERIDOT_ORE_BLOCK,   Configurator.gen_PeridotVeinSize);
        ConfiguredFeature<OreConfiguration, ?> electrotineOreConfiguration   = createOreConfiguration(ELECTROTINE_ORE_BLOCK, DEEPSLATE_ELECTROTINE_ORE_BLOCK, Configurator.gen_ElectrotineVeinSize);
        ConfiguredFeature<OreConfiguration, ?> tinOreConfiguration           = createOreConfiguration(TIN_ORE_BLOCK, DEEPSLATE_TIN_ORE_BLOCK,           Configurator.gen_TinVeinSize);
        ConfiguredFeature<OreConfiguration, ?> silverOreConfiguration        = createOreConfiguration(SILVER_ORE_BLOCK, DEEPSLATE_SILVER_ORE_BLOCK,     Configurator.gen_SilverVeinSize);

        // Placements
        PlacedFeature rubyOrePlacedFeature         = createOrePlacement(rubyOreConfiguration,        -80, 80, 1);
        PlacedFeature sapphireOrePlacedFeature     = createOrePlacement(sapphireOreConfiguration,    -80, 80, 1);
        PlacedFeature peridotOrePlacedFeature      = createOrePlacement(peridotOreConfiguration,     -80, 80, 1);
        PlacedFeature electrotineOrePlacedFeature  = createOrePlacement(electrotineOreConfiguration, -32, 32, 4);
        PlacedFeature tinOrePlacedFeature          = createOrePlacement(tinOreConfiguration,         -24, 56, 8);
        PlacedFeature silverOrePlacedFeature       = createOrePlacement(silverOreConfiguration,      -64, 32, 6);

        return JsonCodecProvider.forDatapackRegistry(
                dataGenerator,
                existingFileHelper,
                MOD_ID,
                ops,
                ForgeRegistries.Keys.BIOME_MODIFIERS,
                Map.of(
                        new ResourceLocation(MOD_ID, "add_overworld_ores"), new ForgeBiomeModifiers.AddFeaturesBiomeModifier(
                                isOverworldBiomes,
                                HolderSet.direct(
                                        Holder.direct(rubyOrePlacedFeature),
                                        Holder.direct(sapphireOrePlacedFeature),
                                        Holder.direct(peridotOrePlacedFeature),
                                        Holder.direct(electrotineOrePlacedFeature),
                                        Holder.direct(tinOrePlacedFeature),
                                        Holder.direct(silverOrePlacedFeature)
                                ),
                                GenerationStep.Decoration.UNDERGROUND_ORES),
                        new ResourceLocation(MOD_ID, "add_overworld_carvers"), new AddCarversBiomeModifier(
                                isOverworldBiomes,
                                HolderSet.direct(Holder.direct(marbleCaveConfiguredCarver)),
                                GenerationStep.Carving.AIR)
                ));
    }

    // Registers the actual ore feature. This describes a single cluster of this specific ore type
    private static ConfiguredFeature<OreConfiguration, ?> createOreConfiguration(Supplier<Block> standard,  Supplier<Block> deepslate, int veinSize) {
        return new ConfiguredFeature<>(Feature.ORE, new OreConfiguration(ImmutableList.of(
                OreConfiguration.target(OreFeatures.STONE_ORE_REPLACEABLES, standard.get().defaultBlockState()),
                OreConfiguration.target(OreFeatures.DEEPSLATE_ORE_REPLACEABLES, deepslate.get().defaultBlockState())), veinSize));
    }

    // Registers a placement for the given feature. This controls how many of said features spawn and where
    private static PlacedFeature createOrePlacement(ConfiguredFeature<OreConfiguration, ?> configuredFeature, int minY, int maxY, int count) {
        List<PlacementModifier> modifiers = ImmutableList.of(
                CountPlacement.of(count),
                InSquarePlacement.spread(),
                HeightRangePlacement.triangle(VerticalAnchor.absolute(minY), VerticalAnchor.absolute(maxY)));

        return new PlacedFeature(Holder.direct(configuredFeature), modifiers);
    }
}
