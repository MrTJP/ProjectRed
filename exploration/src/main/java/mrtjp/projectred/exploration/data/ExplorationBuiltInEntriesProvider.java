package mrtjp.projectred.exploration.data;

import mrtjp.projectred.exploration.init.ExplorationWorldFeatures;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.RegistrySetBuilder;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.PackOutput;
import net.minecraftforge.common.data.DatapackBuiltinEntriesProvider;
import net.minecraftforge.registries.ForgeRegistries;

import java.util.Set;
import java.util.concurrent.CompletableFuture;

import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;

public class ExplorationBuiltInEntriesProvider extends DatapackBuiltinEntriesProvider {

    public static final RegistrySetBuilder BUILDER = new RegistrySetBuilder()
            .add(Registries.CONFIGURED_CARVER, ExplorationWorldFeatures::bootstrapCarvers)
            .add(Registries.CONFIGURED_FEATURE, ExplorationWorldFeatures::bootstrapFeatures)
            .add(Registries.PLACED_FEATURE, ExplorationWorldFeatures::bootstrapPlacements)
            .add(ForgeRegistries.Keys.BIOME_MODIFIERS, ExplorationWorldFeatures::bootstrapBiomeModifiers);

    public ExplorationBuiltInEntriesProvider(PackOutput output, CompletableFuture<HolderLookup.Provider> provider) {
        super(output, provider, BUILDER, Set.of("minecraft", MOD_ID));
    }
}
