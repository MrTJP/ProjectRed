package mrtjp.projectred.exploration.data;

import net.minecraft.data.PackOutput;
import net.neoforged.neoforge.client.model.generators.BlockStateProvider;
import net.neoforged.neoforge.common.data.ExistingFileHelper;

import javax.annotation.Nonnull;

import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;
import static mrtjp.projectred.exploration.init.ExplorationBlocks.*;

public class ExplorationBlockStateModelProvider extends BlockStateProvider {

    public ExplorationBlockStateModelProvider(PackOutput output, ExistingFileHelper exFileHelper) {
        super(output, MOD_ID, exFileHelper);
    }

    @Nonnull
    @Override
    public String getName() {
        return "ProjectRed-Exploration Block State Models";
    }

    @Override
    protected void registerStatesAndModels() {

        // Ores
        simpleBlock(RUBY_ORE_BLOCK.get());
        simpleBlock(DEEPSLATE_RUBY_ORE_BLOCK.get());
        simpleBlock(SAPPHIRE_ORE_BLOCK.get());
        simpleBlock(DEEPSLATE_SAPPHIRE_ORE_BLOCK.get());
        simpleBlock(PERIDOT_ORE_BLOCK.get());
        simpleBlock(DEEPSLATE_PERIDOT_ORE_BLOCK.get());
        simpleBlock(TIN_ORE_BLOCK.get());
        simpleBlock(DEEPSLATE_TIN_ORE_BLOCK.get());
        simpleBlock(SILVER_ORE_BLOCK.get());
        simpleBlock(DEEPSLATE_SILVER_ORE_BLOCK.get());
        simpleBlock(ELECTROTINE_ORE_BLOCK.get());
        simpleBlock(DEEPSLATE_ELECTROTINE_ORE_BLOCK.get());

        // Decorative blocks
        simpleBlock(MARBLE_BLOCK.get());
        simpleBlock(MARBLE_BRICK_BLOCK.get());
        simpleBlock(BASALT_BLOCK.get());
        simpleBlock(BASALT_COBBLE_BLOCK.get());
        simpleBlock(BASALT_BRICK_BLOCK.get());
        simpleBlock(RUBY_BLOCK.get());
        simpleBlock(SAPPHIRE_BLOCK.get());
        simpleBlock(PERIDOT_BLOCK.get());
        simpleBlock(ELECTROTINE_BLOCK.get());
        simpleBlock(RAW_TIN_BLOCK.get());
        simpleBlock(RAW_SILVER_BLOCK.get());
        simpleBlock(TIN_BLOCK.get());
        simpleBlock(SILVER_BLOCK.get());

        // Walls
        wallBlock(MARBLE_WALL.get(), blockTexture(MARBLE_BLOCK.get()));
        wallBlock(MARBLE_BRICK_WALL.get(), blockTexture(MARBLE_BRICK_BLOCK.get()));
        wallBlock(BASALT_WALL.get(), blockTexture(BASALT_BLOCK.get()));
        wallBlock(BASALT_COBBLE_WALL.get(), blockTexture(BASALT_COBBLE_BLOCK.get()));
        wallBlock(BASALT_BRICK_WALL.get(), blockTexture(BASALT_BRICK_BLOCK.get()));
        wallBlock(RUBY_BLOCK_WALL.get(), blockTexture(RUBY_BLOCK.get()));
        wallBlock(SAPPHIRE_BLOCK_WALL.get(), blockTexture(SAPPHIRE_BLOCK.get()));
        wallBlock(PERIDOT_BLOCK_WALL.get(), blockTexture(PERIDOT_BLOCK.get()));
        wallBlock(ELECTROTINE_BLOCK_WALL.get(), blockTexture(ELECTROTINE_BLOCK.get()));
    }
}
