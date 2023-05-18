package mrtjp.projectred.exploration.data;

import net.minecraft.data.DataGenerator;
import net.minecraftforge.client.model.generators.BlockStateProvider;
import net.minecraftforge.common.data.ExistingFileHelper;

import javax.annotation.Nonnull;

import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;
import static mrtjp.projectred.exploration.init.ExplorationReferences.*;

public class ExplorationBlockStateModelProvider extends BlockStateProvider {

    public ExplorationBlockStateModelProvider(DataGenerator gen, ExistingFileHelper exFileHelper) {
        super(gen, MOD_ID, exFileHelper);
    }

    @Nonnull
    @Override
    public String getName() {
        return "ProjectRed-Exploration Block State Models";
    }

    @Override
    protected void registerStatesAndModels() {

        // Ores
        simpleBlock(RUBY_ORE_BLOCK);
        simpleBlock(SAPPHIRE_ORE_BLOCK);
        simpleBlock(PERIDOT_ORE_BLOCK);
        simpleBlock(COPPER_ORE_BLOCK);
        simpleBlock(TIN_ORE_BLOCK);
        simpleBlock(SILVER_ORE_BLOCK);
        simpleBlock(ELECTROTINE_ORE_BLOCK);

        // Decorative blocks
        simpleBlock(MARBLE_BLOCK);
        simpleBlock(MARBLE_BRICK_BLOCK);
        simpleBlock(BASALT_BLOCK);
        simpleBlock(BASALT_COBBLE_BLOCK);
        simpleBlock(BASALT_BRICK_BLOCK);
        simpleBlock(RUBY_BLOCK);
        simpleBlock(SAPPHIRE_BLOCK);
        simpleBlock(PERIDOT_BLOCK);
        simpleBlock(COPPER_BLOCK);
        simpleBlock(TIN_BLOCK);
        simpleBlock(SILVER_BLOCK);
        simpleBlock(ELECTROTINE_BLOCK);

        // Walls
        wallBlock(MARBLE_WALL, blockTexture(MARBLE_BLOCK));
        wallBlock(MARBLE_BRICK_WALL, blockTexture(MARBLE_BRICK_BLOCK));
        wallBlock(BASALT_WALL, blockTexture(BASALT_BLOCK));
        wallBlock(BASALT_COBBLE_WALL, blockTexture(BASALT_COBBLE_BLOCK));
        wallBlock(BASALT_BRICK_WALL, blockTexture(BASALT_BRICK_BLOCK));
        wallBlock(RUBY_BLOCK_WALL, blockTexture(RUBY_BLOCK));
        wallBlock(SAPPHIRE_BLOCK_WALL, blockTexture(SAPPHIRE_BLOCK));
        wallBlock(PERIDOT_BLOCK_WALL, blockTexture(PERIDOT_BLOCK));
        wallBlock(COPPER_BLOCK_WALL, blockTexture(COPPER_BLOCK));
        wallBlock(TIN_BLOCK_WALL, blockTexture(TIN_BLOCK));
        wallBlock(SILVER_BLOCK_WALL, blockTexture(SILVER_BLOCK));
        wallBlock(ELECTROTINE_BLOCK_WALL, blockTexture(ELECTROTINE_BLOCK));
    }
}
