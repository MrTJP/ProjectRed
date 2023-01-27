package mrtjp.projectred.exploration.data;

import net.minecraft.data.DataGenerator;
import net.minecraftforge.common.data.LanguageProvider;

import static mrtjp.projectred.ProjectRedExploration.MOD_ID;
import static mrtjp.projectred.exploration.init.ExplorationReferences.*;

public class ExplorationLanguageProvider extends LanguageProvider {

    public ExplorationLanguageProvider(DataGenerator gen) {
        super(gen, MOD_ID, "en_us");
    }

    @Override
    public String getName() {
        return "ProjectRed-Exploration Language Provider";
    }

    @Override
    protected void addTranslations() {

        // Ores
        add(RUBY_ORE_BLOCK, "Ruby Ore");
        add(SAPPHIRE_ORE_BLOCK, "Sapphire Ore");
        add(PERIDOT_ORE_BLOCK, "Peridot Ore");
        add(COPPER_ORE_BLOCK, "Copper Ore");
        add(TIN_ORE_BLOCK, "Tin Ore");
        add(SILVER_ORE_BLOCK, "Silver Ore");
        add(ELECTROTINE_ORE_BLOCK, "Electrotine Ore");

        // Decorative blocks
        add(MARBLE_BLOCK, "Marble");
        add(MARBLE_BRICK_BLOCK, "Marble Brick");
        add(BASALT_BLOCK, "Basalt");
        add(BASALT_COBBLE_BLOCK, "Basalt Cobblestone");
        add(BASALT_BRICK_BLOCK, "Basalt Brick");
        add(RUBY_BLOCK, "Ruby Block");
        add(SAPPHIRE_BLOCK, "Sapphire Block");
        add(PERIDOT_BLOCK, "Peridot Block");
        add(COPPER_BLOCK, "Copper Block");
        add(TIN_BLOCK, "Tin Block");
        add(SILVER_BLOCK, "Silver Block");
        add(ELECTROTINE_BLOCK, "Electrotine Block");

        // Walls
        add(MARBLE_WALL, "Marble Wall");
        add(MARBLE_BRICK_WALL, "Marble Brick Wall");
        add(BASALT_WALL, "Basalt Wall");
        add(BASALT_COBBLE_WALL, "Basalt Cobblestone Wall");
        add(BASALT_BRICK_WALL, "Basalt Brick Wall");
        add(RUBY_BLOCK_WALL, "Ruby Wall");
        add(SAPPHIRE_BLOCK_WALL, "Sapphire Wall");
        add(PERIDOT_BLOCK_WALL, "Peridot Wall");
        add(COPPER_BLOCK_WALL, "Copper Wall");
        add(TIN_BLOCK_WALL, "Tin Wall");
        add(SILVER_BLOCK_WALL, "Silver Wall");
        add(ELECTROTINE_BLOCK_WALL, "Electrotine Wall");
    }
}
