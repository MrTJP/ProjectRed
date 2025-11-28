package mrtjp.projectred.core.data;

import net.minecraft.data.PackOutput;
import net.neoforged.neoforge.common.data.LanguageProvider;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;
import static mrtjp.projectred.core.init.CoreBlocks.ELECTROTINE_GENERATOR_BLOCK;
import static mrtjp.projectred.core.init.CoreItems.*;

public class CoreLanguageProvider extends LanguageProvider {

    public CoreLanguageProvider(PackOutput output) {
        super(output, MOD_ID, "en_us");
    }

    @Override
    protected void addTranslations() {

        // Creative tab
        add("itemGroup." + MOD_ID, "Project Red: Core");

        // Blocks
        add(ELECTROTINE_GENERATOR_BLOCK.get(), "Electrotine Generator");

        // Items
        add(PLATE_ITEM.get(), "Circuit Plate");
        add(CONDUCTIVE_PLATE_ITEM.get(), "Conductive Plate");
        add(WIRED_PLATE_ITEM.get(), "Wired Plate");
        add(BUNDLED_PLATE_ITEM.get(), "Bundled Plate");
        add(PLATFORMED_PLATE_ITEM.get(), "Platformed Plate");
        add(ANODE_ITEM.get(), "Anode");
        add(CATHODE_ITEM.get(), "Cathode");
        add(POINTER_ITEM.get(), "Pointer");
        add(SILICON_CHIP_ITEM.get(), "Silicon Chip");
        add(ENERGIZED_SILICON_CHIP_ITEM.get(), "Energized Silicon Chip");
        add(RED_ALLOY_INGOT_ITEM.get(), "Red Alloy Ingot");
        add(ELECTROTINE_ALLOY_INGOT_ITEM.get(), "Electrotine Alloy Ingot");
        add(ELECTROTINE_DUST_ITEM.get(), "Electrotine Dust");
        add(RUBY_ITEM.get(), "Ruby");
        add(SAPPHIRE_ITEM.get(), "Sapphire");
        add(PERIDOT_ITEM.get(), "Peridot");
        add(SAND_COAL_COMP_ITEM.get(), "Sand Coal Compound");
        add(RED_IRON_COMP_ITEM.get(), "Red Iron Compound");
        add(ELECTROTINE_IRON_COMP_ITEM.get(), "Electrotine Iron Compound");
        add(SILICON_BOULE_ITEM.get(), "Silicon Boule");
        add(SILICON_ITEM.get(), "Silicon");
        add(RED_SILICON_COMP_ITEM.get(), "Red Silicon Compound");
        add(GLOW_SILICON_COMP_ITEM.get(), "Glowing Silicon Compound");
        add(ELECTROTINE_SILICON_COMP_ITEM.get(), "Electrotine Silicon Compound");
        add(INFUSED_SILICON_ITEM.get(), "Infused Silicon");
        add(ENERGIZED_SILICON_ITEM.get(), "Energized Silicon");
        add(ELECTROTINE_SILICON_ITEM.get(), "Electrotine Silicon");
        add(COPPER_COIL_ITEM.get(), "Copper Coil");
        add(IRON_COIL_ITEM.get(), "Iron Coil");
        add(GOLD_COIL_ITEM.get(), "Gold Coil");
        add(MOTOR_ITEM.get(), "Motor");
        add(WOVEN_CLOTH_ITEM.get(), "Woven Cloth");
        add(SAIL_ITEM.get(), "Sail");
        add(WHITE_ILLUMAR_ITEM.get(), "White Illumar");
        add(ORANGE_ILLUMAR_ITEM.get(), "Orange Illumar");
        add(MAGENTA_ILLUMAR_ITEM.get(), "Magenta Illumar");
        add(LIGHT_BLUE_ILLUMAR_ITEM.get(), "Light Blue Illumar");
        add(YELLOW_ILLUMAR_ITEM.get(), "Yellow Illumar");
        add(LIME_ILLUMAR_ITEM.get(), "Lime Illumar");
        add(PINK_ILLUMAR_ITEM.get(), "Pink Illumar");
        add(GRAY_ILLUMAR_ITEM.get(), "Gray Illumar");
        add(LIGHT_GRAY_ILLUMAR_ITEM.get(), "Light Gray Illumar");
        add(CYAN_ILLUMAR_ITEM.get(), "Cyan Illumar");
        add(PURPLE_ILLUMAR_ITEM.get(), "Purple Illumar");
        add(BLUE_ILLUMAR_ITEM.get(), "Blue Illumar");
        add(BROWN_ILLUMAR_ITEM.get(), "Brown Illumar");
        add(GREEN_ILLUMAR_ITEM.get(), "Green Illumar");
        add(RED_ILLUMAR_ITEM.get(), "Red Illumar");
        add(BLACK_ILLUMAR_ITEM.get(), "Black Illumar");
        add(DRAW_PLATE_ITEM.get(), "Draw Plate");
        add(SCREWDRIVER_ITEM.get(), "Screwdriver");
        add(MULTIMETER_ITEM.get(), "Multimeter");

        addConfigKey("title", "Project Red Configuration");
        addConfigKey("section.projectred.client.toml", "Client Settings");
        addConfigKey("section.projectred.server.toml", "Server Settings");
        addConfigKey("section.projectred.client.toml.title", "Project Red Client Settings");
        addConfigKey("section.projectred.server.toml.title", "Project Red Server Settings");

        addConfigKey("gameplay", "Gameplay Settings", "Settings that effect gameplay, balance, progression, etc.");
        addConfigKey("infinite_screwdriver", "Unbreakable Screwdriver", "If set to ON, the basic screwdriver will not take damage");
        addConfigKey("gate_sounds", "Logic Gate Sounds", "If set to OFF, logic gates will not make sounds");

        addConfigKey("performance", "Performance Settings", "Settings that effect performance");
        addConfigKey("gate_lights", "Logic Gate Lights",
                "If set to OFF, logic gates will not emit light. Can help reduce light updates "
                    + "on particularly large and fast-updating redstone circuits.");
        addConfigKey("gate_min_timer_ticks", "Logic Gate Minimum Timer Ticks",
                "Minimum amount of ticks the timer gates can be set to (min 4). Can be used to enforce "
                    + "lower update rates.");
        addConfigKey("frame_move_limit", "Frame Structure Move Limit",
                "Max blocks in a moving frame structure. Limiting this can improve performance on servers where "
                    + "lots of structures are being moved.");
        addConfigKey("auto_compile_tile_limit", "Auto-Compile Tile Limit",
                "Max number of tiles allowed in IC Workbench before auto-compile becomes disallowed (-1 to always "
                        + "allow, 0 to never allow). Recommended to keep this very low on servers.");

        addConfigKey("world_gen", "World Gen", "World generation settings for ores, structures, etc.");
        addConfigKey("ruby_ore", "Enable Ruby Ores");
        addConfigKey("sapphire_ore", "Enable Sapphire Ores");
        addConfigKey("peridot_ore", "Enable Peridot Ores");
        addConfigKey("tin_ore", "Enable Tin Ores");
        addConfigKey("silver_ore", "Enable Silver Ores");
        addConfigKey("electrotine_ore", "Enable Electrotine Ores");
        addConfigKey("marble_cave", "Enable Marble Caves");

        addConfigKey("rendering", "Rendering Settings", "Settings that effect rendering and special effects");
        addConfigKey("gate_3d_wires", "3D Logic Gate Wires",
                "If set to OFF, flat wire textures will be used for logic gates. "
                    + "Can improve performance significantly for large circuits");
        addConfigKey("static_wire_renderer", "Static Wire Rendering",
                "If set to OFF, wires will be rendered by a Block Entity renderer rather than the World Renderer");
        addConfigKey("static_gate_renderer", "Static Gate Rendering",
                "If set to OFF, gates will be rendered by a Block Entity renderer rather than the World Renderer");

        addConfigKey("lighting", "Lighting Settings", "Settings that effect ProjectRed light sources (lamps, etc)");
        addConfigKey("max_lights", "Light Render Limit",
                "Max lights on screen at a time, -1 for unlimited. This limits the number of light halos that can be rendered"
                        + " around ProjectRed light sources. Lower values improve performance.");
        addConfigKey("fabulous_lights", "Fabulous Light Rendering",
                "Use fabulous shader pipeline for lights when on Fabulous Graphics mode. This creates a screenspace"
                        + "blooming effect when looking towards ProjectRed light sources.");
    }

    private void addConfigKey(String key, String value) {
        add(MOD_ID + ".configuration." + key, value);
    }

    private void addConfigKey(String key, String value, String tooltip) {
        add(MOD_ID + ".configuration." + key, value);
        add(MOD_ID + ".configuration." + key + ".tooltip", tooltip);
    }
}
