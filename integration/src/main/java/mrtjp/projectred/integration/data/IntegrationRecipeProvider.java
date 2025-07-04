package mrtjp.projectred.integration.data;

import codechicken.lib.datagen.recipe.RecipeProvider;
import mrtjp.projectred.integration.GateType;
import net.minecraft.core.HolderLookup;
import net.minecraft.data.PackOutput;
import net.minecraft.world.level.block.Blocks;
import net.neoforged.neoforge.common.Tags;

import java.util.concurrent.CompletableFuture;

import static mrtjp.projectred.core.init.CoreItems.*;
import static mrtjp.projectred.core.init.CoreTags.ILLUMAR_TAG;
import static mrtjp.projectred.integration.ProjectRedIntegration.MOD_ID;

public class IntegrationRecipeProvider extends RecipeProvider {

    public IntegrationRecipeProvider(CompletableFuture<HolderLookup.Provider> registries, PackOutput output) {
        super(registries, output, MOD_ID);
    }

    @Override
    protected void registerRecipes() {

        //OR gate
        shapedRecipe(GateType.OR.getItem(), 1)
                .key('C', CATHODE_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .patternLine("PCP")
                .patternLine("WCW")
                .patternLine("PWP");

        //NOR gate
        shapedRecipe(GateType.NOR.getItem(), 1)
                .key('A', ANODE_ITEM.get())
                .key('C', CATHODE_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .patternLine("PAP")
                .patternLine("WCW")
                .patternLine("PWP");

        //NOT gate
        shapedRecipe(GateType.NOT.getItem(), 1)
                .key('A', ANODE_ITEM.get())
                .key('C', CATHODE_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .patternLine("PCP")
                .patternLine("CAC")
                .patternLine("PWP");

        // AND gate
        shapedRecipe(GateType.AND.getItem(), 1)
                .key('A', ANODE_ITEM.get())
                .key('C', CATHODE_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .patternLine("ACA")
                .patternLine("CCC")
                .patternLine("PWP");
        //NAND gate
        shapedRecipe(GateType.NAND.getItem(), 1)
                .key('A', ANODE_ITEM.get())
                .key('C', CATHODE_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .patternLine("AAA")
                .patternLine("CCC")
                .patternLine("PWP");

        //XOR gate
        shapedRecipe(GateType.XOR.getItem(), 1)
                .key('A', ANODE_ITEM.get())
                .key('C', CATHODE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .patternLine("AWA")
                .patternLine("CAC")
                .patternLine("WCW");

        //XNOR gate
        shapedRecipe(GateType.XNOR.getItem(), 1)
                .key('A', ANODE_ITEM.get())
                .key('C', CATHODE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .patternLine("ACA")
                .patternLine("CAC")
                .patternLine("WCW");

        //Buffer gate
        shapedRecipe(GateType.BUFFER.getItem(), 1)
                .key('A', ANODE_ITEM.get())
                .key('C', CATHODE_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .patternLine("ACA")
                .patternLine("WCW")
                .patternLine("PWP");

        //Multiplexer gate
        shapedRecipe(GateType.MULTIPLEXER.getItem(), 1)
                .key('A', ANODE_ITEM.get())
                .key('C', CATHODE_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .patternLine("ACA")
                .patternLine("CPC")
                .patternLine("ACW");

        //Pulse gate
        shapedRecipe(GateType.PULSE.getItem(), 1)
                .key('A', ANODE_ITEM.get())
                .key('C', CATHODE_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .patternLine("ACA")
                .patternLine("CAC")
                .patternLine("WWP");

        //Repeater gate
        shapedRecipe(GateType.REPEATER.getItem(), 1)
                .key('A', ANODE_ITEM.get())
                .key('C', CATHODE_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .patternLine("PCA")
                .patternLine("ACP")
                .patternLine("PWP");

        //Randomizer gate
        shapedRecipe(GateType.RANDOMIZER.getItem(), 1)
                .key('P', PLATE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .key('E', ENERGIZED_SILICON_CHIP_ITEM.get())
                .patternLine("PEP")
                .patternLine("WWW")
                .patternLine("EWE");

        //SR latch gate
        shapedRecipe(GateType.SR_LATCH.getItem(), 1)
                .key('A', ANODE_ITEM.get())
                .key('C', CATHODE_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .patternLine("ACW")
                .patternLine("WPW")
                .patternLine("WCA");

        //Toggle latch gate
        shapedRecipe(GateType.TOGGLE_LATCH.getItem(), 1)
                .key('C', CATHODE_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .key('L', Blocks.LEVER)
                .patternLine("CPP")
                .patternLine("WLW")
                .patternLine("CPP");

        //Transparent latch gate
        shapedRecipe(GateType.TRANSPARENT_LATCH.getItem(), 1)
                .key('A', ANODE_ITEM.get())
                .key('C', CATHODE_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .patternLine("ACW")
                .patternLine("CCC")
                .patternLine("CWP");

        //Light sensor gate
        shapedRecipe(GateType.LIGHT_SENSOR.getItem(), 1)
                .key('P', PLATE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .key('B', Tags.Items.DYES_BLUE)
                .patternLine("PPP")
                .patternLine("BBB")
                .patternLine("PWP");

        //Rain sensor gate
        shapedRecipe(GateType.RAIN_SENSOR.getItem(), 1)
                .key('P', PLATE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .key('S', Tags.Items.SLIMEBALLS)
                .patternLine("PPP")
                .patternLine("SSS")
                .patternLine("PWP");

        //Timer gate
        shapedRecipe(GateType.TIMER.getItem(), 1)
                .key('A', ANODE_ITEM.get())
                .key('C', CATHODE_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .key('T', POINTER_ITEM.get())
                .patternLine("ACA")
                .patternLine("WTW")
                .patternLine("PWP");

        //Sequencer gate
        shapedRecipe(GateType.SEQUENCER.getItem(), 1)
                .key('C', CATHODE_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .key('T', POINTER_ITEM.get())
                .patternLine("PCP")
                .patternLine("CTC")
                .patternLine("PCP");

        //Counter gate
        shapedRecipe(GateType.COUNTER.getItem(), 1)
                .key('C', CATHODE_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .key('T', POINTER_ITEM.get())
                .patternLine("PCP")
                .patternLine("WWT")
                .patternLine("PCP");

        //State cell gate
        shapedRecipe(GateType.STATE_CELL.getItem(), 1)
                .key('A', ANODE_ITEM.get())
                .key('C', CATHODE_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .key('T', POINTER_ITEM.get())
                .key('S', SILICON_CHIP_ITEM.get())
                .patternLine("PAC")
                .patternLine("WST")
                .patternLine("PWP");

        //Synchronizer gate
        shapedRecipe(GateType.SYNCHRONIZER.getItem(), 1)
                .key('A', ANODE_ITEM.get())
                .key('C', CATHODE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .key('S', SILICON_CHIP_ITEM.get())
                .patternLine("WCW")
                .patternLine("SAS")
                .patternLine("WWW");

        //Bus tranceiver gate
        shapedRecipe(GateType.BUS_TRANSCEIVER.getItem(), 1)
                .key('P', PLATE_ITEM.get())
                .key('B', BUNDLED_PLATE_ITEM.get())
                .key('S', SILICON_CHIP_ITEM.get())
                .patternLine("BBB")
                .patternLine("SPS")
                .patternLine("BBB");

        //Null cell gate
        shapedRecipe(GateType.NULL_CELL.getItem(), 1)
                .key('P', PLATE_ITEM.get())
                .key('W', WIRED_PLATE_ITEM.get())
                .key('F', PLATFORMED_PLATE_ITEM.get())
                .patternLine("PWP")
                .patternLine("WFW")
                .patternLine("PWP");

        //Invert cell gate
        shapedRecipe(GateType.INVERT_CELL.getItem(), 1)
                .key('C', CATHODE_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .key('W', WIRED_PLATE_ITEM.get())
                .key('F', PLATFORMED_PLATE_ITEM.get())
                .patternLine("PWP")
                .patternLine("WFW")
                .patternLine("PCP");

        //Buffer cell gate
        shapedRecipe(GateType.BUFFER_CELL.getItem(), 1)
                .key('C', CATHODE_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .key('W', WIRED_PLATE_ITEM.get())
                .key('F', PLATFORMED_PLATE_ITEM.get())
                .patternLine("PWP")
                .patternLine("WFW")
                .patternLine("PCC");

        //Comparator gate
        shapedRecipe(GateType.COMPARATOR.getItem(), 1)
                .key('C', CATHODE_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .key('Q', Tags.Items.GEMS_QUARTZ)
                .patternLine("WCW")
                .patternLine("QWQ")
                .patternLine("PWP");

        //AND cell gate
        shapedRecipe(GateType.AND_CELL.getItem(), 1)
                .key('C', CATHODE_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .key('w', CONDUCTIVE_PLATE_ITEM.get())
                .key('W', WIRED_PLATE_ITEM.get())
                .key('S', PLATFORMED_PLATE_ITEM.get())
                .patternLine("CwC")
                .patternLine("WSW")
                .patternLine("PwC");

        //Bus randomizer gate
        shapedRecipe(GateType.BUS_RANDOMIZER.getItem(), 1)
                .key('R', CONDUCTIVE_PLATE_ITEM.get())
                .key('B', BUNDLED_PLATE_ITEM.get())
                .key('G', ENERGIZED_SILICON_CHIP_ITEM.get())
                .patternLine("BBB")
                .patternLine("RGR")
                .patternLine("BBB");

        //Bus converter gate
        shapedRecipe(GateType.BUS_CONVERTER.getItem(), 1)
                .key('P', PLATE_ITEM.get())
                .key('B', BUNDLED_PLATE_ITEM.get())
                .key('R', CONDUCTIVE_PLATE_ITEM.get())
                .key('S', SILICON_CHIP_ITEM.get())
                .patternLine("PBP")
                .patternLine("RSR")
                .patternLine("PRP");

        //Bus input panel gate
        shapedRecipe(GateType.BUS_INPUT_PANEL.getItem(), 1)
                .key('B', BUNDLED_PLATE_ITEM.get())
                .key('R', CONDUCTIVE_PLATE_ITEM.get())
                .key('I', ILLUMAR_TAG)
                .patternLine("BRB")
                .patternLine("BIB")
                .patternLine("BBB");

        //Stacking latch gate
        shapedRecipe(GateType.TRANSPARENT_LATCH_CELL.getItem(), 1)
                .key('P', PLATE_ITEM.get())
                .key('C', CATHODE_ITEM.get())
                .key('R', WIRED_PLATE_ITEM.get())
                .patternLine("PCP")
                .patternLine("RCR")
                .patternLine("PCC");

        //Segment display gate
        shapedRecipe(GateType.SEGMENT_DISPLAY.getItem(), 1)
                .key('P', PLATE_ITEM.get())
                .key('B', BUNDLED_PLATE_ITEM.get())
                .key('Q', Tags.Items.GEMS_QUARTZ)
                .key('S', SILICON_CHIP_ITEM.get())
                .patternLine("PBP")
                .patternLine("QSQ")
                .patternLine("PQP");


        //Dec randomizer gate
        shapedRecipe(GateType.DEC_RANDOMIZER.getItem(), 1)
                .key('S', SILICON_CHIP_ITEM.get())
                .key('C', CATHODE_ITEM.get())
                .key('A', ANODE_ITEM.get())
                .key('E', ENERGIZED_SILICON_CHIP_ITEM.get())
                .key('W', CONDUCTIVE_PLATE_ITEM.get())
                .patternLine("SCA")
                .patternLine("CCC")
                .patternLine("EWE");

    }
}
