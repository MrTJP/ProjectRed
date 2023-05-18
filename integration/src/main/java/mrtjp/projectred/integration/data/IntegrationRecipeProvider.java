package mrtjp.projectred.integration.data;

import codechicken.lib.datagen.recipe.RecipeProvider;
import mrtjp.projectred.integration.GateType;
import net.minecraft.data.DataGenerator;
import net.minecraft.world.level.block.Blocks;
import net.minecraftforge.common.Tags;

import static mrtjp.projectred.core.init.CoreReferences.*;
import static mrtjp.projectred.core.init.CoreTags.ILLUMAR_TAG;

public class IntegrationRecipeProvider extends RecipeProvider {

    public IntegrationRecipeProvider(DataGenerator generatorIn) {
        super(generatorIn);
    }

    @Override
    public String getName() {
        return "ProjectRed-Integration Recipes";
    }

    @Override
    protected void registerRecipes() {

        //OR gate
        shapedRecipe(GateType.OR.getItem(), 1)
                .key('C', CATHODE_ITEM)
                .key('P', PLATE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .patternLine("PCP")
                .patternLine("WCW")
                .patternLine("PWP");

        //NOR gate
        shapedRecipe(GateType.NOR.getItem(), 1)
                .key('A', ANODE_ITEM)
                .key('C', CATHODE_ITEM)
                .key('P', PLATE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .patternLine("PAP")
                .patternLine("WCW")
                .patternLine("PWP");

        //NOT gate
        shapedRecipe(GateType.NOT.getItem(), 1)
                .key('A', ANODE_ITEM)
                .key('C', CATHODE_ITEM)
                .key('P', PLATE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .patternLine("PCP")
                .patternLine("CAC")
                .patternLine("PWP");

        // AND gate
        shapedRecipe(GateType.AND.getItem(), 1)
                .key('A', ANODE_ITEM)
                .key('C', CATHODE_ITEM)
                .key('P', PLATE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .patternLine("ACA")
                .patternLine("CCC")
                .patternLine("PWP");
        //NAND gate
        shapedRecipe(GateType.NAND.getItem(), 1)
                .key('A', ANODE_ITEM)
                .key('C', CATHODE_ITEM)
                .key('P', PLATE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .patternLine("AAA")
                .patternLine("CCC")
                .patternLine("PWP");

        //XOR gate
        shapedRecipe(GateType.XOR.getItem(), 1)
                .key('A', ANODE_ITEM)
                .key('C', CATHODE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .patternLine("AWA")
                .patternLine("CAC")
                .patternLine("WCW");

        //XNOR gate
        shapedRecipe(GateType.XNOR.getItem(), 1)
                .key('A', ANODE_ITEM)
                .key('C', CATHODE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .patternLine("ACA")
                .patternLine("CAC")
                .patternLine("WCW");

        //Buffer gate
        shapedRecipe(GateType.BUFFER.getItem(), 1)
                .key('A', ANODE_ITEM)
                .key('C', CATHODE_ITEM)
                .key('P', PLATE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .patternLine("ACA")
                .patternLine("WCW")
                .patternLine("PWP");;

        //Multiplexer gate
        shapedRecipe(GateType.MULTIPLEXER.getItem(), 1)
                .key('A', ANODE_ITEM)
                .key('C', CATHODE_ITEM)
                .key('P', PLATE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .patternLine("ACA")
                .patternLine("CPC")
                .patternLine("ACW");

        //Pulse gate
        shapedRecipe(GateType.PULSE.getItem(), 1)
                .key('A', ANODE_ITEM)
                .key('C', CATHODE_ITEM)
                .key('P', PLATE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .patternLine("ACA")
                .patternLine("CAC")
                .patternLine("WWP");

        //Repeater gate
        shapedRecipe(GateType.REPEATER.getItem(), 1)
                .key('A', ANODE_ITEM)
                .key('C', CATHODE_ITEM)
                .key('P', PLATE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .patternLine("PCA")
                .patternLine("ACP")
                .patternLine("PWP");

        //Randomizer gate
        shapedRecipe(GateType.RANDOMIZER.getItem(), 1)
                .key('P', PLATE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .key('E', ENERGIZED_SILICON_CHIP_ITEM)
                .patternLine("PEP")
                .patternLine("WWW")
                .patternLine("EWE");

        //SR latch gate
        shapedRecipe(GateType.SR_LATCH.getItem(), 1)
                .key('A', ANODE_ITEM)
                .key('C', CATHODE_ITEM)
                .key('P', PLATE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .patternLine("ACW")
                .patternLine("WPW")
                .patternLine("WCA");

        //Toggle latch gate
        shapedRecipe(GateType.TOGGLE_LATCH.getItem(), 1)
                .key('C', CATHODE_ITEM)
                .key('P', PLATE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .key('L', Blocks.LEVER)
                .patternLine("CPP")
                .patternLine("WLW")
                .patternLine("CPP");

        //Transparent latch gate
        shapedRecipe(GateType.TRANSPARENT_LATCH.getItem(), 1)
                .key('A', ANODE_ITEM)
                .key('C', CATHODE_ITEM)
                .key('P', PLATE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .patternLine("ACW")
                .patternLine("CCC")
                .patternLine("CWP");

        //Light sensor gate
        shapedRecipe(GateType.LIGHT_SENSOR.getItem(), 1)
                .key('P', PLATE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .key('B', Tags.Items.DYES_BLUE)
                .patternLine("PPP")
                .patternLine("BBB")
                .patternLine("PWP");

        //Rain sensor gate
        shapedRecipe(GateType.RAIN_SENSOR.getItem(), 1)
                .key('P', PLATE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .key('S', Tags.Items.SLIMEBALLS)
                .patternLine("PPP")
                .patternLine("SSS")
                .patternLine("PWP");

        //Timer gate
        shapedRecipe(GateType.TIMER.getItem(), 1)
                .key('A', ANODE_ITEM)
                .key('C', CATHODE_ITEM)
                .key('P', PLATE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .key('T', POINTER_ITEM)
                .patternLine("ACA")
                .patternLine("WTW")
                .patternLine("PWP");

        //Sequencer gate
        shapedRecipe(GateType.SEQUENCER.getItem(), 1)
                .key('C', CATHODE_ITEM)
                .key('P', PLATE_ITEM)
                .key('T', POINTER_ITEM)
                .patternLine("PCP")
                .patternLine("CTC")
                .patternLine("PCP");

        //Counter gate
        shapedRecipe(GateType.COUNTER.getItem(), 1)
                .key('C', CATHODE_ITEM)
                .key('P', PLATE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .key('T', POINTER_ITEM)
                .patternLine("PCP")
                .patternLine("WWT")
                .patternLine("PCP");

        //State cell gate
        shapedRecipe(GateType.STATE_CELL.getItem(), 1)
                .key('A', ANODE_ITEM)
                .key('C', CATHODE_ITEM)
                .key('P', PLATE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .key('T', POINTER_ITEM)
                .key('S', SILICON_CHIP_ITEM)
                .patternLine("PAC")
                .patternLine("WST")
                .patternLine("PWP");

        //Synchronizer gate
        shapedRecipe(GateType.SYNCHRONIZER.getItem(), 1)
                .key('A', ANODE_ITEM)
                .key('C', CATHODE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .key('S', SILICON_CHIP_ITEM)
                .patternLine("WCW")
                .patternLine("SAS")
                .patternLine("WWW");

        //Bus tranceiver gate
        shapedRecipe(GateType.BUS_TRANSCEIVER.getItem(), 1)
                .key('P', PLATE_ITEM)
                .key('B', BUNDLED_PLATE_ITEM)
                .key('S', SILICON_CHIP_ITEM)
                .patternLine("BBB")
                .patternLine("SPS")
                .patternLine("BBB");

        //Null cell gate
        shapedRecipe(GateType.NULL_CELL.getItem(), 1)
                .key('P', PLATE_ITEM)
                .key('W', WIRED_PLATE_ITEM)
                .key('F', PLATFORMED_PLATE_ITEM)
                .patternLine("PWP")
                .patternLine("WFW")
                .patternLine("PWP");

        //Invert cell gate
        shapedRecipe(GateType.INVERT_CELL.getItem(), 1)
                .key('C', CATHODE_ITEM)
                .key('P', PLATE_ITEM)
                .key('W', WIRED_PLATE_ITEM)
                .key('F', PLATFORMED_PLATE_ITEM)
                .patternLine("PWP")
                .patternLine("WFW")
                .patternLine("PCP");

        //Buffer cell gate
        shapedRecipe(GateType.BUFFER_CELL.getItem(), 1)
                .key('C', CATHODE_ITEM)
                .key('P', PLATE_ITEM)
                .key('W', WIRED_PLATE_ITEM)
                .key('F', PLATFORMED_PLATE_ITEM)
                .patternLine("PWP")
                .patternLine("WFW")
                .patternLine("PCC");

        //Comparator gate
        shapedRecipe(GateType.COMPARATOR.getItem(), 1)
                .key('C', CATHODE_ITEM)
                .key('P', PLATE_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .key('Q', Tags.Items.GEMS_QUARTZ)
                .patternLine("WCW")
                .patternLine("QWQ")
                .patternLine("PWP");

        //AND cell gate
        shapedRecipe(GateType.AND_CELL.getItem(), 1)
                .key('C', CATHODE_ITEM)
                .key('P', PLATE_ITEM)
                .key('w', CONDUCTIVE_PLATE_ITEM)
                .key('W', WIRED_PLATE_ITEM)
                .key('S', PLATFORMED_PLATE_ITEM)
                .patternLine("CwC")
                .patternLine("WSW")
                .patternLine("PwC");

        //Bus randomizer gate
        shapedRecipe(GateType.BUS_RANDOMIZER.getItem(), 1)
                .key('R', CONDUCTIVE_PLATE_ITEM)
                .key('B', BUNDLED_PLATE_ITEM)
                .key('G', ENERGIZED_SILICON_CHIP_ITEM)
                .patternLine("BBB")
                .patternLine("RGR")
                .patternLine("BBB");

        //Bus converter gate
        shapedRecipe(GateType.BUS_CONVERTER.getItem(), 1)
                .key('P', PLATE_ITEM)
                .key('B', BUNDLED_PLATE_ITEM)
                .key('R', CONDUCTIVE_PLATE_ITEM)
                .key('S', SILICON_CHIP_ITEM)
                .patternLine("PBP")
                .patternLine("RSR")
                .patternLine("PRP");

        //Bus input panel gate
        shapedRecipe(GateType.BUS_INPUT_PANEL.getItem(), 1)
                .key('B', BUNDLED_PLATE_ITEM)
                .key('R', CONDUCTIVE_PLATE_ITEM)
                .key('I', ILLUMAR_TAG)
                .patternLine("BRB")
                .patternLine("BIB")
                .patternLine("BBB");

        //Stacking latch gate
        shapedRecipe(GateType.TRANSPARENT_LATCH_CELL.getItem(), 1)
                .key('P', PLATE_ITEM)
                .key('C', CATHODE_ITEM)
                .key('R', WIRED_PLATE_ITEM)
                .patternLine("PCP")
                .patternLine("RCR")
                .patternLine("PCC");

        //Segment display gate
        shapedRecipe(GateType.SEGMENT_DISPLAY.getItem(), 1)
                .key('P', PLATE_ITEM)
                .key('B', BUNDLED_PLATE_ITEM)
                .key('Q', Tags.Items.GEMS_QUARTZ)
                .key('S', SILICON_CHIP_ITEM)
                .patternLine("PBP")
                .patternLine("QSQ")
                .patternLine("PQP");


        //Dec randomizer gate
        shapedRecipe(GateType.DEC_RANDOMIZER.getItem(), 1)
                .key('S', SILICON_CHIP_ITEM)
                .key('C', CATHODE_ITEM)
                .key('A', ANODE_ITEM)
                .key('E', ENERGIZED_SILICON_CHIP_ITEM)
                .key('W', CONDUCTIVE_PLATE_ITEM)
                .patternLine("SCA")
                .patternLine("CCC")
                .patternLine("EWE");

    }
}
