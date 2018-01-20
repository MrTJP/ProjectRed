package mrtjp.projectred.archive

import java.lang.{Character => JChar}

import mrtjp.projectred.core.PartDefs
import mrtjp.projectred.integration.GateDefinition
import net.minecraft.init.Blocks
import net.minecraft.item.ItemStack

object IntegrationRecipes
{
    var dumper:RecipeDumper = _
    def initRecipes()
    {
        dumper = new RecipeDumper("integration")
        initGateRecipes()
        dumper.dump()
    }

    private def initGateRecipes()
    {
            //TODO
        /** AND Gate **/
        dumper.addRecipe(GateDefinition.AND.makeStack,
                "ACA",
                "CCC",
                "PWP",
                'A':JChar, PartDefs.ANODE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'P':JChar, PartDefs.PLATE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        ).setJsonName("and_gate")

        /** OR Gate **/
        dumper.addRecipe(GateDefinition.OR.makeStack,
                "PCP",
                "WCW",
                "PWP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        ).setJsonName("or_gate")

        /** NOT Gate**/
        dumper.addRecipe(GateDefinition.NOT.makeStack,
                "PCP",
                "CAC",
                "PWP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'A':JChar, PartDefs.ANODE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        ).setJsonName("not_gate")


        /** RS Latch **/
        dumper.addRecipe(GateDefinition.SRLatch.makeStack,
                "ACW",
                "WPW",
                "WCA",
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'A':JChar, PartDefs.ANODE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'P':JChar, PartDefs.PLATE.makeStack
        ).setJsonName("rs_latch")

        /** Toggle Latch **/
        dumper.addRecipe(GateDefinition.ToggleLatch.makeStack,
                "CPP",
                "WLW",
                "CPP",
                'C':JChar, PartDefs.CATHODE.makeStack,
                'P':JChar, PartDefs.PLATE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'L':JChar, new ItemStack(Blocks.LEVER)
        ).setJsonName("toggle_latch")

        /** Transparent Latch **/
        dumper.addRecipe(GateDefinition.TransparentLatch.makeStack,
                "ACW",
                "CCC",
                "CWP",
                'C':JChar, PartDefs.CATHODE.makeStack,
                'A':JChar, PartDefs.ANODE.makeStack,
                'P':JChar, PartDefs.PLATE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        ).setJsonName("transparent_latch")

        /** NOR Gate **/
        dumper.addRecipe(GateDefinition.NOR.makeStack,
                "PAP",
                "WCW",
                "PWP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'A':JChar, PartDefs.ANODE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack
        ).setJsonName("nor_gate")

        /** NAND Gate **/
        dumper.addRecipe(GateDefinition.NAND.makeStack,
                "AAA",
                "CCC",
                "PWP",
                'A':JChar, PartDefs.ANODE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'P':JChar, PartDefs.PLATE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        ).setJsonName("nand_gate")

        /** XOR Gate **/
        dumper.addRecipe(GateDefinition.XOR.makeStack,
                "AWA",
                "CAC",
                "WCW",
                'A':JChar, PartDefs.ANODE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack
        ).setJsonName("xor_gate")

        /** XNOR Gate **/
        dumper.addRecipe(GateDefinition.XNOR.makeStack,
                "ACA",
                "CAC",
                "WCW",
                'A':JChar, PartDefs.ANODE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        ).setJsonName("xnor_gate")

        /** Buffer Gate **/
        dumper.addRecipe(GateDefinition.Buffer.makeStack,
                "ACA",
                "WCW",
                "PWP",
                'A':JChar, PartDefs.ANODE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'P':JChar, PartDefs.PLATE.makeStack
        ).setJsonName("buffer_gate")

        /** Multiplexer Gate **/
        dumper.addRecipe(GateDefinition.Multiplexer.makeStack,
                "ACA",
                "CPC",
                "ACW",
                'A':JChar, PartDefs.ANODE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'P':JChar, PartDefs.PLATE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        ).setJsonName("multiplexer_gate")

        /** Repeater Gate **/
        dumper.addRecipe(GateDefinition.Repeater.makeStack,
                "PCA",
                "ACP",
                "PWP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'A':JChar, PartDefs.ANODE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        ).setJsonName("repeater_gate")

        /** Timer Gate **/
        dumper.addRecipe(GateDefinition.Timer.makeStack,
                "ACA",
                "WTW",
                "PWP",
                'A':JChar, PartDefs.ANODE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'T':JChar, PartDefs.POINTER.makeStack,
                'P':JChar, PartDefs.PLATE.makeStack
        ).setJsonName("timer_gate")

        /** Counter Gate **/
        dumper.addRecipe(GateDefinition.Counter.makeStack,
                "PCP",
                "WWT",
                "PCP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'T':JChar, PartDefs.POINTER.makeStack,
                'A':JChar, PartDefs.ANODE.makeStack
        ).setJsonName("counter_gate")

        /** Sequencer Gate **/
        dumper.addRecipe(GateDefinition.Sequencer.makeStack,
                "PCP",
                "CTC",
                "PCP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'T':JChar, PartDefs.POINTER.makeStack
        ).setJsonName("sequencer_gate")

        /** Pulse Former Gate **/
        dumper.addRecipe(GateDefinition.Pulse.makeStack,
                "ACA",
                "CAC",
                "WWP",
                'A':JChar, PartDefs.ANODE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'P':JChar, PartDefs.PLATE.makeStack
        ).setJsonName("pulse_former_gate")

        /** Randomizer Gate **/
        dumper.addRecipe(GateDefinition.Randomizer.makeStack,
                "PEP",
                "WWW",
                "EWE",
                'P':JChar, PartDefs.PLATE.makeStack,
                'E':JChar, PartDefs.ENERGIZEDSILICONCHIP.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        ).setJsonName("randomizer_gate")

        /** State Cell Gate **/
        dumper.addRecipe(GateDefinition.StateCell.makeStack,
                "PAC",
                "WST",
                "PWP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'A':JChar, PartDefs.ANODE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'S':JChar, PartDefs.SILICONCHIP.makeStack,
                'T':JChar, PartDefs.POINTER.makeStack
        ).setJsonName("state_cell_gate")

        /** Synchronizer Gate **/
        dumper.addRecipe(GateDefinition.Synchronizer.makeStack,
                "WCW",
                "SAS",
                "WWW",
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'S':JChar, PartDefs.SILICONCHIP.makeStack,
                'A':JChar, PartDefs.ANODE.makeStack
        ).setJsonName("synchromizer_gate")

        /** Light Sensor **/
        dumper.addRecipe(new ShapedOreRecipe(GateDefinition.LightSensor.makeStack,
                "PPP",
                "LLL",
                "PWP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'L':JChar, "dyeBlue",
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        )).setJsonName("light_sensor")

        /** Rain Sensor **/
        dumper.addRecipe(new ShapedOreRecipe(GateDefinition.RainSensor.makeStack,
                "PPP",
                "SSS",
                "PWP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'S':JChar, "slimeball",
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        )).setJsonName("rain_sensor")

        /** Bus Transceiver **/
        dumper.addRecipe(GateDefinition.BusTransceiver.makeStack,
                "BBB",
                "SPS",
                "BBB",
                'P':JChar, PartDefs.PLATE.makeStack,
                'B':JChar, PartDefs.BUNDLEDPLATE.makeStack,
                'S':JChar, PartDefs.SILICONCHIP.makeStack
        ).setJsonName("bus_transceiver")

        /** Null Cell **/
        dumper.addRecipe(GateDefinition.NullCell.makeStack,
                "PWP",
                "WSW",
                "PWP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'W':JChar, PartDefs.WIREDPLATE.makeStack,
                'S':JChar, PartDefs.PLATFORMEDPLATE.makeStack
        ).setJsonName("null_cell")

        /** Invert Cell **/
        dumper.addRecipe(GateDefinition.InvertCell.makeStack,
                "PWP",
                "WSW",
                "PCP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'W':JChar, PartDefs.WIREDPLATE.makeStack,
                'S':JChar, PartDefs.PLATFORMEDPLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack
        ).setJsonName("invert_cell")

        /** Buffer Cell **/
        dumper.addRecipe(GateDefinition.BufferCell.makeStack,
                "PWP",
                "WSW",
                "PCC",
                'P':JChar, PartDefs.PLATE.makeStack,
                'W':JChar, PartDefs.WIREDPLATE.makeStack,
                'S':JChar, PartDefs.PLATFORMEDPLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack
        ).setJsonName("buffer_cell")

        /** Comparator **/
        dumper.addRecipe(new ShapedOreRecipe(GateDefinition.Comparator.makeStack,
                "WCW",
                "QWQ",
                "PWP",
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'Q':JChar, "gemQuartz",
                'P':JChar, PartDefs.PLATE.makeStack
        )).setJsonName("comparator")

        /** AND Cell **/
        dumper.addRecipe(GateDefinition.ANDCell.makeStack,
                "CwC",
                "WSW",
                "PwC",
                'w':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'W':JChar, PartDefs.WIREDPLATE.makeStack,
                'S':JChar, PartDefs.PLATFORMEDPLATE.makeStack,
                'P':JChar, PartDefs.PLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack
        ).setJsonName("and_cell")

        /** Bus Randomizer **/
        dumper.addRecipe(GateDefinition.BusRandomizer.makeStack,
                "BBB",
                "RGR",
                "BBB",
                'R':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'B':JChar, PartDefs.BUNDLEDPLATE.makeStack,
                'G':JChar, PartDefs.ENERGIZEDSILICONCHIP.makeStack
        ).setJsonName("bus_randomizer")

        /** Bus Converter **/
        dumper.addRecipe(GateDefinition.BusConverter.makeStack,
                "PBP",
                "RSR",
                "PRP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'B':JChar, PartDefs.BUNDLEDPLATE.makeStack,
                'R':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'S':JChar, PartDefs.SILICONCHIP.makeStack
        ).setJsonName("bus_converter")

        /** Bus Input Panel **/
        dumper.addRecipe(new ShapedOreRecipe(GateDefinition.BusInputPanel.makeStack,
                "BRB",
                "BIB",
                "BBB",
                'B':JChar, PartDefs.BUNDLEDPLATE.makeStack,
                'R':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'I':JChar, PartDefs.oreDictDefinitionIllumar
        )).setJsonName("bus_input_panel")

        /** Stacking Latch **/
        dumper.addRecipe(GateDefinition.StackingLatch.makeStack,
            "PCP",
            "RCR",
            "PCC",
            'P':JChar, PartDefs.PLATE.makeStack,
            'C':JChar, PartDefs.CATHODE.makeStack,
            'R':JChar, PartDefs.WIREDPLATE.makeStack
        ).setJsonName("stacking_latch")

        /** Segment Display **/
        dumper.addRecipe(new ShapedOreRecipe(GateDefinition.SegmentDisplay.makeStack,
            "PBP",
            "QSQ",
            "PQP",
            'P':JChar, PartDefs.PLATE.makeStack,
            'B':JChar, PartDefs.BUNDLEDPLATE.makeStack,
            'Q':JChar, "gemQuartz",
            'S':JChar, PartDefs.SILICONCHIP.makeStack
        )).setJsonName("segment_display")

        /** Decoding Randomizer Gate **/
        dumper.addRecipe(GateDefinition.DecRandomizer.makeStack,
            "SCA",
            "CCC",
            "EWE",
            'S':JChar, PartDefs.SILICONCHIP.makeStack,
            'C':JChar, PartDefs.CATHODE.makeStack,
            'A':JChar, PartDefs.ANODE.makeStack,
            'E':JChar, PartDefs.ENERGIZEDSILICONCHIP.makeStack,
            'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        ).setJsonName("decoding_randomizer_gate")
    }
}
