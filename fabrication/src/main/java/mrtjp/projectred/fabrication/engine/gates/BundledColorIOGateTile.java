package mrtjp.projectred.fabrication.engine.gates;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.vec.*;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.editor.tools.InteractionZone;
import mrtjp.projectred.fabrication.editor.tools.SimpleInteractionZone;
import mrtjp.projectred.fabrication.engine.ICInterfaceType;
import net.minecraft.network.chat.Component;

import java.util.List;

import static mrtjp.projectred.fabrication.init.FabricationUnlocal.*;

public class BundledColorIOGateTile extends SingleBitIOGateTile {

    private static final Cuboid6[] INPUT_TOGGLE_ZONE_BOUNDS = new Cuboid6[4];
    private static final Cuboid6[] DIR_ZONE_BOUNDS = new Cuboid6[4];
    private static final Cuboid6[] COLOR_ZONE_BOUNDS = new Cuboid6[4];

    static {
        for (int r = 0; r < 4; r++) {
            Transformation t = new Scale(1/16D).with(Rotation.quarterRotations[r].at(Vector3.CENTER));
            INPUT_TOGGLE_ZONE_BOUNDS[r] = new Cuboid6(1, 2, 0, 15, 3, 3).apply(t);       // Toggle state of IO register
            DIR_ZONE_BOUNDS[r]          = new Cuboid6(3, 2, 6, 13, 5, 10).apply(t);      // Toggle IO mode
            COLOR_ZONE_BOUNDS[r]        = new Cuboid6(6, 2, 10, 10, 4, 14).apply(t);     // Toggle colour
        }
    }

    public BundledColorIOGateTile() {
        super(ICGateTileType.BUNDLED_COLOR_IO);
    }

    //region IIOConnectionTile overrides
    @Override
    public ICInterfaceType getInterfaceType() {
        return ICInterfaceType.BUNDLED;
    }
    //endregion

    //region BaseTile overrides
    @Override
    public void buildInteractionZoneList(List<InteractionZone> zones) {
        super.buildInteractionZoneList(zones);

        // For toggling input to simulation
        zones.add(new SimpleInteractionZone.Builder()
                .bounds(() -> INPUT_TOGGLE_ZONE_BOUNDS[getRotation()])
                .leftClickAction(this::toggleWorldInput)
                .tooltip(toolTip -> {
                    toolTip.add(Component.translatable(isInputIOMode() ? UL_IO_COLORED_INPUT : UL_IO_COLORED_OUTPUT,
                                    Component.translatable(EnumColour.values()[ioBit & 0xFF].getUnlocalizedName()))
                            .append(Component.literal(": "))
                            .append(Component.translatable(((getState() & 0x44) != 0 ? UL_IO_LEVEL_HIGH : UL_IO_LEVEL_LOW)))
                            .withStyle(ICWorkbenchEditor.UNIFORM_GRAY));
                })
                .build());

        // For toggling input/output direction
        zones.add(new SimpleInteractionZone.Builder()
                .bounds(() -> DIR_ZONE_BOUNDS[getRotation()])
                .leftClickAction(this::toggleDirection)
                .tooltip(toolTip -> {
                    toolTip.add(Component.translatable(UL_IO_DIRECTION)
                            .append(Component.literal(": "))
                            .append(Component.translatable((isInputIOMode() ? UL_IO_DIR_INPUT : UL_IO_DIR_OUTPUT)))
                            .withStyle(ICWorkbenchEditor.UNIFORM_GRAY));
                })
                .build());

        // For toggling colour
        zones.add(new SimpleInteractionZone.Builder()
                .bounds(() -> COLOR_ZONE_BOUNDS[getRotation()])
                .leftClickAction(() -> shiftIOBit(true))
                .rightClickAction(() -> shiftIOBit(false))
                .tooltip(toolTip -> {
                    toolTip.add(Component.translatable(UL_IO_BUNDLED_COLOUR)
                            .append(Component.literal(": "))
                            .append(Component.translatable(EnumColour.values()[ioBit & 0xFF].getUnlocalizedName()))
                            .withStyle(ICWorkbenchEditor.UNIFORM_GRAY));
                })
                .build());
    }
    //endregion

}
