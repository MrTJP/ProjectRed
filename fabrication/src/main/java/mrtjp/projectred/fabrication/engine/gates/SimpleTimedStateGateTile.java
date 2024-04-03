package mrtjp.projectred.fabrication.engine.gates;

import codechicken.lib.vec.Cuboid6;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.editor.tools.InteractionZone;
import net.minecraft.network.chat.Component;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import java.util.List;

import static mrtjp.projectred.fabrication.engine.gates.SimpleGateTile.ZONE_BOUNDS;
import static mrtjp.projectred.fabrication.engine.gates.SimpleGateTile.addDeadSidesInteractions;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_SIDE_DISABLED;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_SIDE_ENABLED;

/**
 * Combination of {@link TimedStateGateTile} and {@link SimpleGateTile}.
 * TODO: Find a cleaner way to do this
 */
public abstract class SimpleTimedStateGateTile extends TimedStateGateTile {

    public SimpleTimedStateGateTile(ICGateTileType gateType) {
        super(gateType);
    }

    //region BaseTile overrides
    @Override
    public void buildInteractionZoneList(List<InteractionZone> zones) {
        super.buildInteractionZoneList(zones);
        addDeadSidesInteractions(zones, interactMask(), this::getBoundsForIOToggleZone, this::toggleDeadSide, this::getDeadSideToolTip);
    }

    @OnlyIn(Dist.CLIENT)
    protected Component getDeadSideToolTip(int r) {
        boolean isEnabled = (getShape() & rotationToDeadSideBit(r)) == 0;
        return Component.translatable(isEnabled ? UL_SIDE_ENABLED : UL_SIDE_DISABLED).withStyle(ICWorkbenchEditor.UNIFORM_GRAY);
    }

    protected Cuboid6 getBoundsForIOToggleZone(int r) {
        return ZONE_BOUNDS[getRotation() << 2 | r];
    }

    protected void toggleDeadSide(int r) {
        if (getDeadSides() == 0) return;

        int oldShape = getShape();
        int shape = oldShape ^ rotationToDeadSideBit(r);
        if (Integer.bitCount(shape) > getMaxDeadSides()) return;

        configureShapeAndSend(shape);
    }
    //endregion


    //region SimpleTimedStateGateTile override points
    protected int interactMask() {
        return 0;
    }

    protected int getDeadSides() {
        return 0;
    }

    protected int getMaxDeadSides() {
        return getDeadSides() - 1;
    }

    protected int rotationToDeadSideBit(int r) {
        return 1 << (r - 1);
    }
    //endregion
}
