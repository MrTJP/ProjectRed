package mrtjp.projectred.fabrication.client;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.buffer.TransformingVertexConsumer;
import codechicken.lib.vec.Transformation;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.fabrication.item.component.ICDataComponent;
import mrtjp.projectred.integration.client.GateModelRenderer;
import mrtjp.projectred.integration.part.IGateRenderData;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.world.item.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.LinkedList;
import java.util.List;

import static mrtjp.projectred.integration.client.GateComponentModels.*;

public class RenderFabricatedGate extends GateModelRenderer.GateRenderer {

    private final List<ComponentModel> models = new LinkedList<>();

    private final SidedWireModel simpleWires = new SidedWireModel(generateWireModels("ic1", 4));
    private final SidedWireModel analogWires = new SidedWireModel(generateWireModels("ic2", 4));
    private final SidedICBundledCableModel bundledWires = new SidedICBundledCableModel();
    private final FabricatedICModel icHousing = FabricatedICModel.INSTANCE;

    private boolean runtimeError = false;
    private String name = "untitled";

    public RenderFabricatedGate() {
        models.add(BaseComponentModel.INSTANCE);
        models.add(simpleWires);
        models.add(analogWires);
        models.add(bundledWires);
        models.add(icHousing);
    }

    @Override
    protected List<ComponentModel> getModels() {
        return models;
    }

    @Override
    protected void prepareInventory(@Nullable ItemStack stack) {
        var component = stack == null ? null : ICDataComponent.getComponent(stack);
        if (component == null) {
            runtimeError = true;
            name = "ERROR!";
            simpleWires.sidemask = 0;
            analogWires.sidemask = 0;
            bundledWires.sidemask = 0;
            return;
        }

        runtimeError = false;
        name = component.getName();

        var ifSpec = component.getInterfaceSpec();
        simpleWires.sidemask = ifSpec.getRedstoneInputMask() | ifSpec.getRedstoneOutputMask();
        analogWires.sidemask = ifSpec.getAnalogInputMask() | ifSpec.getAnalogOutputMask();
        bundledWires.sidemask = ifSpec.getBundledInputMask() | ifSpec.getBundledOutputMask();
    }

    @Override
    protected void prepare(IGateRenderData gate) {
        simpleWires.sidemask = gate.state2() & 0xF;
        analogWires.sidemask = (gate.state2() >> 4) & 0xF;
        bundledWires.sidemask = (gate.state2() >> 8) & 0xF;

        simpleWires.wires[0].on = (gate.state() & 0x11) != 0;
        simpleWires.wires[1].on = (gate.state() & 0x22) != 0;
        simpleWires.wires[2].on = (gate.state() & 0x44) != 0;
        simpleWires.wires[3].on = (gate.state() & 0x88) != 0;

        analogWires.wires[0].on = simpleWires.wires[0].on;
        analogWires.wires[1].on = simpleWires.wires[1].on;
        analogWires.wires[2].on = simpleWires.wires[2].on;
        analogWires.wires[3].on = simpleWires.wires[3].on;
    }

    @Override
    public boolean hasSpecials() {
        return true;
    }

    @Override
    protected void prepareDynamic(IGateRenderData gate, float partialFrame) {
        runtimeError = gate.hasRuntimeError();
        name = gate.getGateName();
    }

    @Override
    public void renderDynamic(CCRenderState ccrs, Transformation t) {
    }

    @Override
    public void renderCustomDynamic(CCRenderState ccrs, Transformation t, PoseStack mStack, MultiBufferSource buffers, int packedLight, int packedOverlay, float partialTicks) {

        // Render name
        icHousing.renderName(name, t, mStack, buffers, runtimeError ? EnumColour.RED.argb() : EnumColour.WHITE.argb(), packedLight);

        // Render glass
        ccrs.reset();
        ccrs.brightness = packedLight;
        ccrs.overlay = packedOverlay;
        ccrs.bind(new TransformingVertexConsumer(buffers.getBuffer(RenderType.translucentMovingBlock()), mStack), DefaultVertexFormat.BLOCK);
        icHousing.renderGlass(t, ccrs);
    }
}
