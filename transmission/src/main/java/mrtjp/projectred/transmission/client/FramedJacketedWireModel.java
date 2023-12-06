package mrtjp.projectred.transmission.client;

import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.pipeline.IVertexOperation;
import codechicken.microblock.api.MicroMaterial;
import codechicken.microblock.api.MicroMaterialClient;
import codechicken.microblock.util.MaskedCuboid;
import net.minecraft.client.renderer.RenderType;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

public class FramedJacketedWireModel {

    private final CCModel wireModel;
    private final MaskedCuboid[] wireBoxes;
    private final MaskedCuboid[] highlightBoxes;

    private final List<MaskedCuboid> wireBoxList;
    private final List<MaskedCuboid> highlightBoxList;

    public FramedJacketedWireModel(CCModel wireModel, MaskedCuboid[] wireBoxes, MaskedCuboid[] highlightBoxes) {
        this.wireModel = wireModel;
        this.wireBoxes = wireBoxes;
        this.highlightBoxes = highlightBoxes;

        wireBoxList = Arrays.asList(wireBoxes);
        highlightBoxList = Arrays.asList(highlightBoxes);
    }

    public void renderWire(CCRenderState ccrs, IVertexOperation... ops) {
        wireModel.render(ccrs, ops);
    }

    public void renderMaterial(CCRenderState ccrs, MicroMaterial material, boolean inventory) {
        RenderType layer = inventory ? null : RenderType.solid();
        Objects.requireNonNull(MicroMaterialClient.get(material)).renderCuboids(ccrs, layer, wireBoxList);
    }

    public void renderHighlight(CCRenderState ccrs, MicroMaterial material, boolean inventory) {
        RenderType layer = inventory ? null : RenderType.solid();
        Objects.requireNonNull(MicroMaterialClient.get(material)).renderCuboids(ccrs, layer, highlightBoxList);
    }

    public FramedJacketedWireModel copy() {
        return new FramedJacketedWireModel(wireModel.copy(), wireBoxes, highlightBoxes);
    }
}
