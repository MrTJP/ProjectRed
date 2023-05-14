package mrtjp.projectred.transmission.client;

import codechicken.lib.raytracer.IndexedCuboid6;
import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.pipeline.IVertexOperation;
import codechicken.microblock.MicroblockRender;
import codechicken.microblock.api.MicroMaterial;
import net.minecraft.client.renderer.RenderType;

public class FramedJacketedWireModel {

    private final CCModel wireModel;
    private final IndexedCuboid6[] wireBoxes;
    private final IndexedCuboid6[] highlightBoxes;

    public FramedJacketedWireModel(CCModel wireModel, IndexedCuboid6[] wireBoxes, IndexedCuboid6[] highlightBoxes) {
        this.wireModel = wireModel;
        this.wireBoxes = wireBoxes;
        this.highlightBoxes = highlightBoxes;
    }

    public void renderWire(CCRenderState ccrs, IVertexOperation... ops) {
        wireModel.render(ccrs, ops);
    }

    public void renderMaterial(CCRenderState ccrs, MicroMaterial material, boolean inventory) {
        RenderType layer = inventory ? null : RenderType.solid();
        for (IndexedCuboid6 box : wireBoxes) {
            MicroblockRender.renderCuboid(ccrs, material, layer, box, (int) box.data);
        }
    }

    public void renderHighlight(CCRenderState ccrs, MicroMaterial material, boolean inventory) {
        RenderType layer = inventory ? null : RenderType.solid();
        for (IndexedCuboid6 box : highlightBoxes) {
            MicroblockRender.renderCuboid(ccrs, material, layer, box, (int) box.data);
        }
    }

    public FramedJacketedWireModel copy() {
        return new FramedJacketedWireModel(wireModel.copy(), wireBoxes, highlightBoxes);
    }
}
