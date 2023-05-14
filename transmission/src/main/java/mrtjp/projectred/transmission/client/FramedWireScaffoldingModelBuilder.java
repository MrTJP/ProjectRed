package mrtjp.projectred.transmission.client;

import codechicken.lib.render.CCModel;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Vector3;
import codechicken.lib.vec.Vertex5;
import codechicken.lib.vec.uv.UVTransformation;
import mrtjp.projectred.core.UVT;

import static mrtjp.projectred.transmission.client.WireModelBuilder.finishModel;

public class FramedWireScaffoldingModelBuilder {

    private final CCModel[] models = new CCModel[7];

    private final double w = 2 / 8D;
    private final double d = 1 / 16D - 0.002;

//    private int modelKey = 0;
    private boolean modelBuilt = false;

    // TODO: Buliding a single CCModel conforming to this model key may save some rendering overhead at cost
    //       of memory.
//    public FramedWireScaffoldingModelBuilder setModelKey(int key) {
//        this.modelKey = key;
//        return this;
//    }

    public CCModel[] build() {
        if (!modelBuilt) {
            buildModel();
            modelBuilt = true;
            return models;
        }

        CCModel[] modelsCopy = new CCModel[models.length];
        for (int i = 0; i < models.length; i++) {
            modelsCopy[i] = models[i].copy();
        }
        return modelsCopy;
    }

    private void buildModel() {
        generateCenterModel();
        generateSideModels();
        finihModels();
    }

    private void generateCenterModel() {
        CCModel model = CCModel.quadModel(48);
        model.verts[0] = new Vertex5(0.5 - w, 0.5 - w, 0.5 - w, 20, 8);
        model.verts[1] = new Vertex5(0.5 + w, 0.5 - w, 0.5 - w, 28, 8);
        model.verts[2] = new Vertex5(0.5 + w, 0.5 - w, 0.5 + w, 28, 0);
        model.verts[3] = new Vertex5(0.5 - w, 0.5 - w, 0.5 + w, 20, 0);
        model.verts[4] = new Vertex5(0.5 - w, 0.5 - w + d, 0.5 + w, 20, 8);
        model.verts[5] = new Vertex5(0.5 + w, 0.5 - w + d, 0.5 + w, 28, 8);
        model.verts[6] = new Vertex5(0.5 + w, 0.5 - w + d, 0.5 - w, 28, 0);
        model.verts[7] = new Vertex5(0.5 - w, 0.5 - w + d, 0.5 - w, 20, 0);
        model.generateSidedParts(0, Vector3.CENTER);
        models[6] = model;
    }

    private void generateSideModels() {
        CCModel model = CCModel.quadModel(36);
        model.verts[0] = new Vertex5(0.5 - w, 0, 0.5 + w, 16, 0);
        model.verts[1] = new Vertex5(0.5 + w, 0, 0.5 + w, 16, 8);
        model.verts[2] = new Vertex5(0.5 + w, 0.5 - w, 0.5 + w, 20, 8);
        model.verts[3] = new Vertex5(0.5 - w, 0.5 - w, 0.5 + w, 20, 0);
        model.verts[4] = new Vertex5(0.5 + w, 0, 0.5 + w - d, 16, 0);
        model.verts[5] = new Vertex5(0.5 - w, 0, 0.5 + w - d, 16, 8);
        model.verts[6] = new Vertex5(0.5 - w, 0.5 - w, 0.5 + w - d, 20, 8);
        model.verts[7] = new Vertex5(0.5 + w, 0.5 - w, 0.5 + w - d, 20, 0);

        for (int r = 1; r < 4; r++) {
            model.apply(Rotation.quarterRotations[r].at(Vector3.CENTER), 0, r * 8, 8);
        }

        model.verts[32] = new Vertex5(0.5 - w, 0, 0.5 - w, 24, 32);
        model.verts[33] = new Vertex5(0.5 + w, 0, 0.5 - w, 32, 32);
        model.verts[34] = new Vertex5(0.5 + w, 0, 0.5 + w, 32, 24);
        model.verts[35] = new Vertex5(0.5 - w, 0, 0.5 + w, 24, 24);
        models[0] = model;

        UVTransformation t = new UVT(Rotation.quarterRotations[2].at(new Vector3(24, 0, 4)));

        for (int s = 1; s < 6; s++) {
            models[s] = model.copy().apply(Rotation.sideRotations[s].at(Vector3.CENTER));
            if (s % 2 == 1) {
                Vertex5[] verts = models[s].verts;
                for (int i = 0; i < 32; i++) {
                    verts[i].apply(t);
                }
            }
        }
    }

    private void finihModels() {
        for (CCModel model : models) {
            finishModel(model);
        }
    }
}
