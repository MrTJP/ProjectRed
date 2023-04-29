package mrtjp.projectred.transmission.client;

import codechicken.lib.raytracer.IndexedCuboid6;
import codechicken.lib.render.CCModel;
import codechicken.lib.vec.*;
import codechicken.lib.vec.uv.UVTransformation;
import codechicken.lib.vec.uv.UVTranslation;
import mrtjp.projectred.transmission.part.BaseCenterWirePart;

import java.util.LinkedList;

import static mrtjp.projectred.transmission.client.FramedWireModelBuilder.*;
import static mrtjp.projectred.transmission.client.WireModelBuilder.*;

public class FramedJacketedWireModelBuilder {

    private int connMap = 0;
    private int tw = 0;
    private double w = 0.0D;
    private int connCount = 0; // Number of sided connections
    private int axisCount = 0; // Number of connection axis used
    private int[] fRotationMasks = new int[6]; // Rotation masks for each face
    private int[] fAxisCounts = new int[6]; // Axis count for each face
    private int i = 0;
    private FramedJacketedWireModel model;

    private int modelKey = 0;
    private boolean modelBuilt = false;

    public FramedJacketedWireModelBuilder setModelKey(int key) {
        this.modelKey = key;
        return this;
    }

    public FramedJacketedWireModel build() {
        if (!modelBuilt) {
            buildModel();
            modelBuilt = true;
            return model;
        }
        return model.copy();
    }

    private void buildModel() {

        connMap = modelKey & 0x3F;
        connCount = FramedWireModelBuilder.countConnections(connMap);
        axisCount = countAxis(connMap);
        for (int s = 0; s < 6; s++) {
            fRotationMasks[s] = calcFaceRotationMask(connMap, s);
            fAxisCounts[s] = countFaceAxis(fRotationMasks[s]);
        }
        int thickness = modelKey >> 6;
        tw = thickness + 1;
        w = tw / 16D + 0.004;
        i = 0;

        CCModel ccModel = generateJacketedWireModel();
        IndexedCuboid6[] jacketedBoxes = generateJacketedBoxes();
        IndexedCuboid6[] highlightBoxes = generateHighlightBoxes();

        model = new FramedJacketedWireModel(ccModel, jacketedBoxes, highlightBoxes);
    }

    private CCModel generateJacketedWireModel() {

        int n = connCount == 0 ? 6 : connCount == 1 ? 2 : connCount;
        CCModel model = CCModel.quadModel(n * 4);
        for (int s = 0; s < 6; s++) {
            generateJacketedSide(model, s);
        }
        finishModel(model);
        return model;
    }

    private void generateJacketedSide(CCModel model, int s) {
        double d = (connMap & 1 << s) != 0 ? 0.00D
                : connCount == 0 ? 0.25
                : connCount == 1 && (connMap & 1 << (s ^ 1)) != 0 ? 0.25D
                : -1;

        if (d == -1) return;

        Vertex5[] verts = faceVerts(s, d - 0.002);
        Transformation t = AxisCycle.cycles[s / 2].at(Vector3.CENTER);
        UVTransformation uvt = new UVTranslation(12, 12);
        for (Vertex5 v : verts) {
            v.apply(t);
            v.apply(uvt);
        }

        i = addVerts(model, verts, i);
    }

    private Vertex5[] faceVerts(int s, double d) {
        Vertex5[] verts = new Vertex5[] {
                new Vertex5(0.5 - w, d, 0.5 - w, 8 - tw, 16 + tw),
                new Vertex5(0.5 + w, d, 0.5 - w, 8 + tw, 16 + tw),
                new Vertex5(0.5 + w, d, 0.5 + w, 8 + tw, 16 - tw),
                new Vertex5(0.5 - w, d, 0.5 + w, 8 - tw, 16 - tw)
        };

        if (s % 2 == 1) {
            Transformation t = new Scale(1, -1, 1).at(Vector3.CENTER);
            apply(t, verts);
            reverseOrder(verts);
        }
        return verts;
    }

    private IndexedCuboid6[] generateJacketedBoxes() {
        if (connCount == 0) return new IndexedCuboid6[] { new IndexedCuboid6(0, BaseCenterWirePart.fOBounds[6]) };

        int n = 0;
        for (int a = 0; a < 3; a++) { if ((connMap & 3 << a * 2) != 0) n += 1; }

        IndexedCuboid6[] boxes = new IndexedCuboid6[n];
        i = 0;

        boolean first = true;
        for (int a = 0; a < 3; a++) {
            if (generateAxialJacketedBoxes(a, first, boxes)) {
                first = false;
            }
        }

        return boxes;
    }

    private IndexedCuboid6[] generateHighlightBoxes() {
        if (connCount == 0) return new IndexedCuboid6[] { new IndexedCuboid6(0, BaseCenterWirePart.fOBounds[6]) };

        LinkedList<IndexedCuboid6> boxes = new LinkedList<>();

        for (int s = 0; s < 6; s++) {
            if ((connMap & 1 << s) != 0) {
                Cuboid6 box = BaseCenterWirePart.fOBounds[0].copy();
                box.apply(Rotation.sideRotations[s].at(Vector3.CENTER));
                int fMask = 1 << (s ^ 1);
                boxes.add(new IndexedCuboid6(fMask, box)); // Cull face opposite of connection, it will be occluded by center box
            }
        }

        // center box
        boxes.add(new IndexedCuboid6(connMap, BaseCenterWirePart.fOBounds[6])); // Cull faces with connections

        return boxes.toArray(new IndexedCuboid6[0]);
    }

    private boolean generateAxialJacketedBoxes(int a, boolean first, IndexedCuboid6[] boxes) {

        int mask = connMap >> a * 2 & 3;
        if (mask == 0) return false;

        Cuboid6 box;
        switch (mask) {
            case 1:
                box = BaseCenterWirePart.fOBounds[0].copy();
                break;
            case 2:
                box = BaseCenterWirePart.fOBounds[1].copy();
                break;
            default:
                box = BaseCenterWirePart.fOBounds[0].copy();
                box.max.y = 1;
        }

        box.apply(Rotation.sideRotations[a * 2].at(Vector3.CENTER));
        if (first) {
            box.enclose(BaseCenterWirePart.fOBounds[6]);
        }

        int fMask = (first || mask == 3) ? 0
                : mask == 1 ? 1 << 2 * a + 1
                : 1 << 2 * a;

        boxes[i++] = new IndexedCuboid6(fMask, box);
        return true;
    }

    //region Utils
    private static void reverseOrder(Vertex5[] verts) {
        int k = 0;
        while (k < verts.length) {
            Vertex5 tmp = verts[k + 1];
            verts[k + 1] = verts[k + 3];
            verts[k + 3] = tmp;
            k += 4;
        }
    }
    //endregion
}
