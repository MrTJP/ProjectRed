package mrtjp.projectred.transmission.client;

import codechicken.lib.render.CCModel;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Vector3;
import codechicken.lib.vec.Vertex5;
import codechicken.lib.vec.uv.UV;
import codechicken.lib.vec.uv.UVScale;
import codechicken.lib.vec.uv.UVTransformation;
import codechicken.lib.vec.uv.UVTranslation;
import mrtjp.projectred.core.PRLib;
import mrtjp.projectred.core.UVT;

import static mrtjp.projectred.transmission.client.WireModelBuilder.*;

public class FramedWireModelBuilder {

    private static final UVTransformation ROTATE_WIRE_UV_180 = new UVT(Rotation.quarterRotations[2].at(new Vector3(8, 0, 16)));

    private int connMap = 0;
    private int tw = 0;
    private double w = 0.0D;
    private int connCount = 0; // Number of sided connections
    private int axisCount = 0; // Number of connection axis used
    private int[] fRotationMasks = new int[6]; // Rotation masks for each face
    private int[] fAxisCounts = new int[6]; // Axis count for each face
    private int i = 0;
    private CCModel model = null;

    private int modelKey = 0;
    private boolean modelBuilt = false;

    public FramedWireModelBuilder setModelKey(int key) {
        this.modelKey = key;
        return this;
    }

    public CCModel build() {
        if (!modelBuilt) {
            buildModel();
            modelBuilt = true;
            return model;
        }
        return model.copy();
    }

    private void buildModel() {
        connMap = modelKey & 0x3F;
        connCount = countConnections(connMap);
        axisCount = countAxis(connMap);
        for (int s = 0; s < 6; s++) {
            fRotationMasks[s] = calcFaceRotationMask(connMap, s);
            fAxisCounts[s] = countFaceAxis(fRotationMasks[s]);
        }
        int thickness = modelKey >> 6;
        tw = thickness + 1;
        w = tw / 16D + 0.004;
        i = 0;

        // Count number of verts required based on model spec
        int axisVertCount = axisCount * 16; // Each axis (up/down, north/south, west/east) require 4 faces each
        int capVertCount = connCount * 4;   // Cap at end of connections reaching out of the block
        int stubVertCount = 0;              // Stubs when no rotational conns on a face
        int circleVertCount = 0;            // Circular cross texture on faces with both vertical and horizontal axis
        for (int s = 0; s < 6; s++) {
            if (fAxisCounts[s] == 0) stubVertCount += 4;
            if (fAxisCounts[s] == 2) circleVertCount += 4;
        }

        model = CCModel.quadModel(axisVertCount + stubVertCount + circleVertCount + capVertCount);
        for (int s = 0; s < 6; s++) {
            generateFace(s);
        }
        finishModel(model);
    }

    private void generateFace(int s) {

        int start = i;
        i = addVerts(model, generateFaceAxisVerts(s), i); // Verts for conns perpendicular to 's' axis

        switch (fAxisCounts[s]) {
            case 0:
                i = addVerts(model, generateFaceStubVerts(s, 0.5 - w), i); // caps at center
                break;
            case 2:
                i = addVerts(model, generateFaceCircleVerts(s), i); // circular texture showing crossed axis
                break;
            default:
        }

        if ((connMap & 1 << (s ^ 1)) != 0) { // Verts for caps
            i = addVerts(model, generateFaceStubVerts(s, 0), i);
        }

        Transformation t = Rotation.sideOrientation(s, 0).at(Vector3.CENTER);
        apply(t, model, start, i); // Transform generated model partial to side s
    }

    private Vertex5[] generateFaceAxisVerts(int s) {
        double d = 0.5 - w;

        int numAxis = fAxisCounts[s];
        int fMask = fRotationMasks[s];

        Vertex5[] verts = new Vertex5[numAxis * 4];
        int vi = 0;

        if ((fMask & 0x5) != 0) { // Vertical axis looking at the face (i.e. looking down on face 0, north/south conns)
            Vertex5[] aVerts = axisVerts(fMask, d);
            reflectSide(aVerts, s, 0);
            System.arraycopy(aVerts, 0, verts, vi, 4);
            vi += 4;
        }

        if ((fMask & 0xA) != 0) { // Horizontal axis looking at the face (i.e. looking down on face 0, east/west conns)
            Vertex5[] aVerts = axisVerts(fMask >> 1, d);
            reflectSide(aVerts, s, 1);
            Transformation t = Rotation.quarterRotations[1].at(Vector3.CENTER);
            apply(t, aVerts);
            System.arraycopy(aVerts, 0, verts, vi, 4);
            vi += 4;
        }
        return verts;
    }

    private Vertex5[] axisVerts(int mask, double d) {
        int tl = 8 - tw;
        double l = tl / 16D + 0.004;
        double zn = (mask & 0x1) != 0 ? l : 0;
        double vn = (mask & 0x1) != 0 ? tl : 0;
        double zp = (mask & 0x4) != 0 ? l : 0;
        double vp = (mask & 0x4) != 0 ? tl : 0;

        return new Vertex5[] {
                new Vertex5(0.5 - w, 1 - d, 0.5 + w + zp, 8 - tw, 16 + tw + vp),
                new Vertex5(0.5 + w, 1 - d, 0.5 + w + zp, 8 + tw, 16 + tw + vp),
                new Vertex5(0.5 + w, 1 - d, 0.5 - w - zn, 8 + tw, 16 - tw - vn),
                new Vertex5(0.5 - w, 1 - d, 0.5 - w - zn, 8 - tw, 16 - tw - vn)
        };
    }

    private Vertex5[] generateFaceStubVerts(int s, double d) {
        Vertex5[] aVerts = axisVerts(0, d);
        UVTransformation t = new UVTranslation(12, 12);
        apply(t, aVerts);

        if (s % 2 == 1) { // Reflect the stub on opposite sides
            UVTransformation ft = new UVScale(-1, 1).at(new UV(20, 28));
            apply(ft, aVerts);
        }
        return aVerts;
    }

    private Vertex5[] generateFaceCircleVerts(int s) {
        double d = 0.5 - w;
        Vertex5[] aVerts = axisVerts(0, d - 0.002); //Offset for zfighting
        rotateSide(aVerts, s);
        UVTransformation t = new UVTranslation(16, 0);
        apply(t, aVerts);
        return aVerts;
    }

    //region Utils
    public static int countConnections(int connMask) {
        int count = 0;
        for (int s = 0; s < 6; s++) {
            if ((connMask & (1 << s)) != 0) {
                count++;
            }
        }
        return count;
    }

    public static int countAxis(int connMask) {
        int count = 0;
        for (int a = 0; a < 3; a++) {
            if ((connMask & (0x3 << (a * 2))) != 0) {
                count++;
            }
        }
        return count;
    }

    public static int calcFaceRotationMask(int connMap, int s) {
        int rMask = 0;
        for (int r = 0; r < 4; r++) {
            int absSide = Rotation.rotateSide(s, (r + 2) % 4);
            if ((connMap & 1 << absSide) != 0) {
                rMask |= 1 << r;
            }
        }
        return rMask;
    }

    public static int countFaceAxis(int faceRotMask) {
        int a = 0;
        if ((faceRotMask & 0x5) != 0) a += 1;
        if ((faceRotMask & 0xA) != 0) a += 1;
        return a;
    }

    private void reflectSide(Vertex5[] verts, int s, int r) {
        if ((r + PRLib.bundledCableBaseRotationMap()[s]) % 4 >= 2) {
            apply(ROTATE_WIRE_UV_180, verts);
        }
    }

    private void rotateSide(Vertex5[] verts, int s) {
        int r = PRLib.bundledCableBaseRotationMap()[s];
        UVTransformation uvt = new UVT(Rotation.quarterRotations[r % 4].at(new Vector3(8, 0, 16)));
        apply(uvt, verts);
    }
    //endregion
}
