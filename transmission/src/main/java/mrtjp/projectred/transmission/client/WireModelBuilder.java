package mrtjp.projectred.transmission.client;

import codechicken.lib.math.MathHelper;
import codechicken.lib.render.CCModel;
import codechicken.lib.render.lighting.LightModel;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Vector3;
import codechicken.lib.vec.Vertex5;
import codechicken.lib.vec.uv.UVScale;
import codechicken.lib.vec.uv.UVTransformation;
import codechicken.lib.vec.uv.UVTranslation;
import mrtjp.projectred.core.BundledSignalsLib;
import mrtjp.projectred.core.UVT;

import java.util.Arrays;

public class WireModelBuilder {

    private static final UVTransformation ROTATE_WIRE_UV_180 = new UVT(Rotation.quarterRotations[2].at(new Vector3(8, 0, 16)));

    private int side = 0;
    private int tw = 0;
    private int th = 0;
    private double w = 0.0D;
    private double h = 0.0D;
    private int mask = 0;
    private int connMask = 0;
    private int connCount = 0;
    private CCModel model = null;
    private int i = 0;

    private int modelKey = 0;
    private boolean inv = false;
    private boolean modelBuilt = false;

    public WireModelBuilder setInventory(boolean isInventory) {
        this.inv = isInventory;
        return this;
    }

    public WireModelBuilder setModelKey(int modelKey) {
        this.modelKey = modelKey;
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
        side = (modelKey >> 8) % 6;
        tw = (modelKey >> 8) / 6 + 1;
        w = tw / 16D;
        th = tw + 1;
        h = th / 16D;
        mask = modelKey & 0xFF;
        connMask = (mask & 0xF0) >> 4 | mask & 0xF;
        connCount = FramedWireModelBuilder.countConnections(connMask);
        model = CCModel.quadModel(countNumberOfFaces() * 4);
        i = 0;

        generateCenter();
        for (int r = 0; r < 4; r++) {
            generateSide(r);
        }
        model.apply(Rotation.sideOrientation(side, 0).at(Vector3.CENTER));
        finishModel(model);
    }

    private int countNumberOfFaces() {
        if (inv) return 22;

        int conns = Math.max(connCount, 2);
        int faces = conns * 3 + 5;
        for (int i = 0; i < 4; i++) {
            if ((mask >> i & 0x11) == 1) { faces += 1; }
        }
        return faces;
    }

    private void generateCenter() {

        int tex; //0 = straight n/s, 1 = straight e/w, 2 = circle
        switch (connCount) {
            case 0:
                tex = 1;
                break;
            case 1:
                tex = (connMask & 5) != 0 ? 0 : 1;
                break;
            default:
                tex = connMask == 5 ? 0 : connMask == 10 ? 1 : 2;
        }

        Vertex5 verts[] = new Vertex5[] {
                new Vertex5(0.5 - w, h, 0.5 + w, 8 - tw, 16 + tw),
                new Vertex5(0.5 + w, h, 0.5 + w, 8 + tw, 16 + tw),
                new Vertex5(0.5 + w, h, 0.5 - w, 8 + tw, 16 - tw),
                new Vertex5(0.5 - w, h, 0.5 - w, 8 - tw, 16 - tw)
        };

        int r = BundledSignalsLib.bundledCableBaseRotationMap[side];

        if (tex == 0 || tex == 1) tex = (tex + r) % 2;
        if (tex == 1) r += 3;

        if (r != 0) {
            UVT uvt = new UVT(Rotation.quarterRotations[r % 4].at(new Vector3(8, 0, 16)));
            apply(uvt, verts);
        }

        if (tex == 2) {
            UVTranslation uvt = new UVTranslation(16, 0);
            apply(uvt, verts);
        }

        if (inv) verts = withBottom(verts, 0, 4);
        i = addVerts(model, verts, i);
    }

    private void generateSide(int r) {

        int stype = (mask >> r) & 0x11;

        Vertex5[] verts;

        if (inv) {
            verts = genrateSideInv(r);
        } else {
            switch (connCount) {
                case 0:
                    verts = r % 2 == 1 ? generateStub(r) : generateFlat(r);
                    break;
                case 1:
                    if (connMask == (1 << (r + 2) % 4)) {
                        verts = generateStub(r);
                        break;
                    }
                    //fallthrough
                default:
                    verts = generateSideFromType(stype, r);
            }
        }

        Transformation t = Rotation.quarterRotations[r].at(Vector3.CENTER);
        apply(t, verts);
        i = addVerts(model, verts, i);
    }

    private Vertex5[] genrateSideInv(int r) {
        return withBottom(generateStraight(r), 4, 4);
    }

    private Vertex5[] generateStraight(int r) {
        Vertex5[] verts = generateExtension(8);
        reflectSide(verts, r);
        return verts;
    }

    private Vertex5[] generateExtension(int tl) {
        double l = tl / 16D;
        return new Vertex5[] {
                //cap
                new Vertex5(0.5 - w, 0, 0.5 + l, 8 - tw, 24 + 2 * th),
                new Vertex5(0.5 + w, 0, 0.5 + l, 8 + tw, 24 + 2 * th),
                new Vertex5(0.5 + w, h, 0.5 + l, 8 + tw, 24 + th),
                new Vertex5(0.5 - w, h, 0.5 + l, 8 - tw, 24 + th),
                //top
                new Vertex5(0.5 - w, h, 0.5 + l, 8 - tw, 16 + tl),
                new Vertex5(0.5 + w, h, 0.5 + l, 8 + tw, 16 + tl),
                new Vertex5(0.5 + w, h, 0.5 + w, 8 + tw, 16 + tw),
                new Vertex5(0.5 - w, h, 0.5 + w, 8 - tw, 16 + tw),
                //left
                new Vertex5(0.5 - w, 0, 0.5 + w, 0, 16 + tw),
                new Vertex5(0.5 - w, 0, 0.5 + l, 0, 16 + tl),
                new Vertex5(0.5 - w, h, 0.5 + l, th, 16 + tl),
                new Vertex5(0.5 - w, h, 0.5 + w, th, 16 + tw),
                //right
                new Vertex5(0.5 + w, 0, 0.5 + l, 16, 16 + tl),
                new Vertex5(0.5 + w, 0, 0.5 + w, 16, 16 + tw),
                new Vertex5(0.5 + w, h, 0.5 + w, 16 - th, 16 + tw),
                new Vertex5(0.5 + w, h, 0.5 + l, 16 - th, 16 + tl)
        };
    }

    private Vertex5[] generateStub(int r) {
        Vertex5[] verts = generateExtension(4);
        for (int i = 0; i < 4; i++) {
            verts[i].vec.z -= 0.002; //pull the stub in a little so it doesn't z fight with framed cables
        }

        reflectSide(verts, r);
        return verts;
    }

    private Vertex5[] generateFlat(int r) {
        Vertex5[] verts = new Vertex5[] {
                new Vertex5(0.5 - w, 0, 0.5 + w, 16, 16 + tw),
                new Vertex5(0.5 + w, 0, 0.5 + w, 16, 16 - tw),
                new Vertex5(0.5 + w, h, 0.5 + w, 16 - th, 16 - tw),
                new Vertex5(0.5 - w, h, 0.5 + w, 16 - th, 16 + tw)
        };

        if (Rotation.rotateSide(side, r) % 2 == 0) { //red is on the negative side
            apply(ROTATE_WIRE_UV_180, verts);
        }

        return verts;
    }

    private Vertex5[] generateCorner(int r) {

        Vertex5[] verts = generateExtension(8 + th);

        // retexture cap
        apply(new UVTranslation(0, -th), verts, 0, 4);

        // add end face extending around block
        verts = Arrays.copyOf(verts, 20);
        verts[16] = new Vertex5(0.5 - w, 0, 1, 8 - tw, 24 + 2 * th);
        verts[17] = new Vertex5(0.5 + w, 0, 1, 8 + tw, 24 + 2 * th);
        verts[18] = new Vertex5(0.5 + w, 0, 1 + h, 8 + tw, 24 + th);
        verts[19] = new Vertex5(0.5 - w, 0, 1 + h, 8 - tw, 24 + th);

        reflectSide(verts, r);
        return verts;
    }

    private Vertex5[] generateInternal(int r) {
        Vertex5[] verts = generateExtension(8);

        // retexture cap
        verts[0].uv.set(8 + tw, 24);
        verts[1].uv.set(8 - tw, 24);
        verts[2].uv.set(8 - tw, 24 + tw);
        verts[3].uv.set(8 + tw, 24 + tw);

        //offset side texture
        reflectSide(verts, r);
        apply(new UVTranslation(16, 0), verts, 4, 16);

        return verts;
    }

    private Vertex5[] generateSideFromType(int stype, int r) {
        switch (stype) {
            case 0x00:
                return generateFlat(r);
            case 0x01:
                return generateCorner(r);
            case 0x10:
                return generateStraight(r);
            default:
                return generateInternal(r);
        }
    }

    //region Utils
    private Vertex5[] withBottom(Vertex5[] verts, int start, int count) {

        Vertex5[] newVerts = new Vertex5[verts.length + count];
        Transformation r = new Rotation(MathHelper.pi, 0, 0, 1).at(new Vector3(0.5, h / 2, 0));

        for (int i = 0; i < count; i++) {
            newVerts[i] = verts[i + start].copy().apply(r);
        }
        System.arraycopy(verts, 0, newVerts, count, verts.length);

        return newVerts;
    }

    private void reflectSide(Vertex5[] verts, int r) {
        if ((r + BundledSignalsLib.bundledCableBaseRotationMap[side]) % 4 >= 2) {
            apply(ROTATE_WIRE_UV_180, verts);
        }
    }

    public static int countConnections(int mask) {
        int n = 0;
        for (int r = 0; r < 4; r++) {
            if ((mask & (1 << r)) != 0) {
                n++;
            }
        }
        return n;
    }

    public static int addVerts(CCModel model, Vertex5[] verts, int k) {
        for (int i = 0; i < verts.length; i++) {
            model.verts[k + i] = verts[i];
        }
        return k + verts.length;
    }

    public static void apply(Transformation t, Vertex5[] verts) {
        apply(t, verts, 0, verts.length);
    }

    public static void apply(UVTransformation t, Vertex5[] verts) {
        apply(t, verts, 0, verts.length);
    }

    public static void apply(Transformation t, Vertex5[] verts, int start, int end) {
        for (int i = start; i < end; i++) {
            verts[i].apply(t);
        }
    }

    public static void apply(UVTransformation t, Vertex5[] verts, int start, int end) {
        for (int i = start; i < end; i++) {
            verts[i].apply(t);
        }
    }

    public static void apply(Transformation t, CCModel model, int start, int end) {
        apply(t, model.verts, start, end);
    }

    public static void finishModel(CCModel model) {
        model.apply(new UVScale(1 / 32D));
        model.shrinkUVs(0.0005);
        model.computeNormals();
        model.computeLighting(LightModel.standardLightModel);
    }
    //endregion
}
