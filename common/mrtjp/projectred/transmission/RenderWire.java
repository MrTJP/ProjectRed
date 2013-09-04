package mrtjp.projectred.transmission;

import java.util.Arrays;
import java.util.LinkedList;

import net.minecraft.util.Icon;
import codechicken.lib.lighting.LightModel;
import codechicken.lib.math.MathHelper;
import codechicken.lib.render.CCModel;
import codechicken.lib.render.ColourModifier;
import codechicken.lib.render.ColourMultiplier;
import codechicken.lib.render.IUVTransformation;
import codechicken.lib.render.IVertexModifier;
import codechicken.lib.render.IconTransformation;
import codechicken.lib.render.RenderUtils;
import codechicken.lib.render.UV;
import codechicken.lib.render.UVScale;
import codechicken.lib.render.UVTranslation;
import codechicken.lib.render.Vertex5;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.Vector3;

public class RenderWire {
    
    public static class UVT implements IUVTransformation {
        
        public Transformation t;
        private Vector3 vec = new Vector3();
        
        public UVT(Transformation t) {
            this.t = t;
        }
        
        @Override
        public void transform(UV uv)
        {
            vec.set(uv.u, 0, uv.v).apply(t);
            uv.set(vec.x, vec.z);
        }
    }
    
    public static int[] reorientSide = new int[]{
        0, 3, 3, 0, 0, 3};
    
    /* 
     * All generations are done on side 0 so know that for rotation r
     * 0 = side 3 = +Z = SOUTH
     * 1 = side 4 = -X = WEST
     * 2 = side 2 = -Z = NORTH
     * 3 = side 5 = +X = EAST
     */
    
    private static class WireModelGenerator {
        
        int side;
        int tw;
        int th;
        double w;
        double h;
        int mask;
        int connMask;
        int connCount;
        CCModel model;
        int i = 0;
        boolean inv;
        
        public static int countConnections(int connMask) {
            int n = 0;
            for(int r = 0; r < 4; r++)
                if((connMask & 1<<r) != 0)
                    n+=1;
            return n;
        }
        
        public int numFaces() {
            if(inv)
                return 22;
            
            int conns;
            if(connCount <= 2)
                conns = 2;
            else
                conns = connCount;
            
            int faces = conns*3+5;
            for(int i = 0; i < 4; i++)
                if((mask >> i & 0x11) == 1)
                    faces++;
            
            return faces;
        }
        
        public CCModel generateInvModel(int thickness) {
            return generateModel(modelKey(0, thickness, 0xF0), true);
        }

        public CCModel generateModel(int key, boolean inv) {
            this.inv = inv;
            
            side = (key>>8)%6;
            tw = ((key>>8)/6+1);
            w = tw/16D;
            th = tw+1;
            h = th/16D;
            mask = key&0xFF;
            connMask = (mask&0xF0)>>4|mask&0xF;
            connCount = countConnections(connMask);
            model = CCModel.quadModel(numFaces()*4);
            i=0;
            
            generateCenter();
            for(int r = 0; r < 4; r++)
                generateSide(r);
            
            model.apply(Rotation.sideOrientation(side, 0).at(Vector3.center));
            
            return finishModel(model);
        }
        
        private void generateSide(int r) {
            int type = (mask>>r)&0x11;
            
            Vertex5[] verts;
            if(inv) {
                verts = generateSideInv(r);
            }
            else if(connCount == 0) {
                if(r%2 == 1)
                    verts = generateStub(r);
                else
                    verts = generateFlat(r);
            }
            else if(connCount == 1) {
                if(connMask == 1<<((r+2)%4))//this side is opposite the one with a connection
                    verts = generateStub(r);
                else
                    verts = generateSideFromType(type, r);
            }
            else
            {
                verts = generateSideFromType(type, r);
            }
            
            Transformation t = Rotation.quarterRotations[r].at(Vector3.center);
            for(Vertex5 vert : verts)
                vert.apply(t);
            
            i = addVerts(model, verts, i);
        }
        
        private Vertex5[] generateSideInv(int r) {
            return withBottom(generateStraight(r), 4, 4);
        }

        private Vertex5[] generateSideFromType(int type, int r) {
            if(type == 0x00)
                return generateFlat(r);
            else if(type == 0x01)
                return generateCorner(r);
            else if(type == 0x10)
                return generateStraight(r);
            else
                return generateInternal(r);
        }
        
        private Vertex5[] generateFlat(int r) {
            Vertex5[] verts = new Vertex5[]{
                    new Vertex5(0.5-w, 0, 0.5+w, 16, 16+tw),
                    new Vertex5(0.5+w, 0, 0.5+w, 16, 16-tw),
                    new Vertex5(0.5+w, h, 0.5+w, 16-th, 16-tw),
                    new Vertex5(0.5-w, h, 0.5+w, 16-th, 16+tw)};
            
            if(Rotation.rotateSide(side, r)%2 == 0) {//red is on the negative side
                UVT uvt = new UVT(Rotation.quarterRotations[2].at(new Vector3(8, 0, 16)));
                for(Vertex5 vert : verts)
                    vert.apply(uvt);
            }
            return verts;
        }

        private Vertex5[] generateStub(int r) {
            Vertex5[] verts = generateExtension(4);

            for(int i = 0; i < 4; i++)
                verts[i].vec.z-=0.002;//pull the stub in a little so it doesn't z fight with jacketed cables
            
            reflectSide(verts, r);
            return verts;
        }

        private Vertex5[] generateStraight(int r) {
            Vertex5[] verts = generateExtension(8);

            reflectSide(verts, r);
            return verts;
        }

        private Vertex5[] generateCorner(int r) {
            Vertex5[] verts = generateExtension(8+th);
            
            //retexture cap
            for(int i = 0; i < 4; i++)
                verts[i].apply(new UVTranslation(0, -th));
            
            //add end face extending around block
            verts = Arrays.copyOf(verts, 20);
            verts[16] = new Vertex5(0.5-w, 0, 1, 8-tw, 24+2*th);
            verts[17] = new Vertex5(0.5+w, 0, 1, 8+tw, 24+2*th);
            verts[18] = new Vertex5(0.5+w, 0, 1+h, 8+tw, 24+th);
            verts[19] = new Vertex5(0.5-w, 0, 1+h, 8-tw, 24+th);

            reflectSide(verts, r);
            return verts;
        }

        private Vertex5[] generateInternal(int r) {
            Vertex5[] verts = generateExtension(8);

            //retexture cap
            verts[0].uv.set(8+tw, 24);
            verts[1].uv.set(8-tw, 24);
            verts[2].uv.set(8-tw, 24+tw);
            verts[3].uv.set(8+tw, 24+tw);
            
            reflectSide(verts, r);

            //offset side textures
            for(int i = 4; i < 16; i++)
                verts[i].apply(new UVTranslation(16, 0));
            
            return verts;
        }
        
        private Vertex5[] generateExtension(int tl) {
            double l = tl/16D;
            
            return new Vertex5[]{//cap
                    new Vertex5(0.5-w, 0, 0.5+l, 8-tw, 24+2*th),
                    new Vertex5(0.5+w, 0, 0.5+l, 8+tw, 24+2*th),
                    new Vertex5(0.5+w, h, 0.5+l, 8+tw, 24+th),
                    new Vertex5(0.5-w, h, 0.5+l, 8-tw, 24+th),

                    //top
                    new Vertex5(0.5-w, h, 0.5+l, 8-tw, 16+tl),
                    new Vertex5(0.5+w, h, 0.5+l, 8+tw, 16+tl),
                    new Vertex5(0.5+w, h, 0.5+w, 8+tw, 16+tw),
                    new Vertex5(0.5-w, h, 0.5+w, 8-tw, 16+tw),
                    
                    //left
                    new Vertex5(0.5-w, 0, 0.5+w, 0, 16+tw),
                    new Vertex5(0.5-w, 0, 0.5+l, 0, 16+tl),
                    new Vertex5(0.5-w, h, 0.5+l, th, 16+tl),
                    new Vertex5(0.5-w, h, 0.5+w, th, 16+tw),
                    
                    //right
                    new Vertex5(0.5+w, 0, 0.5+l, 16, 16+tl),
                    new Vertex5(0.5+w, 0, 0.5+w, 16, 16+tw),
                    new Vertex5(0.5+w, h, 0.5+w, 16-th, 16+tw),
                    new Vertex5(0.5+w, h, 0.5+l, 16-th, 16+tl)};
        }

        private void generateCenter() {
            int tex;//0 = straight n/s, 1 = straight e/w, 2 = circle
            if(connCount == 0)
                tex = 1;
            else if(connCount == 1)
                tex = (connMask & 5) != 0 ? 0 : 1;//if there is one connection, and it is north/south then north/south, otherwise east/west
            else if(connMask == 5)
                tex = 0;
            else if(connMask == 10)
                tex = 1;
            else
                tex = 2;
            
            Vertex5[] verts = new Vertex5[]{
                    new Vertex5(0.5-w, h, 0.5+w, 8-tw, 16+tw),
                    new Vertex5(0.5+w, h, 0.5+w, 8+tw, 16+tw),
                    new Vertex5(0.5+w, h, 0.5-w, 8+tw, 16-tw),
                    new Vertex5(0.5-w, h, 0.5-w, 8-tw, 16-tw)};

            if(tex == 0 || tex == 1)
                tex = (tex+reorientSide[side])%2;
            
            int r = reorientSide[side];
            if(tex == 1)
                r += 3;
            
            if(r != 0) {
                IUVTransformation uvt = new UVT(Rotation.quarterRotations[r%4].at(new Vector3(8, 0, 16)));
                for(Vertex5 vert : verts)
                    vert.apply(uvt);
            }
            
            if(tex == 2) {//circle (translate across to u = 24)
                UVTranslation uvt = new UVTranslation(16, 0);
                for(Vertex5 vert : verts)
                    vert.apply(uvt);
            }
            
            if(inv)
                verts = withBottom(verts, 0, 4);
            
            i = addVerts(model, verts, i);
        }
        
        private static UVT sideReflect = new UVT(Rotation.quarterRotations[2].at(new Vector3(8, 0, 16)));
        private void reflectSide(Vertex5[] verts, int r) {
            if((r+reorientSide[side])%4 >= 2)//rotate the texture about the y center
                for(Vertex5 vert : verts)
                    vert.apply(sideReflect);
        }
        
        /**
         * Returns a copy of vertices with the bottom face added at the start.
         * @param start The index of the first vertex making up the top face
         * @param count The number of vertices making up the top face
         */
        private Vertex5[] withBottom(Vertex5[] verts, int start, int count) {
            Vertex5[] i_verts = new Vertex5[verts.length+count];

            //add the bottom face, just a copy of the top, rotated about the z axis
            Transformation r = new Rotation(MathHelper.pi, 0, 0, 1).at(new Vector3(0.5, h/2, 0));
            for(int i = 0; i < count; i++)
                i_verts[i] = verts[i+start].copy().apply(r);
            System.arraycopy(verts, 0, i_verts, count, verts.length);
            return i_verts;
        }
    }
    
    /**
     * Array of all built models. These will be generated on demand.
     */
    public static CCModel[] wireModels = new CCModel[3*6*256];
    public static CCModel[] invModels = new CCModel[3];
    private static WireModelGenerator gen_inst = new WireModelGenerator();
    
    /**
     * Puts verts into model m starting at index k
     */
    public static int addVerts(CCModel m, Vertex5[] verts, int k) {
        for(int i = 0; i < verts.length; i++)
            m.verts[k+i] = verts[i];
        
        return k+verts.length;
    }
    
    public static CCModel finishModel(CCModel m) {
        m.apply(new UVScale(1/32D));
        m.shrinkUVs(0.0005);
        m.computeNormals();
        m.computeLighting(LightModel.standardLightModel);
        
        return m;
    }
    
    /**
     * Returns a tightly packed unique index for the specific model represented by this wire.
     * The mask is split into 3 sections the combination of corresponding bits from the two lowest nybbles gives the connection type in that direction.
     * 00 = none
     * 01 = corner
     * 10 = straight
     * 11 = internal
     * The second byte contains the thickness*6+side
     * 
     * @param side The side the wire is attached to
     * @param thickness The thickness of the wire -1 in 1/8th blocks. Supported values 0, 1, 2
     * @param connMap The connection mask of the wire
     */
    public static int modelKey(int side, int thickness, int connMap) {
        int key = connMap&0xFF;//take the straight and corner connections
        
        int renderCorner = (connMap>>20)&0xF;
        key|=(renderCorner^(key&0xF))<<4;//any corner connections that aren't rendered convert to straight
        key&= ~0xF | renderCorner;//set corners to renderCorners
        
        int internal = (connMap&0xF00)>>8;//internal connections
        key|=internal<<4|internal;//if internal is set, set both straight and corner to 1
        
        key|=(side+thickness*6)<<8;//add side and thickness
        return key;
    }
    
    public static int modelKey(WirePart w) {
        return modelKey(w.side, w.getThickness(), w.connMap);
    }

    public static CCModel getOrGenerateModel(int key) {
        CCModel m = wireModels[key];
        if(m == null)
            wireModels[key] = m = gen_inst.generateModel(key, false);
        return m;
    }
    
    public static void render(WirePart w) {
        IVertexModifier m = w.getColour() == -1 ? ColourModifier.instance : new ColourMultiplier(w.getColour());
        getOrGenerateModel(modelKey(w)).render(new Translation(w.x(), w.y(), w.z()), new IconTransformation(w.getIcon()), m);
    }
    
    public static void renderInv(int thickness, Transformation t, Icon icon) {
        CCModel m = invModels[thickness];
        if(m == null)
            invModels[thickness] = m = gen_inst.generateInvModel(thickness);
        
        m.render(t, new IconTransformation(icon));
    }
    
    public static void renderBreakingOverlay(Icon icon, WirePart wire) {
        int key = modelKey(wire);
        int side = (key>>8)%6;
        double w = ((key>>8)/6+1)/16D;
        double h = w + 1/16D;
        int mask = key&0xFF;
        int connMask = (mask&0xF0)>>4|mask&0xF;
        int connCount = WireModelGenerator.countConnections(connMask);
        
        LinkedList<Cuboid6> boxes = new LinkedList<Cuboid6>();
        boxes.add(new Cuboid6(0.5-w, 0, 0.5-w, 0.5+w, h, 0.5+w)
            .apply(Rotation.sideRotations[side].at(Vector3.center)));//center
        for(int r = 0; r < 4; r++) {
            int length;
            if(connCount == 0) {
                if(r%2 == 1)
                    length = 4;
                else
                    length = 0;
            }
            else if(connCount == 1) {
                if(connMask == 1<<((r+2)%4))//this side is opposite the one with a connection
                    length = 4;
                else if(connMask == 1<<r)
                    length = 8;
                else
                    length = 0;
            }
            else
            {
                length = (connMask & 1<<r) != 0 ? 8 : 0;
            }
            
            if(length > 0) {
                double l = length/16D;
                boxes.add(new Cuboid6(0.5-w, 0, 0.5+w, 0.5+w, h, 0.5+l)
                        .apply(Rotation.sideOrientation(side, r).at(Vector3.center)));
            }
        }
        
        for(Cuboid6 box : boxes) {
            RenderUtils.renderBlock(box, 0, new Translation(wire.x(), wire.y(), wire.z()), new IconTransformation(icon), null);
        }
    }
}
