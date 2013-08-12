package mrtjp.projectred.transmission;

import net.minecraft.util.Icon;
import codechicken.lib.lighting.LightModel;
import codechicken.lib.math.MathHelper;
import codechicken.lib.render.CCModel;
import codechicken.lib.render.ColourMultiplier;
import codechicken.lib.render.IUVTransformation;
import codechicken.lib.render.IVertexModifier;
import codechicken.lib.render.IconTransformation;
import codechicken.lib.render.UV;
import codechicken.lib.render.UVScale;
import codechicken.lib.render.UVTranslation;
import codechicken.lib.render.Vertex5;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Scale;
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
        
        public int countConnections() {
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
            
            return conns*3+5;
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
            connCount = countConnections();
            model = CCModel.quadModel(numFaces()*4);
            i=0;
            
            generateCenter();
            for(int r = 0; r < 4; r++)
                generateSide(r);
            
            model.apply(new UVScale(1/32D));
            model.shrinkUVs(0.005);
            model.apply(Rotation.sideOrientation(side, 0).at(Vector3.center));
            model.computeNormals();
            model.computeLighting(LightModel.standardLightModel);
            
            return model;
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

            reflectSide(verts, r);
            return verts;
        }

        private Vertex5[] generateStub(int r) {
            Vertex5[] verts = generateExtension(4);

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
            
            //retexture top
            verts[4].uv.set(8+tw, 8);
            verts[5].uv.set(8-tw, 8);
            verts[6].uv.set(8-tw, 16-tw);
            verts[7].uv.set(8+tw, 16-tw);
            
            reflectSide(verts, r);
            
            //offset side textures
            for(int i = 8; i < 16; i++)
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
                tex = 0;
            else if(connCount == 1)
                tex = (connMask & 5) != 0 ? 0 : 1;//if there is one connection, and it is north/south then north/south, otherwise east/west
            else
                tex = 2;
            
            Vertex5[] verts = new Vertex5[]{
                    new Vertex5(0.5-w, h, 0.5+w, 8-tw, 16+tw),
                    new Vertex5(0.5+w, h, 0.5+w, 8+tw, 16+tw),
                    new Vertex5(0.5+w, h, 0.5-w, 8+tw, 16-tw),
                    new Vertex5(0.5-w, h, 0.5-w, 8-tw, 16-tw)};
            
            IUVTransformation uvt = null;
            if(tex == 1)//straight e/w (rotate texture clockwise 90)
                uvt = new UVT(Rotation.quarterRotations[1].at(new Vector3(8, 0, 16)));
            else if(tex == 2)//circle (translate across to u = 24)
                uvt = new UVTranslation(16, 0);
            
            if(uvt != null)
                for(Vertex5 vert : verts)
                    vert.apply(uvt);
            
            if(inv)
                verts = withBottom(verts, 0, 4);
            
            i = addVerts(model, verts, i);
        }
        
        private static UVT sideReflect = new UVT(new Scale(-1, 1, 1).at(new Vector3(8, 0, 0)));
        private void reflectSide(Vertex5[] verts, int r) {
            if(r >= 2)//invert the texture about the y center
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
            Transformation r = new Rotation(MathHelper.pi, 0, 0, 1).at(new Vector3(0.5, w/2, 0));
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
        int key = connMap&0xF0;//take the straight connections
        key|=(connMap>>20)&0xF;//take the corner render connections and put them in the lowest 4 bits
        int internal = (connMap&0xF00)>>8;//internal connections
        key|=internal<<4|internal;//if internal is set, set both straight and corner to 1
        key|=side<<8;//add side
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
        IVertexModifier m = w.getColour() == -1 ? null : 
            new ColourMultiplier(w.getColour());
        getOrGenerateModel(modelKey(w)).render(
                new Translation(w.x(), w.y(), w.z()), 
                new IconTransformation(w.getIcon()), m);
    }
    
    public static void renderInv(int thickness, Transformation t, Icon icon) {
        CCModel m = invModels[thickness];
        if(m == null)
            invModels[thickness] = m = gen_inst.generateInvModel(thickness);
        
        m.render(t, new IconTransformation(icon));
    }
}
