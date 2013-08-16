package mrtjp.projectred.transmission;

import net.minecraft.util.Icon;
import net.minecraft.world.World;
import codechicken.lib.lighting.LazyLightMatrix;
import codechicken.lib.raytracer.IndexedCuboid6;
import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.ColourModifier;
import codechicken.lib.render.ColourMultiplier;
import codechicken.lib.render.IUVTransformation;
import codechicken.lib.render.IVertexModifier;
import codechicken.lib.render.IconTransformation;
import codechicken.lib.render.RenderUtils;
import codechicken.lib.render.UVTranslation;
import codechicken.lib.render.Vertex5;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.Vector3;
import codechicken.microblock.IMicroMaterialRender;
import codechicken.microblock.JMicroblockClient;
import codechicken.microblock.MicroMaterialRegistry;
import codechicken.microblock.MicroMaterialRegistry.IMicroMaterial;

import static mrtjp.projectred.transmission.RenderWire.UVT;
import static mrtjp.projectred.transmission.RenderWire.addVerts;
import static mrtjp.projectred.transmission.RenderWire.finishModel;
import static mrtjp.projectred.transmission.ScaffoldWirePart.boundingBoxes;

public class RenderScaffoldWire
{
    private static class ScaffoldModelGenerator
    {
        double w = 2/8D;
        double d = 1/16D-0.002;//little offset for compensating for the slight uv stretch to eliminate seams
        
        public static void generateModels() {
            ScaffoldModelGenerator gen_inst = new ScaffoldModelGenerator();
            gen_inst.generateCenterModel();
            gen_inst.generateSideModels();
            
            gen_inst.finishModels();
        }
        
        public void generateCenterModel() {
            CCModel model = CCModel.quadModel(48);
            
            model.verts[0] = new Vertex5(0.5-w, 0.5-w, 0.5-w, 20, 8);
            model.verts[1] = new Vertex5(0.5+w, 0.5-w, 0.5-w, 28, 8);
            model.verts[2] = new Vertex5(0.5+w, 0.5-w, 0.5+w, 28, 0);
            model.verts[3] = new Vertex5(0.5-w, 0.5-w, 0.5+w, 20, 0);
            
            model.verts[4] = new Vertex5(0.5-w, 0.5-w+d, 0.5+w, 20, 8);
            model.verts[5] = new Vertex5(0.5+w, 0.5-w+d, 0.5+w, 28, 8);
            model.verts[6] = new Vertex5(0.5+w, 0.5-w+d, 0.5-w, 28, 0);
            model.verts[7] = new Vertex5(0.5-w, 0.5-w+d, 0.5-w, 20, 0);
            
            model.generateSidedParts(0, Vector3.center);
            scaffoldModels[6] = model;
        }
        
        public void generateSideModels() {
            CCModel model = CCModel.quadModel(36);
            
            model.verts[0] = new Vertex5(0.5-w, 0, 0.5+w, 16, 0);
            model.verts[1] = new Vertex5(0.5+w, 0, 0.5+w, 16, 8);
            model.verts[2] = new Vertex5(0.5+w, 0.5-w, 0.5+w, 20, 8);
            model.verts[3] = new Vertex5(0.5-w, 0.5-w, 0.5+w, 20, 0);
            
            model.verts[4] = new Vertex5(0.5+w, 0, 0.5+w-d, 16, 0);
            model.verts[5] = new Vertex5(0.5-w, 0, 0.5+w-d, 16, 8);
            model.verts[6] = new Vertex5(0.5-w, 0.5-w, 0.5+w-d, 20, 8);
            model.verts[7] = new Vertex5(0.5+w, 0.5-w, 0.5+w-d, 20, 0);
            
            for(int r = 1; r < 4; r++)
                model.apply(Rotation.quarterRotations[r].at(Vector3.center), 0, r*8, 8);

            model.verts[32] = new Vertex5(0.5-w, 0, 0.5-w, 24, 32);
            model.verts[33] = new Vertex5(0.5+w, 0, 0.5-w, 32, 32);
            model.verts[34] = new Vertex5(0.5+w, 0, 0.5+w, 32, 24);
            model.verts[35] = new Vertex5(0.5-w, 0, 0.5+w, 24, 24);
            
            scaffoldModels[0] = model;
            for(int s = 1; s < 6; s++) {
                scaffoldModels[s] = model.copy().apply(Rotation.sideRotations[s].at(Vector3.center));
                
                if(s%2 == 1) {
                    Vertex5[] verts = scaffoldModels[s].verts;
                    UVT t = new UVT(Rotation.quarterRotations[2].at(new Vector3(24, 0, 4)));
                    for(int i = 0; i < 32; i++)
                        verts[i].apply(t);
                }
            }
        }
        
        public void finishModels() {
            for(CCModel m : scaffoldModels)
                finishModel(m);
        }
    }
    
    private static class JacketedModel implements IMicroMaterialRender
    {
        public CCModel wireModel;
        public IndexedCuboid6[] boxes;
        public Cuboid6 bounds;
        
        private ScaffoldWirePart w;
        
        public JacketedModel(CCModel model, IndexedCuboid6[] boxes) {
            wireModel = model;
            this.boxes = boxes;
            bounds = boxes[0].copy();
            for(int i = 1; i < boxes.length; i++)
                bounds.enclose(boxes[i]);
        }
        
        public void render(ScaffoldWirePart w, LazyLightMatrix olm, IUVTransformation uvt, IVertexModifier m, int mat) {
            
            this.w = w;
            
            IMicroMaterial material = MicroMaterialRegistry.getMaterial(mat);
            Vector3 pos = new Vector3(w.x(), w.y(), w.z());
            for(IndexedCuboid6 box : boxes)
                JMicroblockClient.renderCuboid(pos, olm, material, box, (Integer)box.data, this);
            
            wireModel.render(pos.translation(), uvt, m);
        }
        
        @Override
        public Cuboid6 getRenderBounds() {
            return bounds;
        }
        
        @Override
        public int x() {
            return w.x();
        }
        
        @Override
        public int y() {
            return w.y();
        }
        
        @Override
        public int z() {
            return w.z();
        }
        
        @Override
        public World world() {
            return w.world();
        }
    }
    
    private static class WireModelGenerator
    {
        int connMap;
        int tw;
        double w;
        int connCount;
        int i;
        CCModel model;
        
        public static int countConnections(int connMap) {
            int n = 0;
            for(int s = 0; s < 6; s++)
                if((connMap & 1<<s) != 0)
                    n+=1;
            return n;
        }
                
        private void setup(int key) {
            connMap = key&0x3F;
            connCount = countConnections(connMap);
            int thickness = key>>6;
            tw = thickness+1;
            w = tw/16D+0.004;
            i = 0;
        }
        
        public CCModel generateWireModel(int key) {
            setup(key);
            model = CCModel.quadModel(connCount*16+24);
            
            for(int s = 0; s < 6; s++)
                generateSide(s);
            
            return finishModel(model);
        }

        private void generateSide(int s) {
            Vertex5[] verts;
            if(connCount == 0) {
                verts = generateStub(s);
            }
            else if(connCount == 1) {
                if((connMap & 1<<(s^1)) != 0)
                    verts = generateStub(s);
                else
                    verts = generateSideFromType(s);
            }
            else {
                verts = generateSideFromType(s);
            }
            
            Transformation t = Rotation.sideRotations[s].at(Vector3.center);
            for(Vertex5 vert : verts)
                vert.apply(t);
            
            i = addVerts(model, verts, i);
        }

        private Vertex5[] generateSideFromType(int s) {
            if((connMap & 1<<s) != 0)
                return generateStraight(s);
            
            return generateFlat(s);
        }

        private Vertex5[] generateStraight(int s) {
            Vertex5[] verts = new Vertex5[20];
            System.arraycopy(faceVerts(0), 0, verts, 0, 4);

            verts[4] = new Vertex5(0.5-w, 0, 0.5+w, 8-tw, 24);
            verts[5] = new Vertex5(0.5+w, 0, 0.5+w, 8+tw, 24);
            verts[6] = new Vertex5(0.5+w, 0.5-w, 0.5+w, 8+tw, 16+tw);
            verts[7] = new Vertex5(0.5-w, 0.5-w, 0.5+w, 8-tw, 16+tw);
            
            for(int r = 1; r < 4; r++) {
                Transformation t = Rotation.quarterRotations[r].at(Vector3.center);
                for(int i = 0; i < 4; i++)
                    verts[i+r*4+4] = verts[i+4].copy().apply(t);
            }
            
            reflectSide(verts, s);
            
            UVTranslation t = new UVTranslation(12, 12);
            for(int i = 0; i < 4; i++)
                verts[i].apply(t);
            
            return verts;
        }

        private Vertex5[] generateFlat(int s) {
            Vertex5[] verts = faceVerts(0.5-w);
            //TODO: rotation stuffs
            return verts;
        }

        private Vertex5[] generateStub(int s) {
            Vertex5[] verts = faceVerts(0.5-w);
            reflectSide(verts, s);
            
            UVTranslation t = new UVTranslation(12, 12);
            for(Vertex5 vert : verts)
                vert.apply(t);
            
            return verts;
        }
        
        private Vertex5[] faceVerts(double d) {
            return new Vertex5[]{
                    new Vertex5(0.5-w, d, 0.5-w, 8-tw, 16+tw),
                    new Vertex5(0.5+w, d, 0.5-w, 8+tw, 16+tw),
                    new Vertex5(0.5+w, d, 0.5+w, 8+tw, 16-tw),
                    new Vertex5(0.5-w, d, 0.5+w, 8-tw, 16-tw)};
        }

        private static UVT sideReflect = new UVT(Rotation.quarterRotations[2].at(new Vector3(8, 0, 16)));
        private void reflectSide(Vertex5[] verts, int s) {
            if(s%2 != 0)//rotate the texture about the y center
                for(Vertex5 vert : verts)
                    vert.apply(sideReflect);
        }

        public JacketedModel generateJacketedModel(int key) {
            setup(key);
            
            return new JacketedModel(generateJacketedWireModel(), generateJacketedBoxes());
        }

        private IndexedCuboid6[] generateJacketedBoxes() {
            if(connCount == 0)
                return new IndexedCuboid6[]{new IndexedCuboid6(0, boundingBoxes[6])};
            
            int n = 0;
            for(int a = 0; a < 3; a++)
                if((connMap & 3<<(a*2)) != 0)
                    n++;
            
            IndexedCuboid6[] boxes = new IndexedCuboid6[n];
            i = 0;
            
            boolean first = true;
            for(int a = 0; a < 3; a++)
                first = !generateAxialJacketBoxes(a, first, boxes);
            
            return boxes;
        }

        private boolean generateAxialJacketBoxes(int a, boolean first, IndexedCuboid6[] boxes) {
            int mask = connMap>>(a*2) & 3;
            if(mask == 0)
                return false;
            
            Cuboid6 box;
            if(mask == 1)
                box = boundingBoxes[0].copy();
            else if(mask == 2)
                box = boundingBoxes[1].copy();
            else {//mask == 3
                box = boundingBoxes[0].copy();
                box.max.y = 1;
            }
            box.apply(Rotation.sideRotations[a*2].at(Vector3.center));
            
            if(first)
                box.enclose(boundingBoxes[6]);
            
            int fMask = 0;
            if(first || mask == 3)
                fMask = 0;
            else if(mask == 1)
                fMask = 1<<(2*a+1);
            else//mask == 2
                fMask = 1<<(2*a);
            
            boxes[i] = new IndexedCuboid6(fMask, box);
            i++;
            return true;
        }

        private CCModel generateJacketedWireModel() {
            int n;
            if(connCount == 0)
                n = 6;
            else if(connCount == 1)
                n = 2;
            else
                n = connCount;
            model = CCModel.quadModel(n*4);
            
            for(int s = 0; s < 6; s++)
                generateJacketedSide(s);
            
            return finishModel(model);
        }

        private void generateJacketedSide(int s) {
            double d;
            if((connMap & 1<<s) != 0)
                d = 0;
            else if(connCount == 0)
                d = 0.25;
            else if(connCount == 1 && (connMap & 1<<(s^1)) != 0)
                d = 0.25;
            else
                return;
            
            Vertex5[] verts = faceVerts(d-0.002);
            Transformation t = Rotation.sideRotations[s].at(Vector3.center);
            IUVTransformation uvt = new UVTranslation(12, 12);
            for(Vertex5 vert : verts) {
                vert.apply(t);
                vert.apply(uvt);
            }
            
            i = addVerts(model, verts, i);
        }
    }
    
    public static CCModel[] scaffoldModels = new CCModel[7];
    public static CCModel[] wireModels = new CCModel[64*3];
    public static JacketedModel[] jacketModels = new JacketedModel[64*3];
    private static WireModelGenerator gen_inst = new WireModelGenerator();
    
    static {
        ScaffoldModelGenerator.generateModels();
    }
    
    public static int modelKey(int thickness, int connMap) {
        return connMap|thickness<<6;
    }
    
    public static int modelKey(ScaffoldWirePart w) {
        return modelKey(w.getThickness(), w.connMap);
    }
    
    public static CCModel getOrGenerateWireModel(int key) {
        CCModel m = wireModels[key];
        if(m == null)
            wireModels[key] = m = gen_inst.generateWireModel(key);
        return m;
    }
    
    public static JacketedModel getOrGenerateJacketedModel(int key) {
        JacketedModel m = jacketModels[key];
        if(m == null)
            jacketModels[key] = m = gen_inst.generateJacketedModel(key);
        return m;
    }
    
    public static void render(ScaffoldWirePart w, LazyLightMatrix olm) {
        int key = modelKey(w);
        
        IUVTransformation uvt = new IconTransformation(w.getIcon());
        IVertexModifier m = w.getColour() == -1 ? ColourModifier.instance : new ColourMultiplier(w.getColour());
        
        if(w.material == 0) {
            Transformation t = new Translation(w.x(), w.y(), w.z());
            getOrGenerateWireModel(key).render(t, uvt, m);
            renderScaffold(key, t, uvt);
        }
        else {
            getOrGenerateJacketedModel(key).render(w, olm, uvt, m, w.material);
        }
    }
    
    private static void renderScaffold(int key, Transformation t, IUVTransformation uvt) {
        scaffoldModels[6].render(t, uvt);
        for(int s = 0; s < 6; s++)
            if((key&1<<s) != 0)
                scaffoldModels[s].render(t, uvt);
    }

    public static void renderBreakingOverlay(Icon icon, ScaffoldWirePart wire) {
        for(Cuboid6 box : wire.getCollisionBoxes())
            RenderUtils.renderBlock(box, 0, new Translation(wire.x(), wire.y(), wire.z()), new IconTransformation(icon), null);
    }
    
    public static void renderInv(int thickness, Transformation t, Icon icon) {
        IUVTransformation uvt = new IconTransformation(icon);
        
        getOrGenerateWireModel(modelKey(thickness, 0x3F)).render(t, uvt);
        CCRenderState.setColour(-1);
        renderScaffold(modelKey(thickness, 0), t, uvt);
    }
}
