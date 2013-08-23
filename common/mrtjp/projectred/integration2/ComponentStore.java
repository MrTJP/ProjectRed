package mrtjp.projectred.integration2;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import mrtjp.projectred.core.InvertX;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.util.Icon;
import net.minecraft.util.ResourceLocation;
import codechicken.lib.colour.Colour;
import codechicken.lib.lighting.PlanarLightModel;
import codechicken.lib.render.CCModel;
import codechicken.lib.render.IUVTransformation;
import codechicken.lib.render.IconTransformation;
import codechicken.lib.render.MultiIconTransformation;
import codechicken.lib.render.TextureUtils;
import codechicken.lib.render.Vertex5;
import codechicken.lib.vec.Rectangle4i;
import codechicken.lib.vec.Scale;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.Vector3;

public class ComponentStore
{
    public static CCModel base;
    public static Icon baseIcon;
    public static Icon[] wireIcons = new Icon[3];
    public static Icon[] redstoneTorchIcons = new Icon[2];
    public static CCModel taintedChip;
    public static Icon[] taintedChipIcons = new Icon[2];
    
    static
    {
        base = loadBase();
        taintedChip = loadModel("chip");
    }
    
    public static Map<String, CCModel> loadModels(String name) {
        return CCModel.parseObjModels(new ResourceLocation("projectred:textures/obj/gateparts/"+name+".obj"), 7, new InvertX());
    }

    public static CCModel loadModel(String name) {
        Map<String, CCModel> models = loadModels(name);
        CCModel m = CCModel.combine(models.values());
        m.computeNormals();
        m.shrinkUVs(0.0005);
        return m;
    }
    
    public static CCModel[] loadModelSet(String name, String[]... groups) {
        Map<String, CCModel> modelMap = loadModels(name);
        CCModel[] models = new CCModel[groups.length];
        for(int i = 0; i < groups.length; i++) {
            List<CCModel> grp = new LinkedList<CCModel>();
            for(String s : groups[i])
                grp.add(modelMap.get(s));
            
            CCModel m = CCModel.combine(grp);
            m.computeNormals();
            m.shrinkUVs(0.0005);
            models[i] = m;
        }
        return models;
    }
    
    private static CCModel loadBase() {
        CCModel m = loadModel("base");
        m.apply(new Translation(0.5, 0, 0.5));
        return m;
    }
    
    public static void registerIcons(IconRegister r) {
        String baseTex = "projectred:gates/";
        baseIcon = r.registerIcon(baseTex+"base");
        wireIcons[0] = r.registerIcon(baseTex+"surface/bordermatte");
        wireIcons[1] = r.registerIcon(baseTex+"surface/wirematte-OFF");
        wireIcons[2] = r.registerIcon(baseTex+"surface/wirematte-ON");
        redstoneTorchIcons[0] = r.registerIcon("redstone_torch_off");
        redstoneTorchIcons[1] = r.registerIcon("redstone_torch_on");
        taintedChipIcons[0] = r.registerIcon(baseTex+"yellowchipoff");
        taintedChipIcons[1] = r.registerIcon(baseTex+"yellowchipon");
    }

    public static WireComponentModel[] generateWireModels(String name, int count) {
        WireComponentModel[] models = new WireComponentModel[count];
        for(int i = 0; i < count; i++)
            models[i] = new WireComponentModel(generateWireModel(name+"-"+i));
        return models;
    }

    public static CCModel generateWireModel(String name) {
        Colour[] data = TextureUtils.loadTextureColours(new ResourceLocation("projectred:textures/blocks/gates/surface/"+name+".png"));
        boolean[] wireCorners = new boolean[1024];
    
        for(int y = 2; y <= 28; y++)
            for(int x = 2; x <= 28; x++) {
                if(data[y*32+x].rgba() != -1)
                    continue;
                
                if(overlap(wireCorners, x, y))
                    continue;
                
                if(!segment2x2(data, x, y))
                    throw new RuntimeException("Wire segment not 2x2 at ("+x+", "+y+") in "+name);
                
                wireCorners[y*32+x] = true;
            }
        
        List<Rectangle4i> wireRectangles = new LinkedList<Rectangle4i>();
        for(int i = 0; i < 1024; i++)
            if(wireCorners[i]) {
                Rectangle4i rect = new Rectangle4i(i%32, i/32, 0, 0);
                int x = rect.x+2;
                while(x < 30 && wireCorners[rect.y*32+x])
                    x+=2;
                rect.w = x-rect.x;
                
                int y = rect.y+2;
                while(y < 30) {
                    boolean advance = true;
                    for(int dx = rect.x; dx < rect.x+rect.w && advance; dx+=2)
                        if(!wireCorners[y*32+dx])
                            advance = false;
                    
                    if(!advance)
                        break;
                    
                    y+=2;
                }
                rect.h = y-rect.y;

                for(int dy = rect.y; dy < rect.y+rect.h; dy+=2)
                    for(int dx = rect.x; dx < rect.x+rect.w; dx+=2)
                        wireCorners[dy*32+dx] = false;
                
                wireRectangles.add(rect);
            }
        
        CCModel model = CCModel.quadModel(wireRectangles.size()*40);
        int i = 0;
        for(Rectangle4i rect : wireRectangles) {
            generateWireSegment(model, i, rect);
            i+=40;
        }
        model.computeNormals();
        model.shrinkUVs(0.0005);
        return model;
    }

    private static void generateWireSegment(CCModel model, int i, Rectangle4i rect) {
        double x1 = rect.x/32D;
        double x2 = (rect.x+rect.w)/32D;
        double z1 = rect.y/32D;
        double z2 = (rect.y+rect.h)/32D;
        
        model.generateBlock(i, //border
                x1-1/16D, 0.125, z1-1/16D, 
                x2+1/16D, 0.135, z2+1/16D, 1);
        MultiIconTransformation.setIconIndex(model, i, i+20, 0);
        i+=20;
        model.generateBlock(i, //wire
                x1, 0.125, z1, 
                x2, 0.145, z2, 1);
        MultiIconTransformation.setIconIndex(model, i, i+20, 1);
    }

    private static boolean overlap(boolean[] wireCorners, int x, int y) {
        return wireCorners[y*32+(x-1)] ||
                wireCorners[(y-1)*32+x] ||
                wireCorners[(y-1)*32+(x-1)];
    }

    private static boolean segment2x2(Colour[] data, int x, int y) {
        return data[y*32+(x+1)].rgba() == -1 &&
                data[(y+1)*32+x].rgba() == -1 &&
                data[(y+1)*32+(x+1)].rgba() == -1;
    }

    public static abstract class ComponentModel
    {
        public Translation relPos;
        
        public ComponentModel() {
        }
        
        public ComponentModel(Vector3 pos) {
            relPos = pos.multiply(1/16D).translation();
        }
        
        public final void render(Transformation t) {
            if(relPos != null)
                renderModel(relPos.with(t));
            else
                renderModel(t);
        }
        
        public abstract void renderModel(Transformation t);
    }

    public static abstract class SingleComponentModel extends ComponentModel
    {
        public CCModel model;
        
        public SingleComponentModel(CCModel m) {
            model = m;
        }
        
        public SingleComponentModel(CCModel m, Vector3 pos) {
            super(pos);
            model = m;
        }
        
        @Override
        public void renderModel(Transformation t) {
            model.render(t, getIconT(), PlanarLightModel.standardLightModel);
        }

        public abstract IUVTransformation getIconT();
    }
    
    public static class SimpleComponentModel extends SingleComponentModel
    {
        public IconTransformation icont;
        
        public SimpleComponentModel(CCModel m, Icon icon) {
            super(m);
            icont = new IconTransformation(icon);
        }
        
        @Override
        public IUVTransformation getIconT() {
            return icont;
        }
    }
    
    public static class WireComponentModel extends SingleComponentModel
    {
        public IUVTransformation[] icont = new IUVTransformation[3];
        public boolean on;
        public boolean disabled;
        
        public WireComponentModel(CCModel m) {
            super(m);
            icont[0] = new MultiIconTransformation(new Icon[]{wireIcons[0], wireIcons[1]});
            icont[1] = new MultiIconTransformation(new Icon[]{wireIcons[0], wireIcons[2]});
            icont[2] = new IconTransformation(wireIcons[0]);
        }
        
        @Override
        public IUVTransformation getIconT() {
            return icont[disabled ? 2 : on ? 1 : 0];
        }
    }
    
    public static class RedstoneTorchModel extends SingleComponentModel
    {
        public IconTransformation[] icont = new IconTransformation[2];
        public boolean on;
        public double lightY;
        
        public RedstoneTorchModel(double x, double z, int height) {
            super(genModel(height), new Vector3(x, 0, z));
            lightY = (height-1)/16D;
            icont[0] = new IconTransformation(redstoneTorchIcons[0]);
            icont[1] = new IconTransformation(redstoneTorchIcons[1]);
        }
        
        public static CCModel genModel(int height) {
            CCModel m = CCModel.quadModel(20);
            m.verts[0] = new Vertex5(7/16D, 10/16D, 9/16D, 7/16D, 8/16D);
            m.verts[1] = new Vertex5(9/16D, 10/16D, 9/16D, 9/16D, 8/16D);
            m.verts[2] = new Vertex5(9/16D, 10/16D, 7/16D, 9/16D, 6/16D);
            m.verts[3] = new Vertex5(7/16D, 10/16D, 7/16D, 7/16D, 6/16D);
            m.generateBlock(4, 6/16D, (10-height)/16D, 7/16D, 10/16D, 11/16D, 9/16D, 0x33);
            m.generateBlock(12, 7/16D, (10-height)/16D, 6/16D, 9/16D, 11/16D, 10/16D, 0xF);
            m.apply(new Translation(-0.5, (height-10)/16D, -0.5));
            m.computeNormals();
            m.shrinkUVs(0.0005);
            m.apply(new Scale(1.0005)); // Eliminates z-fighting when torch is on wire.
            return m;
        }

        @Override
        public IUVTransformation getIconT() {
            return icont[on ? 1 : 0];
        }
    }
    
    public static class ChipModel extends SingleComponentModel {
        public IconTransformation[] icont = new IconTransformation[2];
        public boolean on;

        public ChipModel(double x, double z) {
            super(taintedChip, new Vector3(x, 0, z));
            icont[0] = new IconTransformation(taintedChipIcons[0]);
            icont[1] = new IconTransformation(taintedChipIcons[1]);
        }

        @Override
        public IUVTransformation getIconT() {
            return icont[on ? 1 : 0];
        }
        
    }

    public static class MultiStateComponentModel extends ComponentModel
    {
        public CCModel[] models;
        public IconTransformation icont;
        public int state;
        
        public MultiStateComponentModel(Icon icon, Vector3 pos, CCModel... models) {
            super(pos);
            this.models = models;
            icont = new IconTransformation(icon);
        }
        
        public CCModel getModel() {
            return models[state];
        }
        
        @Override
        public void renderModel(Transformation t) {
            getModel().render(t, icont, PlanarLightModel.standardLightModel);
        }
    }
    
    public static class TwoStateComponentModel extends MultiStateComponentModel
    {
        public boolean on;
        
        public TwoStateComponentModel(Icon icon, double x, double y, double z, CCModel[] models) {
            super(icon, new Vector3(x, y, z), models);
        }
        
        @Override
        public CCModel getModel() {
            state = on ? 1 : 0;
            return super.getModel();
        }
    }
}
