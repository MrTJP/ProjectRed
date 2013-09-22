package mrtjp.projectred.integration;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.InvertX;
import mrtjp.projectred.transmission.RenderWire;
import mrtjp.projectred.transmission.RenderWire.UVT;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.util.Icon;
import net.minecraft.util.ResourceLocation;
import codechicken.lib.colour.Colour;
import codechicken.lib.lighting.LightModel;
import codechicken.lib.lighting.PlanarLightModel;
import codechicken.lib.math.MathHelper;
import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.ColourMultiplier;
import codechicken.lib.render.IUVTransformation;
import codechicken.lib.render.IVertexModifier;
import codechicken.lib.render.IconTransformation;
import codechicken.lib.render.MultiIconTransformation;
import codechicken.lib.render.TextureDataHolder;
import codechicken.lib.render.TextureSpecial;
import codechicken.lib.render.TextureUtils;
import codechicken.lib.render.UVScale;
import codechicken.lib.render.UVTranslation;
import codechicken.lib.render.Vertex5;
import codechicken.lib.vec.Rectangle4i;
import codechicken.lib.vec.RedundantTransformation;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Scale;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.Vector3;

public class ComponentStore
{
    public static CCModel base;
    public static CCModel lightChip;
    public static CCModel leverOn;
    public static CCModel leverOff;
    public static CCModel solarArray;
    public static CCModel rainSensor;
    public static CCModel pointer;
    public static CCModel busXcvr;
    public static CCModel busXcvrPanel;

    public static CCModel nullCellWireBottom;
    public static CCModel nullCellWireTop;
    public static CCModel nullCellBase;
    public static CCModel extendedCellWireBottom;
    public static CCModel extendedCellWireTop;
    public static CCModel extendedCellBase;
    public static CCModel cellWireSide;
    public static CCModel cellFrame;
    public static CCModel cellPlate;
    
    public static Icon baseIcon;
    public static Icon[] wireIcons = new Icon[3];
    public static Colour[][] wireData = new Colour[3][];
    public static Icon[] redstoneTorchIcons = new Icon[2];
    public static Icon[] taintedChipIcons = new Icon[2];
    public static Icon[] redstoneChipIcons = new Icon[2];
    public static Icon[] minusChipIcons = new Icon[2];
    public static Icon[] plusChipIcons = new Icon[2];
    public static Icon leverIcon;
    public static Icon[] solarIcons = new Icon[3];
    public static Icon rainIcon;
    public static Icon pointerIcon;
    public static Icon busXcvrIcon;
    public static Icon cellIcon;
    
    static
    {
        base = loadBase("base");
        lightChip = loadModel("chip");
        solarArray = loadModel("solar");
        rainSensor = loadModel("rainsensor");
        leverOff = loadModel("leveroff");
        leverOn = loadModel("leveron");
        pointer = loadModel("pointer");
        busXcvr = loadModel("array/busxcvr");
        busXcvrPanel = loadModel("array/busxcvrpanel");
        
        nullCellWireBottom = loadModel("array/nullcellbottomwire").apply(new Translation(0.5, 0, 0.5));
        nullCellWireTop = loadModel("array/nullcelltopwire").apply(new Translation(0.5, 0, 0.5));
        nullCellBase = loadBase("array/nullcellbase");
        extendedCellWireBottom = loadModel("array/extendedcellbottomwire").apply(new Translation(0.5, 0, 0.5));
        extendedCellWireTop = loadModel("array/extendedcelltopwire").apply(new Translation(0.5, 0, 0.5));        
        extendedCellBase = loadBase("array/extendedcellbase");        
        cellWireSide = loadModel("array/cellsidewire").apply(new Translation(0.5, 0, 0.5));
        cellFrame = loadModel("array/cellstand").apply(new Translation(0.5, 0, 0.5));
        cellPlate = loadModel("array/cellplate").apply(new Translation(0.5, 0, 0.5)); 
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
    
    private static CCModel loadBase(String name) {
        CCModel m = loadModel(name);
        m.apply(new Translation(0.5, 0, 0.5));
        for(int i = 0; i < m.verts.length; i++)//inset each face a little for posts and other stuff that render overtop
            m.verts[i].vec.subtract(m.normals[i].copy().multiply(0.0002));
        return m;
    }
    
    public static void registerIcons(IconRegister r) {
        String baseTex = "projectred:gates/";
        baseIcon = r.registerIcon(baseTex+"base");
        wireIcons[0] = r.registerIcon(baseTex+"surface/bordermatte");
        wireIcons[1] = r.registerIcon(baseTex+"surface/wirematte-OFF");
        wireIcons[2] = r.registerIcon(baseTex+"surface/wirematte-ON");
        for(int i = 0; i < 3; i++) {
            ResourceLocation res = new ResourceLocation(wireIcons[i].getIconName());
            wireData[i] = TextureUtils.loadTextureColours(new ResourceLocation(
                    res.getResourceDomain(), "textures/blocks/"+res.getResourcePath()+".png"));
        }
        redstoneTorchIcons[0] = r.registerIcon("redstone_torch_off");
        redstoneTorchIcons[1] = r.registerIcon("redstone_torch_on");
        taintedChipIcons[0] = r.registerIcon(baseTex+"yellowchipoff");
        taintedChipIcons[1] = r.registerIcon(baseTex+"yellowchipon");
        redstoneChipIcons[0] = r.registerIcon(baseTex+"redchipoff");
        redstoneChipIcons[1] = r.registerIcon(baseTex+"redchipon");
        minusChipIcons[0] = r.registerIcon(baseTex+"minuschipoff");
        minusChipIcons[1] = r.registerIcon(baseTex+"minuschipon");
        plusChipIcons[0] = r.registerIcon(baseTex+"pluschipoff");
        plusChipIcons[1] = r.registerIcon(baseTex+"pluschipon");
        for (int i = 0; i < 3; i++) 
            solarIcons[i] = r.registerIcon(baseTex+"solar"+i);
        rainIcon = r.registerIcon(baseTex+"rainsensor");
        leverIcon = r.registerIcon(baseTex+"lever");
        pointerIcon = r.registerIcon(baseTex+"pointer");
        busXcvrIcon = r.registerIcon(baseTex+"busxcvr");
        cellIcon = r.registerIcon(baseTex+"cells");
        
        RenderGate.registerIcons(r);
    }

    public static WireComponentModel[] generateWireModels(String name, int count) {
        WireComponentModel[] models = new WireComponentModel[count];
        for(int i = 0; i < count; i++)
            models[i] = generateWireModel(name+"-"+i);
        return models;
    }

    public static WireComponentModel generateWireModel(String name) {
        Colour[] data = TextureUtils.loadTextureColours(new ResourceLocation("projectred:textures/blocks/gates/surface/"+name+".png"));
        WireComponentModel m = new WireComponentModel();
        if(Configurator.logicwires3D.getBoolean(true))
            new WireModel3D(data).bind(m);
        else
            new WireModel2D(data).bind(m);
        return m;
    }
    
    public static Transformation orientT(int orient) {
        Transformation t = Rotation.sideOrientation(orient%24>>2, orient&3);
        if(orient >= 24)
            t = new Scale(-1, 1, 1).with(t);
        
        return t.at(Vector3.center);
    }
    
    public static Transformation dynamicT(int orient) {
        return orient == 0 ? new RedundantTransformation() : new Scale(-1, 1, 1).at(Vector3.center);
    }
    
    public static CCModel bakeCopy(CCModel base, int orient) {
        CCModel m = base.copy();
        if(orient >= 24) 
            reverseFacing(m);
        
        m.apply(orientT(orient)).computeLighting(LightModel.standardLightModel);
        return m;
    }
    
    public static CCModel[] bakeDynamic(CCModel base) {
        return new CCModel[]{base.copy(), reverseFacing(base.copy())};
    }
    
    public static CCModel reverseFacing(CCModel m) {
        for(int i = 0; i < m.verts.length; i+=4) {
            Vertex5 vtmp = m.verts[i+1];
            Vector3 ntmp = m.normals[i+1];
            m.verts[i+1] = m.verts[i+3];
            m.normals[i+1] = m.normals[i+3];
            m.verts[i+3] = vtmp;
            m.normals[i+3] = ntmp;
        }
        return m;
    }

    public static abstract class ComponentModel
    {
        public abstract void renderModel(Transformation t, int orient);

        public void registerTextures(IconRegister r) {
        }
    }
    
    public static class BaseComponentModel extends ComponentModel
    {
        public static CCModel[] models = new CCModel[24];
        
        static {
            for(int i = 0; i < 24; i++)
                models[i] = bakeCopy(base, i);
        }
        
        @Override
        public void renderModel(Transformation t, int orient) {
            models[orient%24].render(t, new IconTransformation(baseIcon));
        }
    }
    
    public static abstract class SingleComponentModel extends ComponentModel
    {
        public CCModel[] models = new CCModel[48];
        
        public SingleComponentModel(CCModel m) {
            for(int i = 0; i < 48; i++)
                models[i] = bakeCopy(m, i);
        }
        
        public SingleComponentModel(CCModel m, Vector3 pos) {
            this(m.copy().apply(pos.multiply(1/16D).translation()));
        }
        
        public abstract IUVTransformation getUVT();
        
        @Override
        public void renderModel(Transformation t, int orient) {
            models[orient].render(t, getUVT());
        }
    }
    
    public static abstract class MultiComponentModel extends ComponentModel 
    {
        public CCModel[][] models;
        public int state;
        
        public MultiComponentModel(CCModel... m) {
            this(new Vector3(0,0,0), m);
        }
        
        public MultiComponentModel(Vector3 pos, CCModel... m) {
            models = new CCModel[m.length][48];
            for(int j = 0; j < m.length; j++)
                for(int i = 0; i < 48; i++)
                    models[j][i] = bakeCopy(m[j].copy().apply(pos.copy().multiply(1/16D).translation()), i);

        }

        public abstract IUVTransformation getUVT();
        
        @Override
        public void renderModel(Transformation t, int orient) {
            models[state][orient].render(t, getUVT());
        }
    }
    
    public static class LeverModel extends MultiComponentModel {

        public LeverModel(double x, double z) {
            super(new Vector3(x, 2, z), leverOn, leverOff);
        }
        
        @Override
        public IUVTransformation getUVT() {
            return new IconTransformation(leverIcon);
        }
        
    }
    
    public static abstract class SimpleComponentModel extends SingleComponentModel
    {
        public SimpleComponentModel(CCModel m) {
            super(m);
        }
        
        public SimpleComponentModel(CCModel m, Vector3 pos) {
            super(m, pos);
        }
        
        @Override
        public IUVTransformation getUVT() {
            return new IconTransformation(getIcon());
        }
        
        public abstract Icon getIcon();
    }
    
    public static abstract class OnOffModel extends SingleComponentModel
    {
        public boolean on;
        
        public OnOffModel(CCModel m) {
            super(m);
        }
        
        public OnOffModel(CCModel m, Vector3 pos) {
            super(m, pos);
        }
        
        public abstract Icon[] getIcons();
        
        @Override
        public IUVTransformation getUVT() {
            return new IconTransformation(getIcons()[on ? 1 : 0]);
        }
    }
    
    public static abstract class StateIconModel extends SingleComponentModel
    {
        public int state;
        
        public StateIconModel(CCModel m) {
            super(m);
        }
        
        public StateIconModel(CCModel m, Vector3 pos) {
            super(m, pos);
        }
        
        public abstract Icon[] getIcons();
        
        @Override
        public IUVTransformation getUVT() {
            return new IconTransformation(getIcons()[state]);
        }
    }
    
    //use pass down composition to allow different wire model classes but have the same fields and reference type
    public static class WireComponentModel extends ComponentModel
    {
        public boolean on;
        public boolean disabled;
        
        private ComponentModel model;
        
        public WireComponentModel bind(ComponentModel model) {
            this.model = model;
            return this;
        }
        
        protected static List<Rectangle4i> rectangulate(Colour[] data) {
            boolean[] wireCorners = new boolean[1024];
            
            for(int y = 0; y <= 30; y++)
                for(int x = 0; x <= 30; x++) {
                    if(data[y*32+x].rgba() != -1)
                        continue;
                    
                    if(overlap(wireCorners, x, y))
                        continue;
                    
                    if(!segment2x2(data, x, y))
                        throw new RuntimeException("Wire segment not 2x2 at ("+x+", "+y+")");
                    
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
            
            return wireRectangles;
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
        
        public static Rectangle4i border(Rectangle4i wire) {
            Rectangle4i border = new Rectangle4i(wire.x-2, wire.y-2, wire.w+4, wire.h+4);
            if(border.x < 0) {
                border.w+=border.x;
                border.x = 0;
            }
            if(border.y < 0) {
                border.h+=border.y;
                border.y = 0;
            }
            if(border.x+border.w >= 32)
                border.w -= border.x+border.w-32;
            if(border.y+border.h >= 32)
                border.h -= border.y+border.h-32;
            
            return border;
        }
        
        @Override
        public void renderModel(Transformation t, int orient) {
            model.renderModel(t, orient);
        }
        
        @Override
        public void registerTextures(IconRegister r) {
            model.registerTextures(r);
        }
    }
    
    public static class WireModel3D extends SingleComponentModel
    {
        private WireComponentModel parent;
        
        public WireModel3D(Colour[] data) {
            super(generateModel(data));
        }
        
        public void bind(WireComponentModel parent) {
            this.parent = parent;
            parent.bind(this);
        }
        
        private static CCModel generateModel(Colour[] data) {
            List<Rectangle4i> wireRectangles = WireComponentModel.rectangulate(data);
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
            generateWireSegment(model, i, WireComponentModel.border(rect), 0.01, 0);
            generateWireSegment(model, i+20, rect, 0.02, 1);
        }
        
        private static void generateWireSegment(CCModel model, int i, Rectangle4i rect, double h, int icon) {
            double x1 = rect.x/32D;
            double x2 = (rect.x+rect.w)/32D;
            double z1 = rect.y/32D;
            double z2 = (rect.y+rect.h)/32D;
            double d = 0.0005-h/50D;//little offset for the wires go ontop of the border
            model.generateBlock(i,
                    x1+d, 0.125, z1+d, 
                    x2-d, 0.125+h, z2-d, 1);
            MultiIconTransformation.setIconIndex(model, i, i+20, icon);
        }

        @Override
        public IUVTransformation getUVT() {
            if(parent.disabled)
                return new IconTransformation(wireIcons[0]);
            else if(parent.on)
                return new MultiIconTransformation(wireIcons[0], wireIcons[2]);
            else
                return new MultiIconTransformation(wireIcons[0], wireIcons[1]);
        }
    }
    
    public static class WireModel2D extends ComponentModel
    {
        public static CCModel[] models = new CCModel[48];
        private static int iconCounter = 0;
        
        static
        {
            CCModel m = CCModel.quadModel(4).generateBlock(0, 0, 0, 0, 1, 1/8D+0.002, 1, ~2).computeNormals();
            m.shrinkUVs(0.0005);
            
            for(int i = 0; i < 48; i++)
                models[i] = bakeCopy(m, i);
        }
        
        private WireComponentModel parent;
        
        public TextureSpecial[] icons;
        public Colour[] wireMask;
        private final int iconIndex = iconCounter++;
        
        public WireModel2D(Colour[] data) {
            wireMask = data;
        }
        
        public void bind(WireComponentModel parent) {
            this.parent = parent;
            parent.bind(this);
        }
        
        @Override
        public void renderModel(Transformation t, int orient) {
            models[orient].render(t, new IconTransformation(icons[parent.disabled ? 0 : parent.on ? 2 : 1]));
        }
        
        @Override
        public void registerTextures(IconRegister r) {
            List<Rectangle4i> wireRectangles = WireComponentModel.rectangulate(wireMask);
            
            icons = new TextureSpecial[wireData.length];
            
            for(int tex = 0; tex < icons.length; tex++) {
                int[] texMap = new int[1024];
                for(Rectangle4i rect : wireRectangles) {
                    fillMask(texMap, rect, 2);
                    fillMask(texMap, WireComponentModel.border(rect), 1);
                }
                
                int pSize = (int)Math.sqrt(wireData[0].length);
                int size = Math.max(32, pSize);
                int relM = size/32;
                int relP = size/pSize;
                
                int[] imageData = new int[size*size];
                for(int i = 0; i < imageData.length; i++) {
                    int x = i%size; int y = i/size;
                    int type = texMap[y/relM*32+x/relM];
                    if(type != 0)
                        imageData[i] = wireData[type == 1 ? 0 : tex][y/relP*pSize+x/relP].argb();
                }
                
                icons[tex] = TextureUtils.getTextureSpecial(r, "projectred:gates/wire2d_"+iconIndex+"_"+tex)
                        .addTexture(new TextureDataHolder(imageData, size));
            }
        }

        private void fillMask(int[] map, Rectangle4i r, int val) {
            for(int i = r.x; i < r.x+r.w; i++)
                for(int j = r.y; j < r.y+r.h; j++)
                    if(map[j*32+i] < val)
                        map[j*32+i] = val;
        }
    }
    
    public static class RedstoneTorchModel extends OnOffModel
    {
        public Vector3 lightPos;
        
        public RedstoneTorchModel(double x, double z, int height) {
            super(genModel(height, x, z));
            lightPos = new Vector3(x, height-1, z).multiply(1/16D);
        }
        
        public RedstoneTorchModel(CCModel m) {
        	super(m);
        }
        
        public static CCModel genModel(int height, double x, double z) {
            CCModel m = CCModel.quadModel(20);
            m.verts[0] = new Vertex5(7/16D, 10/16D, 9/16D, 7/16D, 8/16D);
            m.verts[1] = new Vertex5(9/16D, 10/16D, 9/16D, 9/16D, 8/16D);
            m.verts[2] = new Vertex5(9/16D, 10/16D, 7/16D, 9/16D, 6/16D);
            m.verts[3] = new Vertex5(7/16D, 10/16D, 7/16D, 7/16D, 6/16D);
            m.generateBlock(4, 6/16D, (10-height)/16D, 7/16D, 10/16D, 11/16D, 9/16D, 0x33);
            m.generateBlock(12, 7/16D, (10-height)/16D, 6/16D, 9/16D, 11/16D, 10/16D, 0xF);
            m.apply(new Translation(-0.5+x/16, (height-10)/16D, -0.5+z/16));
            m.computeNormals();
            m.shrinkUVs(0.0005);
            m.apply(new Scale(1.0005)); // Eliminates z-fighting when torch is on wire.
            return m;
        }
        
        @Override
        public Icon[] getIcons() {
            return redstoneTorchIcons;
        }
    }
    
    public static class FlippedRSTorchModel extends RedstoneTorchModel
    {
    	public FlippedRSTorchModel(double x, double z) {
			super(genModel(4, x, z).apply(
					new Rotation(180*MathHelper.torad, 0, 0, 1).at(Vector3.center)
					.with(new Translation(new Vector3(0, -6, 0).multiply(1/16D)))));
			lightPos = new Vector3(x, 4-1, z).multiply(1/16D);
    	}
    }
    
    public static class YellowChipModel extends OnOffModel
    {
        public YellowChipModel(double x, double z) {
            super(lightChip, new Vector3(x, 0, z));
        }
        
        @Override
        public Icon[] getIcons() {
            return taintedChipIcons;
        }
    }
    
    public static class RedChipModel extends OnOffModel
    {
        public RedChipModel(double x, double z) {
            super(lightChip, new Vector3(x, 0, z));
        }
        
        @Override
        public Icon[] getIcons() {
            return redstoneChipIcons;
        }
    }
    
    public static class MinusChipModel extends OnOffModel
    {
        public MinusChipModel(double x, double z) {
            super(lightChip, new Vector3(x, 0, z));
        }
        
        @Override
        public Icon[] getIcons() {
            return minusChipIcons;
        }
    }
    
    public static class PlusChipModel extends OnOffModel
    {
        public PlusChipModel(double x, double z) {
            super(lightChip, new Vector3(x, 0, z));
        }
        
        @Override
        public Icon[] getIcons() {
            return plusChipIcons;
        }
    }
    
    public static class SolarModel extends StateIconModel {
        public SolarModel(double x, double z) {
            super(solarArray, new Vector3(x, 0, z));
        }
        
        @Override
        public Icon[] getIcons() {
            return solarIcons;
        }
    }
    
    public static class RainSensorModel extends SimpleComponentModel
    {
        public RainSensorModel(double x, double z) {
            super(rainSensor, new Vector3(x, 0, z));
        }
        
        @Override
        public Icon getIcon() {
            return rainIcon;
        }
    }
    
    public static class PointerModel extends ComponentModel
    {
        public CCModel[] models;
        
        public double angle;
        public Vector3 pos;
        
        public PointerModel(double x, double y, double z) {
            models = bakeDynamic(pointer);
            pos = new Vector3(x, y-1, z).multiply(1/16D);
        }
        
        public PointerModel(double x, double y, double z, double scale) {
            models = bakeDynamic(pointer.copy().apply(new Scale(scale, 1, scale)));
            pos = new Vector3(x, y-1, z).multiply(1/16D);
        }
        
        @Override
        public void renderModel(Transformation t, int orient) {
            models[orient].render(
                    new Rotation(-angle+MathHelper.pi, 0, 1, 0).with(pos.translation())
                        .with(dynamicT(orient)).with(t), 
                    new IconTransformation(pointerIcon), LightModel.standardLightModel);
        }
    }
    
    public static abstract class BundledCableModel extends SingleComponentModel
    {
        public BundledCableModel(CCModel model, Vector3 pos, double texCenterU, double texCenterV) {
            super(model, pos);
            
            for(int orient = 0; orient < 48; orient++) {
                int side = orient%24>>2;
                int r = orient&3;
                boolean reflect = orient >= 24;
                boolean rotate = (r+RenderWire.reorientSide[side])%4 >= 2;
                
                Transformation t = new RedundantTransformation();
                if(reflect)
                    t = t.with(new Scale(-1, 0, 1));
                if(rotate)
                    t = t.with(Rotation.quarterRotations[2]);
                
                if(!(t instanceof RedundantTransformation))
                    models[orient].apply(new UVT(t.at(new Vector3(texCenterU, 0, texCenterV))));
            }
        }
    }
    
    public static class BusXcvrCableModel extends BundledCableModel
    {
        public BusXcvrCableModel() {
            super(busXcvr, new Vector3(8,0,8), 10/32D, 14/32D);
        }
        
        @Override
        public IUVTransformation getUVT() {
            return new IconTransformation(busXcvrIcon);
        }
    }

    public static class BusXcvrPanelModel extends ComponentModel
    {
        public static CCModel[] displayModels = new CCModel[16];
        
        static {
            for(int i = 0; i < 16; i++) {
                CCModel m = CCModel.quadModel(4);
                int x = i%4;
                int z = i/4;
                double y = 10/32D+0.00001D;
                m.verts[0] = new Vertex5(x, y, z+1, x, z);
                m.verts[1] = new Vertex5(x+1, y, z+1, x+1, z);
                m.verts[2] = new Vertex5(x+1, y, z, x+1, z+1);
                m.verts[3] = new Vertex5(x, y, z, x, z+1);
                m.apply(new Scale(1/16D, 1, 1/16D).with(new Translation(-2/16D, 7/16D, -2/16D)));
                m.apply(new UVTranslation(22, 0));
                m.apply(new UVScale(1/32D));
                m.computeNormals();
                m.shrinkUVs(0.0005);
                displayModels[i] = m;
            }
        }
        
        public CCModel[] models;
        public Vector3 pos;
        public boolean flip;
        public int signal;
        
        public BusXcvrPanelModel(double x, double z, boolean flip) {
            this.flip = flip;
            pos = new Vector3(x, 0, z).multiply(1/16D);
            
            CCModel base = busXcvrPanel.copy();
            if(flip)
                base.apply(Rotation.quarterRotations[2]);
            base.apply(pos.translation());
            
            this.models = new CCModel[48];
            for(int i = 0; i < 48; i++)
                models[i] = bakeCopy(base, i);
        }
        
        @Override
        public void renderModel(Transformation t, int orient) {
            IconTransformation icont = new IconTransformation(busXcvrIcon);
            models[orient].render(t, icont);
            
            Vector3 displayPos = pos.copy();
            if(orient >= 24)//flipped x
                displayPos.x = 1-displayPos.x;
            
            Transformation displayT = flip ? new RedundantTransformation() : Rotation.quarterRotations[2];
            displayT = displayT.with(displayPos.translation()).with(orientT(orient%24)).with(t);
            for(int i = 0; i < 16; i++)
                if((signal & 1<<i) != 0)
                    displayModels[i].render(displayT, icont, PlanarLightModel.standardLightModel);
        }
    }
    
    public static abstract class CellWireModel extends ComponentModel
    {
        public byte signal;
        public boolean invColour;
        
        public static int signalColour(byte signal) {
            return ((signal&0xFF)/2 + 60) << 24 | 0xFF;
        }
        
        @Override
        public final void renderModel(Transformation t, int orient) {
            if(invColour) {
                CCRenderState.setColour(signalColour((byte) 0));
                renderWire(t, orient, null);
                CCRenderState.setColour(-1);
            }
            else {
                renderWire(t, orient, new ColourMultiplier(signalColour(signal)));
            }
        }
        
        public abstract void renderWire(Transformation t, int orient, IVertexModifier colour);
    }
    
    public static class CellTopWireModel extends CellWireModel
    {
        public static CCModel[] left = new CCModel[24];
        public static CCModel[] right = new CCModel[24];
        
        static {
            CCModel cellWireLeft = cellWireSide.copy().apply(new Translation(-7.001/16D, 0, 0));
            CCModel cellWireRight = cellWireSide.copy().apply(new Translation(7.001/16D, 0, 0));
            for(int i = 0; i < 24; i++) {
                left[i] = bakeCopy(cellWireLeft, i);
                right[i] = bakeCopy(cellWireRight, i);
            }
        }
        
        public CCModel[] top = new CCModel[24];
        public byte conn;
        
        public CellTopWireModel(CCModel wireTop) {
            for(int i = 0; i < 24; i++)
                top[i] = bakeCopy(wireTop, i);
        }
        
        public void renderWire(Transformation t, int orient, IVertexModifier colour) {
            IconTransformation icont = new IconTransformation(cellIcon);
            top[orient].render(t, icont, colour);
            if((conn & 2) == 0)
                right[orient].render(t, icont, colour);
            if((conn & 8) == 0)
                left[orient].render(t, icont, colour);
        }
    }
    
    public static class CellBottomWireModel extends CellWireModel
    {
        public CCModel[] bottom = new CCModel[24];
        
        public CellBottomWireModel(CCModel wireBottom) {
            for(int i = 0; i < 24; i++)
                bottom[i] = bakeCopy(wireBottom, i);
        }
        
        @Override
        public void renderWire(Transformation t, int orient, IVertexModifier colour) {
            bottom[orient].render(t, new IconTransformation(cellIcon), colour);
        }
    }
    
    public static class CellFrameModel extends SimpleComponentModel
    {
        public CellFrameModel() {
            super(cellFrame);
        }
        
        @Override
        public Icon getIcon() {
            return cellIcon;
        }
    }
    
    public static class CellPlateModel extends SimpleComponentModel
    {
        public CellPlateModel() {
            super(cellPlate);
        }
        
        @Override
        public Icon getIcon() {
            return cellIcon;
        }
    }
    
    public static class NullCellBaseModel extends SimpleComponentModel
    {
        public NullCellBaseModel() {
            super(nullCellBase);
        }
        
        @Override
        public Icon getIcon() {
            return cellIcon;
        }
    }
    
    public static class ExtendedCellBaseModel extends SimpleComponentModel
    {
        public ExtendedCellBaseModel() {
            super(extendedCellBase);
        }
        
        @Override
        public Icon getIcon() {
            return cellIcon;
        }
    }
}
