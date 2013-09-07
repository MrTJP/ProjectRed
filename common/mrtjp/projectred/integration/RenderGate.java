package mrtjp.projectred.integration;

import static mrtjp.projectred.integration.ComponentStore.*;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import mrtjp.projectred.integration.BundledGateLogic.BusTransceiver;
import mrtjp.projectred.integration.BundledGateLogic.BusDisplay;
import mrtjp.projectred.integration.InstancedRsGateLogic.ExtraStateLogic;
import mrtjp.projectred.integration.InstancedRsGateLogic.TimerGateLogic;
import net.minecraft.client.renderer.texture.IconRegister;
import codechicken.lib.math.MathHelper;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.TextureUtils;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.Vector3;

@SuppressWarnings({"rawtypes", "unchecked"})
public class RenderGate
{
    public static GateRenderer[] renderers = new GateRenderer[]{
            new OR(),
            new NOR(),
            new NOT(),
            new AND(),
            new NAND(),
            new XOR(),
            new XNOR(),
            new Buffer(),
            new Multiplexer(),
            new Pulse(),
            new Repeater(),
            new Randomizer(),
            new RSLatch(),
            new ToggleLatch(),
            new TransparentLatch(),
            new LightSensor(),
            new RainSensor(),
            new Timer(),
            new Sequencer(),
            new Counter(),
            new StateCell(),
            new Synchronizer(),
            new BusXcvr(),
            new NullCell(),
            new InvertCell(),
            new BufferCell(),
            new Comparator(),
            new ANDCell(),
            new BusEye()
        };
    
    public static void registerIcons(IconRegister r) {
        for(GateRenderer render : renderers)
        	render.registerIcons(r);
    }
    
    public static void renderStatic(GatePart gate) {
        GateRenderer r = renderers[gate.subID&0xFF];
        r.prepare(gate);
        r.renderStatic(new Translation(gate.x(), gate.y(), gate.z()), gate.orientation&0xFF);
    }

    public static void renderDynamic(GatePart gate, Vector3 pos, float frame) {
        GateRenderer r = renderers[gate.subID&0xFF];
        if(r.hasSpecials()) {
            r.prepareDynamic(gate, frame);
            r.renderDynamic(gate.rotationT().with(pos.translation()));
        }
    }

    public static void renderInv(Transformation t, int id) {
        GateRenderer r = renderers[id];
        TextureUtils.bindAtlas(0);
        r.prepareInv();
        CCRenderState.startDrawing(7);
        r.renderStatic(t, 0);
        CCRenderState.draw();
        if(r.hasSpecials())
            r.renderDynamic(t);
    }

    public static void spawnParticles(GatePart gate, Random rand) {
        renderers[gate.subID&0xFF].spawnParticles(gate, rand);
    }

    public static abstract class GateRenderer<PartType extends GatePart>
    {
        public List<ComponentModel> models = new LinkedList<ComponentModel>();
        public boolean reflect = false;
        
        public GateRenderer() {
            models.add(new BaseComponentModel());
        }
        
        public void registerIcons(IconRegister r) {
            for(ComponentModel m : models)
                if (m != null)
                    m.registerTextures(r);
        }

        public final void renderStatic(Transformation t, int orient) {
            renderModels(t, reflect ? orient+24 : orient);
        }
        
        public void renderModels(Transformation t, int orient) {
            for(ComponentModel m : models)
                m.renderModel(t, orient);
        }
        
        public void renderDynamic(Transformation t) {
        }
        
        public void prepareInv() {
        }
        
        public void prepare(PartType part) {
        }
        
        public void prepareDynamic(PartType part, float frame) {
        }
    
        public boolean hasSpecials() {
            return false;
        }
    
        public void spawnParticles(PartType part, Random rand) {
            prepare(part);
            List<RedstoneTorchModel> torches = new LinkedList<RedstoneTorchModel>();
            for(ComponentModel m : models)
                if(m instanceof RedstoneTorchModel)
                    torches.add((RedstoneTorchModel) m);
            
            for(RedstoneTorchModel m : torches)
                if(m.on && rand.nextInt(torches.size()) == 0) {
                    Vector3 pos = new Vector3(rand.nextFloat(), rand.nextFloat(), rand.nextFloat())
                        .add(-0.5).multiply(0.05, 0.1, 0.05);
                    pos.add(m.lightPos);//height
                    pos.apply(part.rotationT()).add(part.x(), part.y(), part.z());
                    part.world().spawnParticle("reddust", pos.x, pos.y, pos.z, 0, 0, 0);
                }
        }
    }

    public static class OR extends GateRenderer<SimpleGatePart>
    {
        WireComponentModel[] wires = generateWireModels("OR", 4);
        RedstoneTorchModel[] torches = new RedstoneTorchModel[]{
                new RedstoneTorchModel(8, 9, 6),
                new RedstoneTorchModel(8, 2.5, 8)
            };
        
        public OR() {
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }
        
        @Override
        public void prepareInv() {
            wires[0].on = true;
            wires[1].on = false;
            wires[2].on = false;
            wires[3].on = false;
            wires[1].disabled = false;
            wires[2].disabled = false;
            wires[3].disabled = false;
            torches[0].on = true;
            torches[1].on = false;
        }
        
        @Override
        public void prepare(SimpleGatePart part) {
            wires[0].on = (part.state&0x10) == 0;
            wires[1].on = (part.state&2) != 0;
            wires[2].on = (part.state&4) != 0;
            wires[3].on = (part.state&8) != 0;
            wires[1].disabled = (part.shape&1) != 0;
            wires[2].disabled = (part.shape&2) != 0;
            wires[3].disabled = (part.shape&4) != 0;
            torches[0].on = (part.state&0xE) == 0;
            torches[1].on = !wires[0].on;
        }
    }
    
    public static class NOR extends GateRenderer<SimpleGatePart>
    {
        WireComponentModel[] wires = generateWireModels("OR", 4);
        RedstoneTorchModel torch = new RedstoneTorchModel(8, 9, 6);
        
        public NOR() {
            models.addAll(Arrays.asList(wires));
            models.add(torch);
        }
        
        @Override
        public void prepareInv() {
            wires[0].on = true;
            wires[1].on = false;
            wires[2].on = false;
            wires[3].on = false;
            wires[1].disabled = false;
            wires[2].disabled = false;
            wires[3].disabled = false;
            torch.on = true;
        }
        
        @Override
        public void prepare(SimpleGatePart part) {
            wires[0].on = (part.state&0x11) != 0;
            wires[1].on = (part.state&2) != 0;
            wires[2].on = (part.state&4) != 0;
            wires[3].on = (part.state&8) != 0;
            wires[1].disabled = (part.shape&1) != 0;
            wires[2].disabled = (part.shape&2) != 0;
            wires[3].disabled = (part.shape&4) != 0;
            torch.on = (part.state&0xE) == 0;
        }
    }
    
    public static class NOT extends GateRenderer<SimpleGatePart>
    {
        WireComponentModel[] wires = generateWireModels("NOT", 4);
        RedstoneTorchModel torch = new RedstoneTorchModel(8, 8, 6);
        
        public NOT() {
            models.addAll(Arrays.asList(wires));
            models.add(torch);
        }
        
        @Override
        public void prepareInv() {
            wires[0].on = true;
            wires[1].on = true;
            wires[2].on = true;
            wires[3].on = false;
            wires[3].disabled = false;
            wires[0].disabled = false;
            wires[2].disabled = false;
            torch.on = true;
        }
        
        @Override
        public void prepare(SimpleGatePart part) {
            wires[0].on = (part.state&0x11) != 0;
            wires[3].on = (part.state&0x22) != 0;
            wires[1].on = (part.state&4) != 0;
            wires[2].on = (part.state&0x88) != 0;
            wires[3].disabled = (part.shape&1) != 0;
            wires[0].disabled = (part.shape&2) != 0;
            wires[2].disabled = (part.shape&4) != 0;
            torch.on = (part.state&0xF0) != 0;
        }
    }

    public static class AND extends GateRenderer<SimpleGatePart>
    {   
        WireComponentModel[] wires = generateWireModels("AND", 4);
        RedstoneTorchModel[] torches = new RedstoneTorchModel[]{
                new RedstoneTorchModel(4, 8, 6),
                new RedstoneTorchModel(12, 8, 6),
                new RedstoneTorchModel(8, 8, 6),
                new RedstoneTorchModel(8, 2, 8)
            };

        public AND() {
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }
        
        @Override
        public void prepareInv() {
            wires[0].on = true;
            wires[1].on = false;
            wires[2].on = false;
            wires[3].on = false;
            wires[1].disabled = false;
            wires[2].disabled = false;
            wires[3].disabled = false;
            torches[0].on = true;
            torches[1].on = true;
            torches[2].on = true;
            torches[3].on = false;
        }
        
        @Override
        public void prepare(SimpleGatePart part) {
            wires[0].on = (part.state&0x11) == 0;
            wires[3].on = (part.state&2) != 0;
            wires[1].on = (part.state&4) != 0;
            wires[2].on = (part.state&8) != 0;
            wires[3].disabled = (part.shape&1) != 0;
            wires[1].disabled = (part.shape&2) != 0;
            wires[2].disabled = (part.shape&4) != 0;
            torches[2].on = !wires[1].on && !wires[1].disabled;
            torches[0].on = !wires[2].on && !wires[2].disabled;;
            torches[1].on = !wires[3].on && !wires[3].disabled;;
            torches[3].on = !wires[0].on;;
        }
    }
    
    public static class NAND extends GateRenderer<SimpleGatePart>
    {   
        WireComponentModel[] wires = generateWireModels("NAND", 4);
        RedstoneTorchModel[] torches = new RedstoneTorchModel[]{
                new RedstoneTorchModel(4, 8, 6),
                new RedstoneTorchModel(12, 8, 6),
                new RedstoneTorchModel(8, 8, 6)
        };
        
        public NAND() {
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }
        
        @Override
        public void prepareInv() {
            wires[0].on = true;
            wires[1].on = false;
            wires[2].on = false;
            wires[3].on = false;
            wires[1].disabled = false;
            wires[2].disabled = false;
            wires[3].disabled = false;
            torches[0].on = true;
            torches[1].on = true;
            torches[2].on = true;
        }
        
        @Override
        public void prepare(SimpleGatePart part) {
            wires[0].on = (part.state&0x11) != 0;
            wires[3].on = (part.state&2) != 0;
            wires[1].on = (part.state&4) != 0;
            wires[2].on = (part.state&8) != 0;
            wires[3].disabled = (part.shape&1) != 0;
            wires[1].disabled = (part.shape&2) != 0;
            wires[2].disabled = (part.shape&4) != 0;
            torches[0].on = !wires[2].on && !wires[2].disabled;;
            torches[1].on = !wires[3].on && !wires[3].disabled;;
            torches[2].on = !wires[1].on && !wires[1].disabled;
        }
        
    }

    public static class XOR extends GateRenderer<SimpleGatePart>
    {
        WireComponentModel[] wires = generateWireModels("XOR", 4);
        RedstoneTorchModel[] torches = new RedstoneTorchModel[]{
                new RedstoneTorchModel(4.5, 8, 6),
                new RedstoneTorchModel(11.5, 8, 6),
                new RedstoneTorchModel(8, 12, 6)
        };

        public XOR() {
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }

        @Override
        public void prepareInv() {
            wires[0].on = false;
            wires[3].on = false;
            wires[2].on = false;
            wires[1].on = true;
            torches[0].on = false;
            torches[1].on = false;
            torches[2].on = true;
        }
        
        @Override
        public void prepare(SimpleGatePart part) {
            wires[0].on = (part.state&0x11) != 0;
            wires[3].on = (part.state&2) != 0;
            wires[2].on = (part.state&8) != 0;
            wires[1].on = !wires[3].on && !wires[2].on;
            torches[0].on = !wires[2].on && ! wires[1].on;
            torches[1].on = !wires[3].on && ! wires[1].on;
            torches[2].on = wires[1].on;
        }
    }
    
    public static class XNOR extends GateRenderer<SimpleGatePart>
    {
        WireComponentModel[] wires = generateWireModels("XNOR", 5);
        RedstoneTorchModel[] torches = new RedstoneTorchModel[]{
                new RedstoneTorchModel(8, 2, 8),
                new RedstoneTorchModel(4.5, 8, 6),
                new RedstoneTorchModel(11.5, 8, 6),
                new RedstoneTorchModel(8, 12, 6)
        };
        
        public XNOR() {
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }
        
        @Override
        public void prepareInv() {
            wires[0].on = false;
            wires[3].on = false;
            wires[2].on = false;
            wires[1].on = false;
            torches[0].on = true;
            torches[1].on = false;
            torches[2].on = false;
            torches[3].on = true;
        }
        
        @Override
        public void prepare(SimpleGatePart part) {
            wires[0].on = (part.state&2) != 0 && (part.state&8) == 0;
            wires[1].on = (part.state&8) != 0 && (part.state&2) == 0;
            wires[2].on = (part.state&8) != 0;
            wires[3].on = (part.state&2) != 0;
            wires[4].on = !wires[3].on && !wires[2].on;
            torches[0].on = (part.state&0x11) != 0;
            torches[1].on = !wires[4].on && (part.state&8) == 0;
            torches[2].on = !wires[4].on && (part.state&2) == 0;
            torches[3].on = (part.state&2) == 0 && (part.state&8) == 0;
        }
    }
    
    public static class Buffer extends GateRenderer<SimpleGatePart>
    {
        WireComponentModel[] wires = generateWireModels("BUFFER", 4);
        RedstoneTorchModel[] torches = new RedstoneTorchModel[]{
                new RedstoneTorchModel(8, 2, 8),
                new RedstoneTorchModel(8, 9, 6),
        };
     
        public Buffer() {
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }
        
        @Override
        public void prepareInv() {
            wires[0].on = true;
            wires[1].on = false;
            wires[2].on = false;
            wires[3].on = false;
            wires[1].disabled  = false;
            wires[3].disabled  = false;
            torches[0].on = false;
            torches[1].on = true;
        }
        
        @Override
        public void prepare(SimpleGatePart part) {
            wires[0].on = (part.state&4) == 0;
            wires[1].on = (part.state&0x22) != 0;
            wires[2].on = (part.state&0x44) != 0;
            wires[3].on = (part.state&0x88) != 0;
            wires[1].disabled = (part.shape&1) != 0;
            wires[3].disabled = (part.shape&2) != 0;
            torches[0].on = (part.state&4) != 0;
            torches[1].on = (part.state&4) == 0;
        }
    }
    
    public static class Multiplexer extends GateRenderer<SimpleGatePart>
    {
        WireComponentModel[] wires = generateWireModels("MULTIPLEXER", 6);
        RedstoneTorchModel[] torches = new RedstoneTorchModel[]{
                new RedstoneTorchModel(8, 2, 8),
                new RedstoneTorchModel(9, 10.5, 6),
                new RedstoneTorchModel(4.5, 8, 6),
                new RedstoneTorchModel(11.5, 8, 6),
        };
     
        public Multiplexer() {
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }
        
        @Override
        public void prepareInv() {
            wires[0].on = false;
            wires[1].on = true;
            wires[2].on = true;
            wires[3].on = false;
            wires[4].on = false;
            wires[5].on = false;
            torches[0].on = false;
            torches[1].on = true;
            torches[2].on = false;
            torches[3].on = true;
        }
        
        @Override
        public void prepare(SimpleGatePart part) {
            wires[2].on = (part.state&4) == 0;
            wires[3].on = (part.state&4) != 0;
            wires[4].on = (part.state&8) != 0;
            wires[5].on = (part.state&2) != 0;
            torches[0].on = (part.state&0x10) != 0;
            torches[1].on = !wires[3].on;
            torches[2].on = (part.state&8) == 0 && wires[3].on;
            torches[3].on = (part.state&4) == 0 && !wires[5].on;
            wires[0].on = torches[2].on;
            wires[1].on = torches[1].on;
        }
    }
    
    public static class Pulse extends GateRenderer<SimpleGatePart>
    {
        WireComponentModel[] wires = generateWireModels("PULSE", 3);
        RedstoneTorchModel[] torches = new RedstoneTorchModel[]{
                new RedstoneTorchModel(4, 9.5, 6),
                new RedstoneTorchModel(11, 9.5, 6),
                new RedstoneTorchModel(8, 3.5, 8)
            };
        
        public Pulse() {
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }
        
        @Override
        public void prepareInv() {
            wires[0].on = true;
            wires[1].on = false;
            wires[2].on = false;
            torches[0].on = true;
            torches[1].on = false;
            torches[2].on = false;
        }
        
        @Override
        public void prepare(SimpleGatePart part) {
            wires[0].on = (part.state&4) == 0;
            wires[1].on = (part.state&4) != 0;
            wires[2].on = (part.state&0x14) == 4;//input on, output off
            torches[0].on = wires[0].on;
            torches[1].on = wires[1].on;
            torches[2].on = (part.state&0x10) != 0;
        }
    }

    public static class Repeater extends GateRenderer<SimpleGatePart>
    {
        WireComponentModel[] wires = generateWireModels("REPEATER", 2);
        RedstoneTorchModel endtorch = new RedstoneTorchModel(8, 2, 6);
        RedstoneTorchModel[] vartorches = new RedstoneTorchModel[]{
                new RedstoneTorchModel(12.5, 12, 6),
                new RedstoneTorchModel(12.5, 11, 6),
                new RedstoneTorchModel(12.5, 10, 6),
                new RedstoneTorchModel(12.5, 9, 6),
                new RedstoneTorchModel(12.5, 8, 6),
                new RedstoneTorchModel(12.5, 7, 6),
                new RedstoneTorchModel(12.5, 6, 6),
                new RedstoneTorchModel(12.5, 5, 6),
                new RedstoneTorchModel(12.5, 4, 6)
            };
        int shape = 0;
        
        public Repeater() {
            models.addAll(Arrays.asList(wires));
            models.add(endtorch);
        }
        
        @Override
        public void prepareInv() {
            wires[0].on = true;
            wires[1].on = false;
            endtorch.on = true;
            shape = 0;
            vartorches[0].on = false;
        }
        
        @Override
        public void prepare(SimpleGatePart part) {
            wires[0].on = (part.state&0x10) == 0;
            wires[1].on = (part.state&4) != 0;
            endtorch.on = (part.state&0x10) != 0;
            
            shape = part.shape();
            vartorches[shape].on = (part.state&4) == 0;
        }
        
        @Override
        public void renderModels(Transformation t, int orient) {
            super.renderModels(t, orient);
            vartorches[shape].renderModel(t, orient);
        }
    }

    public static class Randomizer extends GateRenderer<SimpleGatePart>
    {
        WireComponentModel[] wires = generateWireModels("RANDOM", 4);
        YellowChipModel[] chips = new YellowChipModel[] {
                new YellowChipModel(8, 5.5),
                new YellowChipModel(11.5, 12),
                new YellowChipModel(4.5, 12),                
            };
        
        public Randomizer() {
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(chips));
        }
        
        @Override
        public void prepareInv() {
            wires[0].on = false;
            wires[1].on = false;
            wires[2].on = false;
            wires[3].on = false;
            chips[0].on = false;
            chips[1].on = false;
            chips[2].on = false;
        }
        
        @Override
        public void prepare(SimpleGatePart part) {
            wires[1].on = (part.state&4) != 0;
            wires[0].on = (part.state&0x11) != 0;
            wires[3].on = (part.state&0x22) != 0;
            wires[2].on = (part.state&0x88) != 0;
            chips[0].on = (part.state&0x10) != 0;
            chips[1].on = (part.state&0x20) != 0;
            chips[2].on = (part.state&0x80) != 0;
        }
    }
    
    public static class RSLatch extends GateRenderer<InstancedRsGatePart>
    {
        WireComponentModel[] wires1 = generateWireModels("RSLATCH", 2);
        WireComponentModel[] wires2 = generateWireModels("RSLATCH2", 4);
        RedstoneTorchModel[] torches1 = new RedstoneTorchModel[]{
                new RedstoneTorchModel(8, 3, 6),
                new RedstoneTorchModel(8, 13, 6)
            };
        RedstoneTorchModel[] torches2 = new RedstoneTorchModel[]{
                new RedstoneTorchModel(9.5, 3, 6),
                new RedstoneTorchModel(6.5, 13, 6)
            };
        int type = 0;
        
        @Override
        public void prepareInv() {
            reflect = false;
            type = 0;
            wires1[0].on = false;
            wires1[1].on = true;
            torches1[0].on = false;
            torches1[1].on = true;
        }
        
        @Override
        public void prepare(InstancedRsGatePart part) {
            reflect = (part.shape&1) != 0;
            type = part.shape >> 1;
            int state = part.state();
            if(reflect)
                state = GatePart.flipMaskZ(state>>4)<<4 | GatePart.flipMaskZ(state);
            
            if(type == 0) {
                wires1[0].on = (state&0x88) != 0;
                wires1[1].on = (state&0x22) != 0;
                torches1[0].on = (state&0x10) != 0;
                torches1[1].on = (state&0x40) != 0;
            }
            else {
                wires2[1].on = (state&2) != 0;
                wires2[3].on = (state&8) != 0;
                torches2[0].on = (state&0x10) != 0;
                torches2[1].on = (state&0x40) != 0;
                wires2[0].on = torches2[1].on;
                wires2[2].on = torches2[0].on;
            }
        }
        
        @Override
        public void renderModels(Transformation t, int orient) {
            super.renderModels(t, orient);

            for(ComponentModel m : type == 0 ? wires1 : wires2)
                m.renderModel(t, orient);
            for(ComponentModel m : type == 0 ? torches1 : torches2)
                m.renderModel(t, orient);
        }
        
        @Override
        public void registerIcons(IconRegister r) {
            for(ComponentModel m : wires1)
                m.registerTextures(r);
            for(ComponentModel m : wires2)
                m.registerTextures(r);
        }
    }
    
    public static class ToggleLatch extends GateRenderer<InstancedRsGatePart>
    {
        WireComponentModel[] wires = generateWireModels("TOGLATCH", 2);
        RedstoneTorchModel[] torches = new RedstoneTorchModel[]{
                new RedstoneTorchModel(4, 4, 6),
                new RedstoneTorchModel(4, 12, 6)
        };
        LeverModel lever = new LeverModel(11, 8);

        public ToggleLatch() {
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
            models.add(lever);
        }

        @Override
        public void prepareInv() {
            wires[0].on = false;
            wires[1].on = false;
            torches[0].on = true;
            torches[1].on = false;
            lever.state = 0;
        }
        
        @Override
        public void prepare(InstancedRsGatePart part) {
            wires[0].on = (part.state&8) != 0;
            wires[1].on = (part.state&2) != 0;
            torches[0].on = (part.state&0x10) != 0;
            torches[1].on = (part.state&0x40) != 0;
            lever.state = ((ExtraStateLogic)part.getLogic()).state2 != 0 ? 1 : 0;
        }
    }
    
    public static class TransparentLatch extends GateRenderer<SimpleGatePart>
    {
        WireComponentModel[] wires = generateWireModels("TRANSLATCH", 5);
        RedstoneTorchModel[] torches = new RedstoneTorchModel[]{
                new RedstoneTorchModel(4, 12.5, 6),
                new RedstoneTorchModel(4, 8, 6),
                new RedstoneTorchModel(8, 8, 6),
                new RedstoneTorchModel(8, 2, 8),
                new RedstoneTorchModel(14, 8, 8)
            };
        
        public TransparentLatch() {
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }
        
        @Override
        public void prepareInv() {
            reflect = false;
            wires[0].on = true;
            wires[1].on = false;
            wires[2].on = true;
            wires[3].on = false;
            wires[4].on = false;
            torches[0].on = true;
            torches[1].on = false;
            torches[2].on = true;
            torches[3].on = false;
            torches[4].on = false;
        }
        
        @Override
        public void prepare(SimpleGatePart part) {
            reflect = part.shape() == 1;
            boolean on = (part.state&0x10) != 0;
            
            wires[0].on = !on;
            wires[1].on = (part.state&4) != 0;
            wires[2].on = (part.state&4) == 0;
            wires[3].on = on;
            wires[4].on = (part.state&0xA) != 0;
            
            torches[0].on = wires[2].on;
            torches[1].on = !wires[2].on && !wires[4].on;
            torches[2].on = !wires[1].on && !wires[3].on;
            torches[3].on = on;
            torches[4].on = on;
        }
    }
    
    public static class LightSensor extends GateRenderer<SimpleGatePart>
    {
        WireComponentModel[] wires = generateWireModels("LIGHTSENSOR", 1);
        SolarModel solar = new SolarModel(8, 5.5);
        
        public LightSensor() {
            models.addAll(Arrays.asList(wires));
            models.add(solar);
        }
        
        @Override
        public void prepareInv() {
            wires[0].on = false;
            solar.state = 0;
        }
        
        @Override
        public void prepare(SimpleGatePart part) {
            wires[0].on = (part.state&0xF4) != 0;
            solar.state = part.shape();
        }
    }
    
    public static class RainSensor extends GateRenderer<SimpleGatePart>
    {
        WireComponentModel[] wires = generateWireModels("RAINSENSOR", 1);
        RainSensorModel solar = new RainSensorModel(8, 6);
        
        public RainSensor() {
            models.addAll(Arrays.asList(wires));
            models.add(solar);
        }
        
        @Override
        public void prepareInv() {
            wires[0].on = false;
        }
        
        @Override
        public void prepare(SimpleGatePart part) {
            wires[0].on = (part.state&0x44) != 0;
        }
    }
    
    public static class Timer extends GateRenderer<InstancedRsGatePart>
    {
        WireComponentModel[] wires = generateWireModels("TIME", 3);
        RedstoneTorchModel[] torches = new RedstoneTorchModel[]{
                new RedstoneTorchModel(8, 3, 6),
                new RedstoneTorchModel(8, 8, 12)
            };
        PointerModel pointer = new PointerModel(8, 8, 8);
        
        public Timer() {
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
            torches[1].on = true;
        }
        
        @Override
        public void prepare(InstancedRsGatePart part) {
            torches[0].on = (part.state&0x10) != 0;
            wires[0].on = (part.state&0x88) != 0;
            wires[1].on = (part.state&0x22) != 0;
            wires[2].on = (part.state&4) != 0;
        }
        
        @Override
        public void prepareInv() {
            wires[0].on = false;
            wires[1].on = false;
            wires[2].on = false;
            torches[0].on = false;
            pointer.angle = 0;
        }
        
        @Override
        public boolean hasSpecials() {
            return true;
        }
        
        @Override
        public void prepareDynamic(InstancedRsGatePart part, float frame) {
            pointer.angle = ((TimerGateLogic)part.getLogic()).interpPointer(frame)*MathHelper.pi*2;
        }
        
        @Override
        public void renderDynamic(Transformation t) {
            CCRenderState.startDrawing(7);
            CCRenderState.pullLightmap();
            CCRenderState.useNormals(true);
            pointer.renderModel(t, 0);
            CCRenderState.draw();
        }
    }
    
    public static class Sequencer extends GateRenderer<InstancedRsGatePart>
    {
        RedstoneTorchModel[] torches = new RedstoneTorchModel[]{
                new RedstoneTorchModel(8, 8, 12),
                new RedstoneTorchModel(8, 3, 6),
                new RedstoneTorchModel(13, 8, 6),
                new RedstoneTorchModel(8, 13, 6),
                new RedstoneTorchModel(3, 8, 6),
        };
        PointerModel pointer = new PointerModel(8, 8, 8);
        
        public Sequencer() {
            models.addAll(Arrays.asList(torches));
            torches[0].on = true;
        }
        
        @Override
        public void prepare(InstancedRsGatePart gate) {
            torches[1].on = (gate.state&0x10) != 0;
            torches[2].on = (gate.state&0x20) != 0;
            torches[3].on = (gate.state&0x40) != 0;
            torches[4].on = (gate.state&0x80) != 0;
        }
        
        @Override
        public void prepareInv() {
            torches[1].on = false;
            torches[2].on = false;
            torches[3].on = false;
            torches[4].on = false;
            pointer.angle = 0;
        }
                
        @Override
        public void prepareDynamic(InstancedRsGatePart gate, float frame) {
            int max = ((InstancedRsGateLogic.Sequencer)gate.getLogic()).pointer_max;
            pointer.angle = (gate.world().getWorldTime()%max + frame)/max*2*MathHelper.pi;
            if (gate.shape() == 1) 
                pointer.angle = -pointer.angle;
        }
        
        @Override
        public boolean hasSpecials() {
            return true;
        }

        @Override
        public void renderDynamic(Transformation t) {
            CCRenderState.startDrawing(7);
            CCRenderState.pullLightmap();
            CCRenderState.useNormals(true);
            pointer.renderModel(t, 0);
            CCRenderState.draw();
        }
    }
    
    public static class Counter extends GateRenderer<InstancedRsGatePart>
    {
        WireComponentModel[] wires = generateWireModels("COUNT", 2);
        RedstoneTorchModel[] torches = new RedstoneTorchModel[]{
                new RedstoneTorchModel(11, 8, 12),
                new RedstoneTorchModel(8, 3, 6),
                new RedstoneTorchModel(8, 13, 6),
        };
        PointerModel pointer = new PointerModel(11, 8, 8, 1.2D);
        
        public Counter() {
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
            torches[0].on = true;
        }
        
        @Override
        public void prepare(InstancedRsGatePart gate) {
            reflect = gate.shape() == 1;
            wires[0].on = (gate.state()&8) != 0;
            wires[1].on = (gate.state()&2) != 0;
            torches[1].on = (gate.state()&0x10) != 0;
            torches[2].on = (gate.state()&0x40) != 0;
        }
        
        @Override
        public void prepareInv() {
            reflect = false;
            wires[0].on = false;
            wires[1].on = false;
            torches[1].on = false;
            torches[2].on = true;
            pointer.angle = 220 * MathHelper.torad;
        }

        @Override
        public void prepareDynamic(InstancedRsGatePart gate, float frame) {
            int max = ((InstancedRsGateLogic.Counter)gate.getLogic()).max;
            int val = ((InstancedRsGateLogic.Counter)gate.getLogic()).value;
            pointer.angle = (val/(double)max * (340-220) + 210)*MathHelper.torad;
            if (gate.shape() == 1)  {
                reflect = true;
            }
        }
        
        @Override
        public boolean hasSpecials() {
            return true;
        }

        @Override
        public void renderDynamic(Transformation t) {
            CCRenderState.startDrawing(7);
            CCRenderState.pullLightmap();
            CCRenderState.useNormals(true);
            pointer.renderModel(t, reflect ? 1 : 0);
            CCRenderState.draw();
        }
    }
    
    public static class StateCell extends GateRenderer<InstancedRsGatePart>
    {
        WireComponentModel[] wires = generateWireModels("STATECELL", 5);
        RedstoneTorchModel[] torches = new RedstoneTorchModel[]{
                new RedstoneTorchModel(10, 3.5, 6),
                new RedstoneTorchModel(13, 8, 12)
            };
        PointerModel pointer = new PointerModel(13, 8, 8);
        RedChipModel chip = new RedChipModel(6.5, 10);
        
        public StateCell() {
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
            models.add(chip);
        }
        
        public InstancedRsGateLogic.StateCell getLogic(InstancedRsGatePart part) {
            return (InstancedRsGateLogic.StateCell)part.getLogic();
        }
        
        @Override
        public void prepareInv() {
            reflect = false;
            wires[0].on = false;
            wires[1].on = false;
            wires[2].on = false;
            wires[3].on = false;
            wires[4].on = false;
            torches[0].on = false;
            torches[1].on = true;
            chip.on = false;
            pointer.angle = -MathHelper.pi/2;
        }
        
        @Override
        public void prepare(InstancedRsGatePart part) {
            reflect = part.shape == 1;
            int state = part.state();
            if(reflect)
                state = GatePart.flipMaskZ(state>>4)<<4 | GatePart.flipMaskZ(state);
            
            wires[0].on = (state & 0x10) != 0;
            wires[1].on = (state & 4) != 0;
            wires[2].on = getLogic(part).state2 == 0 || (state & 4) != 0;
            wires[3].on = (state & 0x88) != 0;
            wires[4].on = (state & 2) != 0;
            torches[0].on = (state & 0x10) != 0;
            torches[1].on = getLogic(part).pointer_start >= 0;
            chip.on = getLogic(part).state2 != 0;
        }
        
        @Override
        public boolean hasSpecials() {
            return true;
        }
        
        @Override
        public void prepareDynamic(InstancedRsGatePart part, float frame) {
            reflect = part.shape == 1;
            pointer.angle = getLogic(part).interpPointer(frame)-MathHelper.pi/2;
        }
        
        @Override
        public void renderDynamic(Transformation t) {
            CCRenderState.startDrawing(7);
            CCRenderState.pullLightmap();
            CCRenderState.useNormals(true);
            pointer.renderModel(t, reflect ? 1 : 0);
            CCRenderState.draw();
        }
    }
    
    public static class Synchronizer extends GateRenderer<InstancedRsGatePart>
    {
        WireComponentModel[] wires = generateWireModels("SYNC", 6);
        RedstoneTorchModel torch = new RedstoneTorchModel(8, 3, 6);
        RedChipModel[] chips = new RedChipModel[] {
                new RedChipModel(4.5, 9),
                new RedChipModel(11.5, 9),
        };
        
        public Synchronizer() {
            models.addAll(Arrays.asList(wires));
            models.add(torch);
            models.addAll(Arrays.asList(chips));
        }
        
        @Override
        public void prepareInv() {
            wires[0].on = true;
            wires[1].on = true;
            wires[2].on = false;
            wires[3].on = false;
            wires[4].on = false;
            wires[5].on = false;
            chips[0].on = false;
            chips[1].on = false;
            torch.on = false;
        }
        
        @Override
        public void prepare(InstancedRsGatePart gate) {
            InstancedRsGateLogic.Synchronizer logic = (InstancedRsGateLogic.Synchronizer) gate.getLogic();
            wires[0].on = !logic.left();
            wires[1].on = !logic.right();
            wires[2].on = (gate.state() & 4) != 0;
            wires[3].on = logic.left() && logic.right();
            wires[4].on = (gate.state() & 8) != 0;
            wires[5].on = (gate.state() & 2) != 0;
            chips[0].on = logic.left();
            chips[1].on = logic.right();
            torch.on = (gate.state() & 0x10) != 0;
        }
        
    }
    
    public static class BusXcvr extends GateRenderer<BundledGatePart>
    {
        WireComponentModel[] wires = generateWireModels("BUSXCVR", 2);
        BusXcvrCableModel cable = new BusXcvrCableModel();
        BusXcvrPanelModel[] panels = new BusXcvrPanelModel[] {
                new BusXcvrPanelModel(4, 8, false),
                new BusXcvrPanelModel(12, 8, true),
        };
        public BusXcvr () {
            models.add(cable);
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(panels));
        }
        
        @Override
        public void prepareInv() {
            reflect = false;
            wires[0].on = false;
            wires[1].on = false;
            panels[0].signal = 0;
            panels[1].signal = 0;
        }
        
        @Override
        public void prepare(BundledGatePart gate) {
            reflect = gate.shape() != 0;
            int state = gate.state();
            if(reflect)
                state = GatePart.flipMaskZ(state);
            
            wires[0].on = (gate.state()&2) != 0;
            wires[1].on = (gate.state()&8) != 0;
            
            BusTransceiver logic = (BusTransceiver) gate.getLogic();
            int packed = logic.packClientData();
            panels[0].signal = packed & 0xFFFF;
            panels[1].signal = packed >>> 16;
        }
    }
    
    public static class ArrayCell extends GateRenderer<ArrayGatePart>
    {
        public CellBottomWireModel bottomWire;
        public CellTopWireModel topWire;
        
        public ArrayCell(CellTopWireModel topWire, CellBottomWireModel bottomWire) {
            this.topWire = topWire;
            this.bottomWire = bottomWire;
            models.clear();
            models.add(topWire);
            models.add(bottomWire);
            models.add(new CellFrameModel());
        }
        
        @Override
        public void prepareInv() {
            bottomWire.signal = 0;
            bottomWire.invColour = true;
            topWire.signal = 0;
            topWire.invColour = true;
            topWire.conn = 0;
        }
        
        @Override
        public void prepare(ArrayGatePart part) {
            super.prepare(part);
            bottomWire.signal = part.signal1;
            bottomWire.invColour = false;
            topWire.signal = part.signal2;
            topWire.conn = ArrayCommons.topWireConn(part);
            topWire.invColour = false;
        }
    }
    
    public static class NullCell extends ArrayCell
    {
        public NullCell() {
            super(new CellTopWireModel(nullCellWireTop), new CellBottomWireModel(nullCellWireBottom));
            models.add(new NullCellBaseModel());
        }
    }
    
    public static class InvertCell extends ArrayCell
    {
        WireComponentModel[] wires = generateWireModels("INVCELL", 1);
        RedstoneTorchModel torch = new RedstoneTorchModel(8, 8, 6);

        public InvertCell() {
            super(new CellTopWireModel(extendedCellWireTop), new CellBottomWireModel(extendedCellWireBottom));
            models.add(new ExtendedCellBaseModel());
            models.add(new CellPlateModel());
            models.addAll(Arrays.asList(wires));
            models.add(torch);
        }
        
        @Override
        public void prepareInv() {
            super.prepareInv();
            wires[0].on = false;
            torch.on = true;
        }
        
        @Override
        public void prepare(ArrayGatePart gate) {
            super.prepare(gate);
            torch.on = gate.signal1 == 0;
            wires[0].on = gate.signal1 != 0;
        }
    }
    
    public static class BufferCell extends ArrayCell
    {
        WireComponentModel[] wires = generateWireModels("BUFFCELL", 2);
        RedstoneTorchModel torches[] = new RedstoneTorchModel[] {
                new RedstoneTorchModel(11, 13, 6),
                new RedstoneTorchModel(8, 8, 6)                
        };

        public BufferCell() {
            super(new CellTopWireModel(extendedCellWireTop), new CellBottomWireModel(extendedCellWireBottom));
            models.add(new ExtendedCellBaseModel());
            models.add(new CellPlateModel());
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }
        
        @Override
        public void prepareInv() {
            super.prepareInv();
            wires[0].on = false;
            wires[1].on = true;
            torches[0].on = true;
            torches[1].on = false;
        }
        
        @Override
        public void prepare(ArrayGatePart gate) {
            super.prepare(gate);
            torches[0].on = gate.signal1 == 0;
            torches[1].on = gate.signal1 != 0;
            wires[0].on = gate.signal1 != 0;
            wires[1].on = gate.signal1 == 0;
        }
    }
    
    public static class Comparator extends GateRenderer<InstancedRsGatePart>
    {
        WireComponentModel[] wires = generateWireModels("COMPARATOR", 4);

        RedstoneTorchModel torch = new RedstoneTorchModel(8, 2, 6);
        
        OnOffModel[] chips = new OnOffModel[] {
                new MinusChipModel(5, 8),
                new PlusChipModel(11, 8)
        };
                
        public Comparator() {
            models.addAll(Arrays.asList(wires));
            models.add(torch);
        }
        
        @Override
        public void prepareInv() {
            reflect = false;
            wires[0].on = false;
            wires[1].on = false;
            wires[2].on = false;
            wires[3].on = false;
            chips[0].on = false;
            chips[1].on = false;
            torch.on = false;
        }
        
        public void prepare(InstancedRsGatePart gate) {
            reflect = gate.shape() != 0;
            
            wires[0].on = (gate.state&0x10) == 0;
            wires[1].on = (gate.state&2) != 0;
            wires[2].on = (gate.state&4) != 0;
            wires[3].on = (gate.state&8) != 0;
            chips[0].on = (gate.state&1) != 0 && gate.shape == 1;
            chips[1].on = (gate.state&1) != 0 && gate.shape != 1;
            torch.on = (gate.state&0x10) != 0;
            
            if(gate.shape() != 0) {
                boolean a = wires[1].on;
                boolean b = wires[3].on;
                wires[3].on = a;
                wires[1].on = b;
            }
        }
        
        @Override
        public void renderModels(Transformation t, int orient) {
            super.renderModels(t, orient);
            chips[0].renderModel(t, orient%24);
            chips[1].renderModel(t, orient%24);
        }
    }
    
    public static class ANDCell extends GateRenderer<RowGatePart>
    {
        WireComponentModel[] wires = generateWireModels("ANDCELL", 2);
        CellTopWireModel topWire = new CellTopWireModel(nullCellWireTop);
        RedstoneTorchModel[] torches = new RedstoneTorchModel[]{
                new RedstoneTorchModel(8, 13, 6),
                new RedstoneTorchModel(8, 2, 8),
                new FlippedRSTorchModel(8,8)
            };
        
        public ANDCell() {
        	models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
            models.add(topWire);
            models.add(new CellFrameModel());
        }
        
        @Override
        public void prepareInv() {
            topWire.signal = 0;
            topWire.invColour = true;
            topWire.conn = 0;
            torches[0].on = false;
            torches[1].on = false;
            torches[2].on = true;
            wires[0].on = true;
            wires[1].on = false;
        }
        
        @Override
        public void prepare(RowGatePart part) {
            super.prepare(part);
            topWire.signal = part.signal;
            topWire.conn = ArrayCommons.topWireConn(part);
            topWire.invColour = false;
            torches[0].on = (part.state & 4) == 0;
            torches[1].on = (part.state & 0x10) != 0;
            torches[2].on = (part.state & 0xA) == 0;            
            wires[0].on = torches[0].on || torches[2].on;
            wires[1].on = !torches[0].on;
        }
    }
    
    public static class BusEye extends GateRenderer<BundledGatePart>
    {
        BusXcvrCableModel cable = new BusXcvrCableModel();
        BusXcvrPanelModel panel = new BusXcvrPanelModel(8, 8, true);
        public BusEye () {
            models.add(cable);
            models.add(panel);
        }
        
        @Override
        public void prepareInv() {
            reflect = false;
            panel.signal = 0;
        }
        
        @Override
        public void prepare(BundledGatePart gate) {
            reflect = gate.shape() != 0;
            int state = gate.state();
            if(reflect)
                state = GatePart.flipMaskZ(state);

            BusDisplay logic = (BusDisplay) gate.getLogic();
            int packed = logic.packClientData();
            panel.signal = packed & 0xFFFF;
        }
    }
}
