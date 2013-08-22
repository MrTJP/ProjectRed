package mrtjp.projectred.integration2;

import static mrtjp.projectred.integration2.ComponentStore.base;
import static mrtjp.projectred.integration2.ComponentStore.baseIcon;
import static mrtjp.projectred.integration2.ComponentStore.generateWireModels;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import mrtjp.projectred.integration2.ComponentStore.ComponentModel;
import mrtjp.projectred.integration2.ComponentStore.RedstoneTorchModel;
import mrtjp.projectred.integration2.ComponentStore.SimpleComponentModel;
import mrtjp.projectred.integration2.ComponentStore.WireComponentModel;
import codechicken.lib.render.CCRenderState;
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
            null,
            null,
            null,
            null,
            null,
            null,
            new Pulse()
        };
    
    public static void renderStatic(GatePart gate) {
        GateRenderer r = renderers[gate.subID&0xFF];
        r.prepare(gate);
        r.renderStatic(gate.rotationT().with(new Translation(gate.x(), gate.y(), gate.z())));
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
        r.prepareInv();
        CCRenderState.startDrawing(7);
        r.renderStatic(t);
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
        
        public GateRenderer() {
            models.add(new SimpleComponentModel(base, baseIcon));
        }
        
        public void renderStatic(Transformation t) {
            for(ComponentModel m : models)
                m.render(t);
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
                    pos.add(0, m.lightY, 0).apply(m.relPos);//height
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
            wires[0].on = (part.state&0x10) != 0;
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
            wires[2].on = true;
            wires[3].on = true;
            wires[1].on = false;
            torch.on = true;
        }
        
        @Override
        public void prepare(SimpleGatePart part) {
            wires[0].on = (part.state&0x11) != 0;
            wires[3].on = (part.state&0x22) != 0;
            wires[1].on = (part.state&4) != 0;
            wires[2].on = (part.state&0x88) != 0;
            torch.on = (part.state&0xF0) != 0;
        }
    }

    public static class Pulse extends GateRenderer<SimpleGatePart>
    {
        WireComponentModel[] wires = generateWireModels("PULSE", 3);
        RedstoneTorchModel[] torches = new RedstoneTorchModel[]{
                new RedstoneTorchModel(4.5, 8.5, 6),
                new RedstoneTorchModel(12.5, 8.5, 6),
                new RedstoneTorchModel(8.5, 3, 8)
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
}
