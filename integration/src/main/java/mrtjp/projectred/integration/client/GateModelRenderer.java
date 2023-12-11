package mrtjp.projectred.integration.client;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.math.MathHelper;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.buffer.TransformingVertexConsumer;
import codechicken.lib.texture.AtlasRegistrar;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.math.Vector3f;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.part.GatePart;
import mrtjp.projectred.integration.part.IGateRenderData;
import mrtjp.projectred.lib.VecLib;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.core.BlockPos;
import net.minecraft.core.particles.DustParticleOptions;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.packs.resources.ResourceManager;
import net.minecraft.util.RandomSource;
import net.minecraft.world.item.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import static mrtjp.projectred.core.part.IOrientableFacePart.flipMaskZ;
import static mrtjp.projectred.integration.client.GateComponentModels.*;

public class GateModelRenderer {

    private static final ThreadLocal<GateModelRenderer> INSTANCES = ThreadLocal.withInitial(GateModelRenderer::new);

    public static GateModelRenderer instance() {
        return INSTANCES.get();
    }

    private final GateRenderer[] renderers = new GateRenderer[] {
            new RenderOR(),
            new RenderNOR(),
            new RenderNOT(),
            new RenderAND(),
            new RenderNAND(),
            new RenderXOR(),
            new RenderXNOR(),
            new RenderBuffer(),
            new RenderMultiplexer(),
            new RenderPulse(),
            new RenderRepeater(),
            new RenderRandomizer(),
            new RenderSRLatch(),
            new RenderToggleLatch(),
            new RenderTransparentLatch(),
            new RenderLightSensor(),
            new RenderRainSensor(),
            new RenderTimer(),
            new RenderSequencer(),
            new RenderCounter(),
            new RenderStateCell(),
            new RenderSynchronizer(),
            new RenderBusXcvr(),
            new RenderNullCell(),
            new RenderInvertCell(),
            new RenderBufferCell(),
            new RenderComparator(),
            new RenderANDCell(),
            new RenderBusRandomizer(),
            new RenderBusConverter(),
            new RenderBusInputPanel(),
            new RenderTransparentLatchCell(),
            new RenderSegmentDisplay(),
            new RenderDecodingRandomizer(),
            new RenderFabricatedGate(),
    };

    private final GateRenderer[] nonPartRenderers = new GateRenderer[] {
            new RenderIOGate(),
    };

    //region Static rendering
    public void renderStatic(CCRenderState ccrs, IGateRenderData key, Transformation t) {
        GateRenderer r = getRenderer(key.getRenderIndex());
        r.prepare(key);
        r.renderStatic(ccrs, key.getOrientation(), t);
    }
    //endregion

    //region Dynamic rendering
    public void renderDynamic(CCRenderState ccrs, IGateRenderData key, Transformation t, float partialFrame) {
        renderDynamic(ccrs, key, t, null, null, 0, 0, partialFrame);
    }

    public void renderDynamic(CCRenderState ccrs, IGateRenderData key, Transformation t, @Nullable PoseStack mStack, @Nullable MultiBufferSource buffers, int packedLight, int packedOverlay, float partialTicks) {
        GateRenderer r = getRenderer(key.getRenderIndex());
        if (r.hasSpecials()) {
            Transformation t2 = VecLib.orientT(key.getOrientation()).with(t);
            r.prepareDynamic(key, partialTicks);
            r.renderDynamic(ccrs, t2);
            if (mStack != null && buffers != null) {
                r.renderCustomDynamic(ccrs, t2, mStack, buffers, packedLight, packedOverlay, partialTicks);
            }
        }
    }
    //endregion

    //region Inventory rendering
    public void renderInventory(CCRenderState ccrs, @Nullable ItemStack stack, int renderIndex, int orient, Transformation t) {
        renderInventory(ccrs, stack, renderIndex, orient, t, null, null, 0, 0);
    }

    public void renderInventory(CCRenderState ccrs, @Nullable ItemStack stack, int renderIndex, int orient, Transformation t, @Nullable PoseStack mStack, @Nullable MultiBufferSource buffers, int packedLight, int packedOverlay) {
        GateRenderer r = getRenderer(renderIndex);
        r.prepareInventory(stack);
        r.renderStatic(ccrs, orient, t);
        if (r.hasSpecials()) {
            r.renderDynamic(ccrs, t);
            if (mStack != null && buffers != null) {
                r.renderCustomDynamic(ccrs, t, mStack, buffers, packedLight, packedOverlay, 0);
            }
        }
    }
    //endregion

    public void spawnParticles(GatePart part, RandomSource random) {
        GateRenderer r = getRenderer(part.getGateType().ordinal());
        r.prepare(part);
        r.spawnParticles(part, random);
    }

    public static int getRenderIndex(GateType type) {
        return type.ordinal();
    }

    public static int getNonPartRenderIndex(int i) {
        return 0x100 | i;
    }

    private GateRenderer getRenderer(int renderIndex) {
        if ((renderIndex & 0x100) != 0) {
            return nonPartRenderers[renderIndex & 0xFF];
        } else {
            return renderers[renderIndex];
        }
    }

    public static void registerIcons(AtlasRegistrar registrar) {
        GateComponentModels.registerIcons(registrar);
        // TODO find a way around calling into all renderers, having them call into all component models
        //      (it was only used for 2d wires)
    }

    public static void onResourceManagerReload(ResourceManager resourceManager) {
        WireModel3D.regenerateModels();
    }

    public static abstract class GateRenderer {

        public boolean reflect = false;

        protected abstract List<ComponentModel> getModels();

        protected abstract void prepareInventory(@Nullable ItemStack stack);

        protected abstract void prepare(IGateRenderData gate);

        protected void prepareDynamic(IGateRenderData gate, float partialFrame) {
        }

        protected void renderModels(CCRenderState ccrs, int orient, Transformation t) {
            for (ComponentModel m : getModels()) {
                m.renderModel(t, orient, ccrs);
            }
        }

        public void renderStatic(CCRenderState ccrs, int orient, Transformation t) {
            renderModels(ccrs, reflect ? orient + 24 : orient, t);
        }

        public void spawnParticles(GatePart gate, RandomSource random) {
            List<IRedstoneTorchComponentModel> torches = new LinkedList<>();

            for (ComponentModel m : getModels()) {
                if (m instanceof IRedstoneTorchComponentModel && ((IRedstoneTorchComponentModel) m).isLit()) {
                    torches.add((IRedstoneTorchComponentModel) m);
                }
            }

            for (IRedstoneTorchComponentModel torch : torches) {
                if (random.nextInt(torches.size()) == 0) {
                    Vector3 pos = torch.getLightPos().copy().add((random.nextDouble() - 0.5D) * 0.2D, 0, (random.nextDouble() - 0.5D) * 0.2D);
                    pos.apply(gate.rotationT()).add(gate.pos());

                    float f = 1.0F; // RS Strength 0-1
                    float f1 = f * 0.6F + 0.4F;
                    float f2 = Math.max(0.0F, f * f * 0.7F - 0.5F);
                    float f3 = Math.max(0.0F, f * f * 0.6F - 0.7F);

                    gate.level().addParticle(new DustParticleOptions(new Vector3f(f1, f2, f3), 1.0F), pos.x, pos.y, pos.z, 0, 0, 0);
                }
            }
        }

        public boolean hasSpecials() {
            return false;
        }

        public void renderDynamic(CCRenderState ccrs, Transformation t) {
        }

        public void renderCustomDynamic(CCRenderState ccrs, Transformation t, PoseStack mStack, MultiBufferSource buffers, int packedLight, int packedOverlay, float partialTicks) {
        }
    }

    public static class RenderOR extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("or", 4);
        private final RedstoneTorchModel[] torches = { new RedstoneTorchModel(8, 9, 6), new RedstoneTorchModel(8, 2.5, 8) };

        public RenderOR() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
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
        protected void prepare(IGateRenderData gate) {
            wires[0].on = (gate.state() & 0x10) == 0;
            wires[1].on = (gate.state() & 2) != 0;
            wires[2].on = (gate.state() & 4) != 0;
            wires[3].on = (gate.state() & 8) != 0;
            wires[1].disabled = (gate.shape() & 1) != 0;
            wires[2].disabled = (gate.shape() & 2) != 0;
            wires[3].disabled = (gate.shape() & 4) != 0;
            torches[0].on = (gate.state() & 0xE) == 0;
            torches[1].on = !wires[0].on;
        }
    }

    public static class RenderNOR extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("nor", 4);
        private final RedstoneTorchModel torch = new RedstoneTorchModel(8, 9, 6);

        public RenderNOR() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.add(torch);
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
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
        protected void prepare(IGateRenderData gate) {
            wires[0].on = (gate.state() & 0x11) != 0;
            wires[1].on = (gate.state() & 2) != 0;
            wires[2].on = (gate.state() & 4) != 0;
            wires[3].on = (gate.state() & 8) != 0;
            wires[1].disabled = (gate.shape() & 1) != 0;
            wires[2].disabled = (gate.shape() & 2) != 0;
            wires[3].disabled = (gate.shape() & 4) != 0;
            torch.on = (gate.state() & 0xE) == 0;
        }
    }

    public static class RenderNOT extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("not", 4);
        private final RedstoneTorchModel torch = new RedstoneTorchModel(8, 8, 6);

        public RenderNOT() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.add(torch);
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            wires[0].on = true;
            wires[1].on = true;
            wires[2].on = false;
            wires[3].on = true;
            wires[0].disabled = false;
            wires[1].disabled = false;
            wires[3].disabled = false;
            torch.on = true;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            wires[0].on = (gate.state() & 0x11) != 0;
            wires[1].on = (gate.state() & 0x22) != 0;
            wires[2].on = (gate.state() & 4) != 0;
            wires[3].on = (gate.state() & 0x88) != 0;
            wires[0].disabled = (gate.shape() & 2) != 0;
            wires[1].disabled = (gate.shape() & 1) != 0;
            wires[3].disabled = (gate.shape() & 4) != 0;
            torch.on = (gate.state() & 0xF0) != 0;
        }
    }

    public static class RenderAND extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("and", 4);
        private final RedstoneTorchModel[] torches = {
                new RedstoneTorchModel(4, 8, 6), new RedstoneTorchModel(12, 8, 6),
                new RedstoneTorchModel(8, 8, 6), new RedstoneTorchModel(8, 2, 8) };

        public RenderAND() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
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
        protected void prepare(IGateRenderData gate) {
            wires[0].on = (gate.state() & 0x11) == 0;
            wires[3].on = (gate.state() & 2) != 0;
            wires[1].on = (gate.state() & 4) != 0;
            wires[2].on = (gate.state() & 8) != 0;
            wires[3].disabled = (gate.shape() & 1) != 0;
            wires[1].disabled = (gate.shape() & 2) != 0;
            wires[2].disabled = (gate.shape() & 4) != 0;
            torches[2].on = !wires[1].on && !wires[1].disabled;
            torches[0].on = !wires[2].on && !wires[2].disabled;
            torches[1].on = !wires[3].on && !wires[3].disabled;
            torches[3].on = !wires[0].on;
        }
    }

    public static class RenderNAND extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("nand", 4);
        private final RedstoneTorchModel[] torches = {
                new RedstoneTorchModel(4, 8, 6), new RedstoneTorchModel(12, 8, 6),
                new RedstoneTorchModel(8, 8, 6) };

        public RenderNAND() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
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
        protected void prepare(IGateRenderData gate) {
            wires[0].on = (gate.state() & 0x11) != 0;
            wires[3].on = (gate.state() & 2) != 0;
            wires[1].on = (gate.state() & 4) != 0;
            wires[2].on = (gate.state() & 8) != 0;
            wires[3].disabled = (gate.shape() & 1) != 0;
            wires[1].disabled = (gate.shape() & 2) != 0;
            wires[2].disabled = (gate.shape() & 4) != 0;
            torches[0].on = !wires[2].on && !wires[2].disabled;
            torches[1].on = !wires[3].on && !wires[3].disabled;
            torches[2].on = !wires[1].on && !wires[1].disabled;
        }
    }

    public static class RenderXOR extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("xor", 4);
        private final RedstoneTorchModel[] torches = {
                new RedstoneTorchModel(4.5, 8, 6), new RedstoneTorchModel(11.5, 8, 6),
                new RedstoneTorchModel(8, 12, 6) };

        public RenderXOR() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            wires[0].on = false;
            wires[3].on = false;
            wires[2].on = false;
            wires[1].on = true;
            torches[0].on = false;
            torches[1].on = false;
            torches[2].on = true;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            wires[0].on = (gate.state() & 0x11) != 0;
            wires[3].on = (gate.state() & 2) != 0;
            wires[2].on = (gate.state() & 8) != 0;
            wires[1].on = !wires[3].on && !wires[2].on;
            torches[0].on = !wires[2].on && !wires[1].on;
            torches[1].on = !wires[3].on && !wires[1].on;
            torches[2].on = wires[1].on;
        }
    }

    public static class RenderXNOR extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("xnor", 5);
        private final RedstoneTorchModel[] torches = {
                new RedstoneTorchModel(8, 2, 8), new RedstoneTorchModel(4.5, 8, 6),
                new RedstoneTorchModel(11.5, 8, 6), new RedstoneTorchModel(8, 12, 6) };

        public RenderXNOR() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
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
        protected void prepare(IGateRenderData gate) {
            wires[0].on = (gate.state() & 2) != 0 && (gate.state() & 8) == 0;
            wires[1].on = (gate.state() & 8) != 0 && (gate.state() & 2) == 0;
            wires[2].on = (gate.state() & 8) != 0;
            wires[3].on = (gate.state() & 2) != 0;
            wires[4].on = !wires[3].on && !wires[2].on;
            torches[0].on = (gate.state() & 0x11) != 0;
            torches[1].on = !wires[4].on && (gate.state() & 8) == 0;
            torches[2].on = !wires[4].on && (gate.state() & 2) == 0;
            torches[3].on = (gate.state() & 2) == 0 && (gate.state() & 8) == 0;
        }
    }

    public static class RenderBuffer extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("buffer", 4);
        private final RedstoneTorchModel[] torches = {
                new RedstoneTorchModel(8, 3.5, 8), new RedstoneTorchModel(8, 9, 6) };

        public RenderBuffer() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            wires[0].on = true;
            wires[1].on = false;
            wires[2].on = false;
            wires[3].on = false;
            wires[1].disabled = false;
            wires[3].disabled = false;
            torches[0].on = false;
            torches[1].on = true;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            wires[0].on = (gate.state() & 4) == 0;
            wires[1].on = (gate.state() & 0x22) != 0;
            wires[2].on = (gate.state() & 0x44) != 0;
            wires[3].on = (gate.state() & 0x88) != 0;
            wires[1].disabled = (gate.shape() & 1) != 0;
            wires[3].disabled = (gate.shape() & 2) != 0;
            torches[0].on = (gate.state() & 4) != 0;
            torches[1].on = (gate.state() & 4) == 0;
        }
    }

    public static class RenderMultiplexer extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("multiplexer", 6);
        private final RedstoneTorchModel[] torches = {
                new RedstoneTorchModel(8, 2, 8), new RedstoneTorchModel(9, 10.5, 6),
                new RedstoneTorchModel(4.5, 8, 6), new RedstoneTorchModel(11.5, 8, 6) };

        public RenderMultiplexer() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
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
        protected void prepare(IGateRenderData gate) {
            wires[2].on = (gate.state() & 4) == 0;
            wires[3].on = (gate.state() & 4) != 0;
            wires[4].on = (gate.state() & 8) != 0;
            wires[5].on = (gate.state() & 2) != 0;
            torches[0].on = (gate.state() & 0x10) != 0;
            torches[1].on = !wires[3].on;
            torches[2].on = (gate.state() & 8) == 0 && wires[3].on;
            torches[3].on = (gate.state() & 4) == 0 && !wires[5].on;
            wires[0].on = torches[2].on;
            wires[1].on = torches[3].on;
        }
    }

    public static class RenderPulse extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("pulse", 3);
        private final RedstoneTorchModel[] torches = {
                new RedstoneTorchModel(4, 9.5, 6), new RedstoneTorchModel(11, 9.5, 6),
                new RedstoneTorchModel(8, 3.5, 8) };

        public RenderPulse() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            wires[0].on = true;
            wires[1].on = false;
            wires[2].on = false;
            torches[0].on = true;
            torches[1].on = false;
            torches[2].on = false;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            wires[0].on = (gate.state() & 4) == 0;
            wires[1].on = (gate.state() & 4) != 0;
            wires[2].on = (gate.state() & 0x14) == 4;
            torches[0].on = wires[0].on;
            torches[1].on = wires[1].on;
            torches[2].on = (gate.state() & 0x10) != 0;
        }
    }

    public static class RenderRepeater extends GateRenderer {

        private final ArrayList<List<ComponentModel>> models;

        private final WireModel[] wires = generateWireModels("repeater", 2);
        private final RedstoneTorchModel endTorch = new RedstoneTorchModel(8, 2, 6);
        private final RedstoneTorchModel[] delayTorches = {
                new RedstoneTorchModel(12.5, 12, 6), new RedstoneTorchModel(12.5, 11, 6),
                new RedstoneTorchModel(12.5, 10, 6), new RedstoneTorchModel(12.5, 9, 6),
                new RedstoneTorchModel(12.5, 8, 6), new RedstoneTorchModel(12.5, 7, 6),
                new RedstoneTorchModel(12.5, 6, 6), new RedstoneTorchModel(12.5, 5, 6),
                new RedstoneTorchModel(12.5, 4, 6) };

        private int shape = 0;

        public RenderRepeater() {
            models = new ArrayList<>(delayTorches.length);
            for (int i = 0; i < delayTorches.length; i++) {
                List<ComponentModel> list = new LinkedList<>();
                list.add(BaseComponentModel.INSTANCE);
                list.addAll(Arrays.asList(wires));
                list.add(endTorch);
                list.add(delayTorches[i]);
                models.add(i, list);
            }
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models.get(shape);
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            wires[0].on = true;
            wires[1].on = false;
            endTorch.on = false;
            shape = 0;
            delayTorches[0].on = true;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            wires[0].on = (gate.state() & 0x10) == 0;
            wires[1].on = (gate.state() & 4) != 0;
            endTorch.on = (gate.state() & 0x10) != 0;
            shape = gate.shape();
            delayTorches[shape].on = (gate.state() & 4) == 0;
        }
    }

    public static class RenderRandomizer extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("rand", 7);
        private final OnOffModel[] chips = {
                new YellowChipModel(8, 5.5), new YellowChipModel(11.5, 11.5), new YellowChipModel(4.5, 11.5) };

        public RenderRandomizer() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(chips));
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            wires[0].on = false;
            wires[1].on = false;
            wires[2].on = false;
            wires[3].on = false;
            wires[4].on = false;
            wires[5].on = false;
            wires[6].on = false;
            wires[0].disabled = false;
            wires[1].disabled = false;
            wires[3].disabled = false;
            wires[4].disabled = false;
            wires[5].disabled = false;
            wires[6].disabled = false;
            chips[0].on = false;
            chips[1].on = false;
            chips[2].on = false;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            wires[2].on = (gate.state() & 4) != 0;
            wires[0].on = (gate.state() & 0x11) != 0;
            wires[1].on = (gate.state() & 0x22) != 0;
            wires[3].on = (gate.state() & 0x88) != 0;
            wires[4].on = wires[2].on;
            wires[5].on = wires[2].on;
            wires[6].on = wires[2].on;
            wires[1].disabled = (gate.shape() & 1) != 0;
            wires[0].disabled = (gate.shape() & 2) != 0;
            wires[3].disabled = (gate.shape() & 4) != 0;
            wires[5].disabled = wires[1].disabled;
            wires[4].disabled = wires[0].disabled;
            wires[6].disabled = wires[3].disabled;
            chips[0].on = (gate.state() & 0x10) != 0;
            chips[1].on = (gate.state() & 0x20) != 0;
            chips[2].on = (gate.state() & 0x80) != 0;
        }
    }

    public static class RenderSRLatch extends GateRenderer {

        private final ArrayList<List<ComponentModel>> models = new ArrayList<>(2);

        private final WireModel[] wires1 = generateWireModels("rslatch", 2);
        private final WireModel[] wires2 = generateWireModels("rslatch2", 4);
        private final RedstoneTorchModel[] torches1 = new RedstoneTorchModel[] {
                new RedstoneTorchModel(8, 3, 6), new RedstoneTorchModel(8, 13, 6) };
        private final RedstoneTorchModel[] torches2 = new RedstoneTorchModel[] {
                new RedstoneTorchModel(9.5, 3, 6), new RedstoneTorchModel(6.5, 13, 6) };

        private int shape = 0;

        public RenderSRLatch() {
            List<ComponentModel> shape0 = new LinkedList<>();
            shape0.add(BaseComponentModel.INSTANCE);
            shape0.addAll(Arrays.asList(wires1));
            shape0.addAll(Arrays.asList(torches1));

            List<ComponentModel> shape1 = new LinkedList<>();
            shape1.add(BaseComponentModel.INSTANCE);
            shape1.addAll(Arrays.asList(wires2));
            shape1.addAll(Arrays.asList(torches2));

            models.add(shape0);
            models.add(shape1);
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models.get(shape);
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            reflect = false;
            shape = 0;
            wires1[0].on = false;
            wires1[1].on = true;
            torches1[0].on = false;
            torches1[1].on = true;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            reflect = (gate.shape() & 1) != 0;
            shape = gate.shape() >> 1;
            int state = gate.state();
            if (reflect) state = flipMaskZ(state >> 4) << 4 | flipMaskZ(state);
            if (shape == 0) {
                wires1[0].on = (state & 0x88) != 0;
                wires1[1].on = (state & 0x22) != 0;
                torches1[0].on = (state & 0x10) != 0;
                torches1[1].on = (state & 0x40) != 0;
            } else {
                wires2[1].on = (state & 2) != 0;
                wires2[3].on = (state & 8) != 0;
                torches2[0].on = (state & 0x10) != 0;
                torches2[1].on = (state & 0x40) != 0;
                wires2[0].on = torches2[1].on;
                wires2[2].on = torches2[0].on;
            }
        }
    }

    public static class RenderToggleLatch extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("toglatch", 2);
        private final RedstoneTorchModel[] torches = {
                new RedstoneTorchModel(4, 4, 6), new RedstoneTorchModel(4, 12, 6) };
        private final LeverModel lever = new LeverModel(11, 8);

        public RenderToggleLatch() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
            models.add(lever);
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            wires[0].on = false;
            wires[1].on = false;
            torches[0].on = true;
            torches[1].on = false;
            lever.state = 0;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            wires[0].on = (gate.state() & 8) != 0;
            wires[1].on = (gate.state() & 2) != 0;
            torches[0].on = (gate.state() & 0x10) != 0;
            torches[1].on = (gate.state() & 0x40) != 0;
            lever.state = (gate.state() & 0x10) != 0 ? 0 : 1;
        }
    }

    public static class RenderTransparentLatch extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("translatch", 5);
        private final RedstoneTorchModel[] torches = {
                new RedstoneTorchModel(4, 12.5, 6), new RedstoneTorchModel(4, 8, 6),
                new RedstoneTorchModel(8, 8, 6), new RedstoneTorchModel(8, 2, 8), new RedstoneTorchModel(14, 8, 8) };

        public RenderTransparentLatch() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
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
        protected void prepare(IGateRenderData gate) {
            reflect = gate.shape() == 1;
            boolean on = (gate.state() & 0x10) != 0;
            wires[0].on = !on;
            wires[1].on = (gate.state() & 4) != 0;
            wires[2].on = (gate.state() & 4) == 0;
            wires[3].on = on;
            wires[4].on = (gate.state() & 0xA) != 0;
            torches[0].on = wires[2].on;
            torches[1].on = !wires[2].on && !wires[4].on;
            torches[2].on = !wires[1].on && !wires[3].on;
            torches[3].on = on;
            torches[4].on = on;
        }
    }

    public static class RenderLightSensor extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("lightsensor", 1);
        private final SolarModel solar = new SolarModel(8, 5.5);

        public RenderLightSensor() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.add(solar);
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            wires[0].on = false;
            solar.state = 0;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            wires[0].on = (gate.state() & 0xF4) != 0;
            solar.state = gate.shape();
        }
    }

    public static class RenderRainSensor extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("rainsensor", 1);
        private final RainSensorModel sensor = new RainSensorModel(8, 6);

        public RenderRainSensor() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.add(sensor);
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            wires[0].on = false;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            wires[0].on = (gate.state() & 0x44) != 0;
        }
    }

    public static class RenderTimer extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("time", 3);
        private final RedstoneTorchModel[] torches = {
                new RedstoneTorchModel(8, 3, 6), new RedstoneTorchModel(8, 8, 12) };
        private final PointerModel pointer = new PointerModel(8, 8, 8);

        public RenderTimer() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            wires[0].on = false;
            wires[1].on = false;
            wires[2].on = false;
            torches[0].on = false;
            pointer.angle = 0;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            torches[0].on = (gate.state() & 0x10) != 0;
            wires[0].on = (gate.state() & 0x88) != 0;
            wires[1].on = (gate.state() & 0x22) != 0;
            wires[2].on = (gate.state() & 4) != 0;
        }

        @Override
        public boolean hasSpecials() {
            return true;
        }

        @Override
        protected void prepareDynamic(IGateRenderData gate, float partialFrame) {
            float interpPointer = !gate.isPointerStarted() ? 0f : (gate.pointerValue() + partialFrame) / gate.pointerMax();
            pointer.angle = interpPointer * MathHelper.pi * 2;
        }

        @Override
        public void renderDynamic(CCRenderState ccrs, Transformation t) {
            pointer.renderModel(t, 0, ccrs);
        }
    }

    public static class RenderSequencer extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final RedstoneTorchModel[] torches = {
                new RedstoneTorchModel(8, 8, 12), new RedstoneTorchModel(8, 3, 6),
                new RedstoneTorchModel(13, 8, 6), new RedstoneTorchModel(8, 13, 6), new RedstoneTorchModel(3, 8, 6) };
        private final PointerModel pointer = new PointerModel(8, 8, 8);

        public RenderSequencer() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(torches));
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            torches[0].on = true;
            torches[1].on = true;
            torches[2].on = false;
            torches[3].on = false;
            torches[4].on = false;
            pointer.angle = 0;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            torches[0].on = true;
            torches[1].on = (gate.state() & 0x10) != 0;
            torches[2].on = (gate.state() & 0x20) != 0;
            torches[3].on = (gate.state() & 0x40) != 0;
            torches[4].on = (gate.state() & 0x80) != 0;
        }

        @Override
        public boolean hasSpecials() {
            return true;
        }

        @Override
        protected void prepareDynamic(IGateRenderData gate, float partialFrame) {
            int max = gate.pointerMax() * 4;
            float interpPointer = (gate.pointerValue() + partialFrame) / max;
            pointer.angle = interpPointer * MathHelper.pi * 2;
            if (gate.shape() == 1) pointer.angle *= -1;
        }

        @Override
        public void renderDynamic(CCRenderState ccrs, Transformation t) {
            pointer.renderModel(t, 0, ccrs);
        }
    }

    public static class RenderCounter extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("count", 2);
        private final RedstoneTorchModel[] torches = {
                new RedstoneTorchModel(11, 8, 12), new RedstoneTorchModel(8, 3, 6),
                new RedstoneTorchModel(8, 13, 6) };
        private final PointerModel pointer = new PointerModel(11, 8, 8, 1.2D);

        public RenderCounter() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            reflect = false;
            wires[0].on = false;
            wires[1].on = false;
            torches[1].on = false;
            torches[2].on = true;
            pointer.angle = 220 * MathHelper.torad;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            reflect = gate.shape() == 1;
            wires[0].on = (gate.state() & 8) != 0;
            wires[1].on = (gate.state() & 2) != 0;
            torches[1].on = (gate.state() & 0x10) != 0;
            torches[2].on = (gate.state() & 0x40) != 0;
        }

        @Override
        public boolean hasSpecials() {
            return true;
        }

        @Override
        protected void prepareDynamic(IGateRenderData gate, float partialFrame) {
            double interpPointer = ((double) gate.pointerValue() / gate.pointerMax()) * (340 - 220) + 210;
            pointer.angle = interpPointer * MathHelper.torad;
            reflect = gate.shape() == 1;
        }

        @Override
        public void renderDynamic(CCRenderState ccrs, Transformation t) {
            pointer.renderModel(t, reflect ? 1 : 0, ccrs);
        }
    }

    public static class RenderStateCell extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("statecell", 5);
        private final RedstoneTorchModel[] torches = {
                new RedstoneTorchModel(10, 3.5, 6), new RedstoneTorchModel(13, 8, 12) };
        private final OnOffModel chip = new RedChipModel(6.5, 10);
        private final PointerModel pointer = new PointerModel(13, 8, 8);

        public RenderStateCell() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
            models.add(chip);
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            reflect = false;
            wires[0].on = false;
            wires[1].on = false;
            wires[2].on = false;
            wires[3].on = false;
            wires[4].on = false;
            torches[0].on = false;
            torches[1].on = true;
            chip.on = false;
            pointer.angle = -MathHelper.pi / 2;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            reflect = gate.shape() == 1;
            int state = gate.state();
            if (reflect) state = flipMaskZ(state >> 4) << 4 | flipMaskZ(state);

            wires[0].on = (state & 0x10) != 0;
            wires[1].on = (state & 4) != 0;
            wires[2].on = gate.state2() == 0 || (state & 4) != 0;
            wires[3].on = (state & 0x88) != 0;
            wires[4].on = (state & 2) != 0;
            torches[0].on = (state & 0x10) != 0;
            torches[1].on = gate.isPointerStarted();
            chip.on = gate.state2() != 0;
        }

        @Override
        public boolean hasSpecials() {
            return true;
        }

        @Override
        protected void prepareDynamic(IGateRenderData gate, float partialFrame) {
            reflect = gate.shape() == 1;
            double interpPointer = !gate.isPointerStarted() ? 0f : (gate.pointerValue() + partialFrame) / gate.pointerMax();
            pointer.angle = interpPointer - MathHelper.pi / 2;
        }

        @Override
        public void renderDynamic(CCRenderState ccrs, Transformation t) {
            pointer.renderModel(t, reflect ? 1 : 0, ccrs);
        }
    }

    public static class RenderSynchronizer extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("sync", 6);
        private final RedstoneTorchModel torch = new RedstoneTorchModel(8, 3, 6);
        private final OnOffModel[] chips = {
                new RedChipModel(4.5, 9), new RedChipModel(11.5, 9) };

        public RenderSynchronizer() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.add(torch);
            models.addAll(Arrays.asList(chips));
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
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
        protected void prepare(IGateRenderData gate) {
            boolean right = (gate.state2() & 1) != 0;
            boolean left = (gate.state2() & 2) != 0;

            wires[0].on = !left;
            wires[1].on = !right;
            wires[2].on = (gate.state() & 4) != 0;
            wires[3].on = left && right;
            wires[4].on = (gate.state() & 8) != 0;
            wires[5].on = (gate.state() & 2) != 0;
            chips[0].on = left;
            chips[1].on = right;
            torch.on = (gate.state() & 0x10) != 0;
        }
    }

    public static class RenderBusXcvr extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("busxcvr", 2);
        private final SignalPanelModel[] panels = { new SignalPanelModel(4, 8, 0), new SignalPanelModel(12, 8, 2) };

        public RenderBusXcvr() {
            models.add(BaseComponentModel.INSTANCE);
            models.add(BusXcvrCableModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(panels));
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            reflect = false;
            wires[0].on = false;
            wires[1].on = false;
            panels[0].signal = 0;
            panels[1].signal = 0;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            reflect = gate.shape() != 0;
            int state = gate.state();
            if (reflect) state = flipMaskZ(state);

            wires[0].on = (state & 2) != 0;
            wires[1].on = (state & 8) != 0;
            panels[0].signal = gate.bOutput2();
            panels[1].signal = gate.bOutput0();
        }
    }

    public static abstract class RenderArrayCell extends GateRenderer {

        protected final List<ComponentModel> models = new LinkedList<>();

        protected final CellTopWireModel topWire;
        protected final CellBottomWireModel bottomWire;

        public RenderArrayCell(CellTopWireModel topWire, CellBottomWireModel bottomWire) {
            this.topWire = topWire;
            this.bottomWire = bottomWire;

            models.add(topWire);
            models.add(bottomWire);
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            bottomWire.signal = 0;
            topWire.signal = 0;
            topWire.conn = 0;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            bottomWire.signal = gate.bottomSignal();
            topWire.signal = gate.topSignal();
            topWire.conn = gate.topSignalConnMask();
        }
    }

    public static class RenderNullCell extends RenderArrayCell {

        public RenderNullCell() {
            super(new NullCellTopWireModel(), new NullCellBottomWireModel());
            models.add(NullCellBaseModel.INSTANCE);
        }
    }

    public static class RenderInvertCell extends RenderArrayCell {

        private final WireModel[] wires = generateWireModels("invcell", 1);
        private final RedstoneTorchModel torch = new RedstoneTorchModel(8, 8, 6);

        public RenderInvertCell() {
            super(new LogicCellTopWireModel(), new LogicCellBottomWireModel());
            models.add(LogicCellBaseModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.add(torch);
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            super.prepareInventory(stack);
            topWire.signal = (byte) 255;
            wires[0].on = false;
            torch.on = true;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            super.prepare(gate);
            torch.on = gate.bottomSignal() == 0;
            wires[0].on = gate.bottomSignal() != 0;
        }
    }

    public static class RenderBufferCell extends RenderArrayCell {

        private final WireModel[] wires = generateWireModels("buffcell", 2);
        private final RedstoneTorchModel[] torches = new RedstoneTorchModel[] { new RedstoneTorchModel(11, 13, 6), new RedstoneTorchModel(8, 8, 6) };

        public RenderBufferCell() {
            super(new LogicCellTopWireModel(), new LogicCellBottomWireModel());
            models.add(LogicCellBaseModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            super.prepareInventory(stack);
            wires[0].on = false;
            wires[1].on = true;
            torches[0].on = true;
            torches[1].on = false;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            super.prepare(gate);
            torches[0].on = gate.bottomSignal() == 0;
            torches[1].on = gate.bottomSignal() != 0;
            wires[0].on = gate.bottomSignal() != 0;
            wires[1].on = gate.bottomSignal() == 0;
        }
    }

    public static class RenderANDCell extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final CellTopWireModel topWire = new AndCellTopWireModel();
        private final WireModel[] wires = generateWireModels("andcell", 2);
        private final OnOffModel[] torches = new OnOffModel[] { new RedstoneTorchModel(8, 13, 6), new RedstoneTorchModel(8, 2, 8), new FlippedRedstoneTorchModel(8, 10, 8, 5) };

        public RenderANDCell() {
            models.add(AndCellBaseModel.INSTANCE);
            models.add(topWire);
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            topWire.signal = 0;
            topWire.conn = 0;
            torches[0].on = true;
            torches[1].on = false;
            torches[2].on = true;
            wires[0].on = true;
            wires[1].on = false;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            topWire.signal = gate.topSignal();
            topWire.conn = gate.topSignalConnMask();
            torches[0].on = (gate.state() & 4) == 0;
            torches[1].on = (gate.state() & 0x10) != 0;
            torches[2].on = gate.topSignal() == 0;
            wires[0].on = torches[0].on || torches[2].on;
            wires[1].on = !torches[0].on;
        }
    }

    public static class RenderTransparentLatchCell extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final CellTopWireModel topWire = new TransparentLatchCellTopWireModel();
        private final WireModel[] wires = generateWireModels("transparent-latch-cell", 5);
        private final OnOffModel[] torches = new OnOffModel[] { new RedstoneTorchModel(12.5, 12, 6), new RedstoneTorchModel(8, 12, 6),
                new RedstoneTorchModel(8, 8, 6), new RedstoneTorchModel(8, 2, 8) };

        public RenderTransparentLatchCell() {
            models.add(TransparentLatchCellBaseModel.INSTANCE);
            models.add(topWire);
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            topWire.signal = 0;
            topWire.conn = 0;
            wires[0].on = true;
            wires[1].on = false;
            wires[2].on = true;
            wires[3].on = false;
            wires[4].on = false;
            torches[0].on = true;
            torches[1].on = false;
            torches[2].on = true;
            torches[3].on = false;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            boolean on = (gate.state() & 0x10) != 0;
            topWire.signal = gate.topSignal();
            topWire.conn = gate.topSignalConnMask() & ~0x2; // Always render left side
            wires[0].on = !on;
            wires[1].on = gate.topSignal() != 0;
            wires[2].on = gate.topSignal() == 0;
            wires[3].on = on;
            wires[4].on = (gate.state() & 4) != 0;
            torches[0].on = wires[2].on;
            torches[1].on = !wires[2].on && !wires[4].on;
            torches[2].on = !wires[1].on && !wires[3].on;
            torches[3].on = on;
        }
    }

    public static class RenderComparator extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("comparator", 4);
        private final RedstoneTorchModel torch = new RedstoneTorchModel(8, 2, 6);
        private final OnOffModel[] chips = new OnOffModel[] { new MinusChipModel(5, 8), new PlusChipModel(11, 8) };

        public RenderComparator() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.add(torch);
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            reflect = false;
            wires[0].on = true;
            wires[1].on = false;
            wires[2].on = false;
            wires[3].on = false;
            chips[0].on = false;
            chips[1].on = false;
            torch.on = false;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            reflect = gate.shape() != 0;
            wires[0].on = (gate.state() & 0x10) == 0;
            wires[1].on = (gate.state() & 2) != 0;
            wires[2].on = (gate.state() & 4) != 0;
            wires[3].on = (gate.state() & 8) != 0;
            chips[0].on = (gate.state() & 1) != 0 && gate.shape() == 1;
            chips[1].on = (gate.state() & 1) != 0 && gate.shape() != 1;
            torch.on = (gate.state() & 0x10) != 0;
            if (gate.shape() != 0) {
                boolean a = wires[1].on;
                boolean b = wires[3].on;
                wires[3].on = a;
                wires[1].on = b;
            }
        }

        @Override
        protected void renderModels(CCRenderState ccrs, int orient, Transformation t) {
            super.renderModels(ccrs, orient, t);
            for (OnOffModel m : chips) {
                m.renderModel(t, orient % 24, ccrs); // Don't reflect the +/- chips
            }
        }
    }

    public static class RenderBusRandomizer extends GateRenderer {

        private final ArrayList<List<ComponentModel>> models = new ArrayList<>(2);

        private final WireModel[] wires1 = generateWireModels("busrand1", 2);
        private final WireModel[] wires2 = generateWireModels("busrand2", 2);
        private final SignalPanelModel panel = new SignalPanelModel(8, 8, 0);

        private int shape = 0;

        public RenderBusRandomizer() {
            List<ComponentModel> models0 = new LinkedList<>();
            models0.add(BaseComponentModel.INSTANCE);
            models0.add(BusRandCableModel.INSTANCE);
            models0.add(panel);
            models0.addAll(Arrays.asList(wires1));

            List<ComponentModel> models1 = new LinkedList<>();
            models1.add(BaseComponentModel.INSTANCE);
            models1.add(BusRandCableModel.INSTANCE);
            models1.add(panel);
            models1.addAll(Arrays.asList(wires2));

            models.add(models0);
            models.add(models1);

            panel.offColour = 0x756900FF;
            panel.onColour = 0xe1d600FF;
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models.get(shape);
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            shape = 0;
            panel.signal = 0;
            panel.disableMask = 0;

            wires1[0].on = false;
            wires2[0].on = false;
            wires1[1].on = false;
            wires2[1].on = false;
        }

        @Override
        protected void prepare(IGateRenderData part) {
            shape = part.shape();
            panel.signal = part.bOutput0();
            panel.disableMask = ~part.bInput2();

            wires1[0].on = (part.state() & 2) != 0;
            wires2[0].on = (part.state() & 2) != 0;
            wires1[1].on = (part.state() & 8) != 0;
            wires2[1].on = (part.state() & 8) != 0;
        }
    }

    public static class RenderBusConverter extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("busconv", 3);
        private final SignalBarModel bar = new SignalBarModel(8, 8);

        public RenderBusConverter() {
            models.add(BaseComponentModel.INSTANCE);
            models.add(BusConvCableModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.add(bar);
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            wires[0].on = false;
            wires[1].on = false;
            wires[2].on = false;
            bar.signal = 0;
            bar.inverted = false;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            wires[0].on = (gate.state() & 0x20) != 0;
            wires[1].on = (gate.state() & 0x80) != 0;
            wires[2].on = gate.rsIO() != 0;
            bar.inverted = gate.shape() != 0;
            bar.signal = gate.rsIO();
        }
    }

    public static class RenderBusInputPanel extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("businput", 1);
        private final InputPanelButtonsModel buttons = new InputPanelButtonsModel();

        private final BlockPos.MutableBlockPos lightPos = new BlockPos.MutableBlockPos();

        public RenderBusInputPanel() {
            models.add(BaseComponentModel.INSTANCE);
            models.add(BusInputPanelCableModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.add(buttons);
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            wires[0].on = false;
            buttons.pressMask = 0;
            lightPos.set(0, 0, 0);
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            wires[0].on = (gate.state() & 1) != 0;
            buttons.pressMask = gate.bInput0();
        }

        @Override
        public boolean hasSpecials() {
            return true;
        }

        @Override
        protected void prepareDynamic(IGateRenderData gate, float partialFrame) {
            buttons.pressMask = gate.bInput0();
            lightPos.set(gate.worldPos());
        }

        @Override
        public void renderCustomDynamic(CCRenderState ccrs, Transformation t, PoseStack mStack, MultiBufferSource buffers, int packedLight, int packedOverlay, float partialTicks) {
            buttons.renderLights(ccrs, lightPos, mStack, buffers, t);
        }
    }

    public static class RenderSegmentDisplay extends GateRenderer {

        private final ArrayList<List<ComponentModel>> models = new ArrayList<>(2);

        private final SevenSegmentDisplayModel sevenSeg1 = new SevenSegmentDisplayModel(4.5, 8);
        private final SevenSegmentDisplayModel sevenSeg0 = new SevenSegmentDisplayModel(11.5, 8);
        private final SixteenSegmentDisplayModel sixteenSeg = new SixteenSegmentDisplayModel(8, 8);

        private int shape = 0;

        public RenderSegmentDisplay() {
            List<ComponentModel> models0 = new LinkedList<>();
            models0.add(BaseComponentModel.INSTANCE);
            models0.add(SegmentDisplayBusCableModel.INSTANCE);
            models0.add(sevenSeg1);
            models0.add(sevenSeg0);

            List<ComponentModel> models1 = new LinkedList<>();
            models1.add(BaseComponentModel.INSTANCE);
            models1.add(SegmentDisplayBusCableModel.INSTANCE);
            models1.add(sixteenSeg);

            models.add(models0);
            models.add(models1);
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models.get(shape);
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            shape = 0;
            sevenSeg1.signal = 64;
            sevenSeg0.signal = 64;
            sixteenSeg.signal = 0;
            int c = EnumColour.RED.ordinal();
            sevenSeg0.setColourByIndex(c);
            sevenSeg1.setColourByIndex(c);
            sixteenSeg.setColourByIndex(c);
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            shape = gate.shape();
            sevenSeg1.signal = gate.bInput0() >> 8;
            sevenSeg0.signal = gate.bInput0() & 0xFF;
            sixteenSeg.signal = gate.bInput0();
            int c = gate.state();
            sevenSeg0.setColourByIndex(c);
            sevenSeg1.setColourByIndex(c);
            sixteenSeg.setColourByIndex(c);
        }
    }

    public static class RenderDecodingRandomizer extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("decrand", 6);
        private final RedstoneTorchModel[] torches = {
                new RedstoneTorchModel(8, 2.5, 8), new RedstoneTorchModel(14, 8, 8), new RedstoneTorchModel(2, 8, 8), new RedstoneTorchModel(9, 8, 6) };
        private final OnOffModel[] chips = {
                new YellowChipModel(5, 13), new YellowChipModel(11, 13), new RedChipModel(5.5, 8) };

        public RenderDecodingRandomizer() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.addAll(Arrays.asList(torches));
            models.addAll(Arrays.asList(chips));
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            wires[0].on = false;
            wires[1].on = false;
            wires[2].on = false;
            wires[3].on = false;
            wires[4].on = true;
            wires[5].on = true;
            wires[0].disabled = false;
            wires[3].disabled = false;
            torches[0].on = true;
            torches[1].on = false;
            torches[2].on = false;
            torches[3].on = false;
            chips[0].on = false;
            chips[1].on = true;
            chips[2].on = true;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            int state = gate.state();
            wires[0].on = (state >> 4) == 2;
            wires[1].on = (state >> 4) == 8;
            wires[2].on = (state & 4) != 0;
            wires[3].on = (state & 4) != 0;
            wires[4].on = (state >> 4) == 1 || (state >> 4) == 2;
            wires[5].on = (state >> 4) == 1;
            wires[0].disabled = gate.shape() != 0;
            wires[3].disabled = gate.shape() != 0;
            torches[0].on = (state >> 4) == 1;
            torches[1].on = (state >> 4) == 2;
            torches[2].on = (state >> 4) == 8;
            torches[3].on = !wires[4].on;
            chips[0].on = (state >> 4) == 2;
            chips[1].on = (state >> 4) == 1 || (state >> 4) == 2;
            chips[2].on = true;
        }
    }

    public static class RenderFabricatedGate extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final SidedWireModel simpleWires = new SidedWireModel(generateWireModels("ic1", 4));
        private final SidedWireModel analogWires = new SidedWireModel(generateWireModels("ic2", 4));
        private final SidedICBundledCableModel bundledWires = new SidedICBundledCableModel();
        private final FabricatedICModel icHousing = FabricatedICModel.INSTANCE;

        //**** Copied from EditorDataUtils.class ****/
        public static final String KEY_FORMAT = "format"; // int
        public static final String KEY_ACTIVE = "active"; // boolean
        public static final String KEY_IC_NAME = "ic_name"; // String
        public static final String KEY_TILE_MAP = "tile_map"; // CompoundTag
        public static final String KEY_IS_BUILT = "is_built"; // boolean
        public static final String KEY_IO_SPEC = "io_spec";
        public static final String KEY_COMP_STATE = "state"; // byte
        public static final String KEY_FLAT_MAP = "flat_map"; // String
        public static final String KEY_SIMULATION = "sim_cont"; // CompoundTag
        public static final String KEY_COMPILER_LOG = "compiler_log"; // CompoundTag

        // Minimum subset of data required to fabricate gate (i.e. create photomask)
        public static boolean hasFabricationTarget(@Nullable CompoundTag tag) {
            return tag != null &&
                    tag.contains(KEY_IS_BUILT) &&
                    tag.contains(KEY_FLAT_MAP);
        }

        private String name = "untitled";

        public RenderFabricatedGate() {
            models.add(BaseComponentModel.INSTANCE);
            models.add(simpleWires);
            models.add(analogWires);
            models.add(bundledWires);
            models.add(icHousing);
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            if (stack == null  || !hasFabricationTarget(stack.getTag())) {
                name = "ERROR!";
                simpleWires.sidemask = 0;
                analogWires.sidemask = 0;
                bundledWires.sidemask = 0;
                return;
            }

            //TODO use EditorDataUtils helpers once this class is moved to Fabrication

            CompoundTag tag = stack.getTag();
            name = tag.getString("ic_name");

            CompoundTag ifspecTag = tag.getCompound("io_spec");
            byte rMask = ifspecTag.getByte("rmask");
            byte aMask = 0; //TODO analog stuff
            byte bMask = ifspecTag.getByte("bmask");

            simpleWires.sidemask = rMask & 0xF | (rMask >> 4) & 0xF;
            analogWires.sidemask = aMask;
            bundledWires.sidemask = bMask & 0xF | (bMask >> 4) & 0xF;
        }

        @Override
        protected void prepare(IGateRenderData gate) {

            simpleWires.sidemask = gate.state2() & 0xF;
            analogWires.sidemask = (gate.state2() >> 4) & 0xF;
            bundledWires.sidemask = (gate.state2() >> 8) & 0xF;

            simpleWires.wires[0].on = (gate.state() & 0x11) != 0;
            simpleWires.wires[1].on = (gate.state() & 0x22) != 0;
            simpleWires.wires[2].on = (gate.state() & 0x44) != 0;
            simpleWires.wires[3].on = (gate.state() & 0x88) != 0;

            analogWires.wires[0].on = simpleWires.wires[0].on;
            analogWires.wires[1].on = simpleWires.wires[1].on;
            analogWires.wires[2].on = simpleWires.wires[2].on;
            analogWires.wires[3].on = simpleWires.wires[3].on;
        }

        @Override
        public boolean hasSpecials() {
            return true;
        }

        @Override
        protected void prepareDynamic(IGateRenderData gate, float partialFrame) {
            name = gate.getGateName();
        }

        @Override
        public void renderDynamic(CCRenderState ccrs, Transformation t) {
        }

        @Override
        public void renderCustomDynamic(CCRenderState ccrs, Transformation t, PoseStack mStack, MultiBufferSource buffers, int packedLight, int packedOverlay, float partialTicks) {

            // Render name
            icHousing.renderName(name, mStack, t);

            // Render glass
            ccrs.reset();
            ccrs.brightness = packedLight;
            ccrs.overlay = packedOverlay;
            ccrs.bind(new TransformingVertexConsumer(buffers.getBuffer(RenderType.translucentMovingBlock()), mStack), DefaultVertexFormat.BLOCK);
            icHousing.renderGlass(t, ccrs);
        }
    }

    public static class RenderIOGate extends GateRenderer {

        private final List<ComponentModel> models = new LinkedList<>();

        private final WireModel[] wires = generateWireModels("fabio", 1);
        private final IOCrimpWireModel crimpWire = new IOCrimpWireModel();
        private final IOCrimpColourBoxModel colourBox = new IOCrimpColourBoxModel(3, 10.5);

        public RenderIOGate() {
            models.add(BaseComponentModel.INSTANCE);
            models.addAll(Arrays.asList(wires));
            models.add(IOCrimpConnectorModel.INSTANCE);
            models.add(crimpWire);
            models.add(colourBox);
        }

        @Override
        protected List<ComponentModel> getModels() {
            return models;
        }

        @Override
        protected void prepareInventory(@Nullable ItemStack stack) {
            crimpWire.signal = 0;
            colourBox.colour = 0;
        }

        @Override
        protected void prepare(IGateRenderData gate) {
            wires[0].on = (gate.state() & 0x44) != 0;
            crimpWire.signal = (byte) (wires[0].on ? 255 : 0);
            colourBox.colour = gate.state2() & 0xF;
            colourBox.isInput = gate.shape() == 0;
        }
    }
}
