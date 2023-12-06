package mrtjp.projectred.core.client;

import codechicken.lib.render.BlockRenderer;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.*;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexFormat;
import mrtjp.projectred.api.MovingBlockEntityRenderCallback;
import mrtjp.projectred.api.ProjectRedAPI;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.init.CoreClientInit;
import net.minecraft.client.GraphicsStatus;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderStateShard;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.core.BlockPos;
import net.minecraft.server.packs.resources.ResourceManager;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.client.event.RenderLevelLastEvent;
import net.minecraftforge.client.event.RenderLevelStageEvent;
import org.jetbrains.annotations.Nullable;

import java.awt.*;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;
import static net.minecraft.client.renderer.RenderStateShard.*;

public class HaloRenderer {

    private static @Nullable HaloBloomPostChain HALO_POST_CHAIN;

    private static final RenderType HALO_GLOW_RENDER_TYPE = RenderType.create(MOD_ID + ":halo",
            DefaultVertexFormat.POSITION_COLOR, VertexFormat.Mode.QUADS, 2048, false, true,
            RenderType.CompositeState.builder()
                    .setTransparencyState(LIGHTNING_TRANSPARENCY)
                    .setShaderState(new RenderStateShard.ShaderStateShard(() -> CoreClientInit.HALO_SHADER))
                    .setCullState(CULL)
                    .setOutputState(PARTICLES_TARGET)
                    .setWriteMaskState(COLOR_WRITE)
                    .createCompositeState(false));

    private static final RenderType HALO_FABULOUS_DEPTH_RENDER_TYPE = RenderType.create(MOD_ID + ":halo_depth",
            DefaultVertexFormat.POSITION_COLOR, VertexFormat.Mode.QUADS, 2048, false, false,
            RenderType.CompositeState.builder()
                    .setTransparencyState(NO_TRANSPARENCY) // Depth only, doesn't matter
                    .setShaderState(new RenderStateShard.ShaderStateShard(() -> CoreClientInit.HALO_SHADER))
                    .setCullState(CULL)
                    .setOutputState(PARTICLES_TARGET)
                    .setWriteMaskState(DEPTH_WRITE)
                    .createCompositeState(false));

    private static final RenderType HALO_FABULOUS_BLOOM_RENDER_TYPE = RenderType.create(MOD_ID + ":halo_bloom",
            DefaultVertexFormat.POSITION_COLOR, VertexFormat.Mode.QUADS, 2048, false, true,
            RenderType.CompositeState.builder()
                    .setTransparencyState(NO_TRANSPARENCY)
                    .setShaderState(new RenderStateShard.ShaderStateShard(() -> CoreClientInit.HALO_SHADER))
                    .setCullState(CULL)
                    .setOutputState(new OutputStateShard("halo_post",
                            () -> HALO_POST_CHAIN.getInputTarget().bindWrite(false),
                            () -> Minecraft.getInstance().getMainRenderTarget().bindWrite(false)))
                    .setWriteMaskState(COLOR_WRITE) // No need for depth here. Plus this avoids z-fighting
                    .createCompositeState(false));

    private static final RenderType HALO_FABULOUS_ITEM_ENTITY_RENDER_TYPE = RenderType.create(MOD_ID + ":halo_item_entity",
            DefaultVertexFormat.POSITION_COLOR, VertexFormat.Mode.QUADS, 256, false, true,
            RenderType.CompositeState.builder()
                    .setTransparencyState(LIGHTNING_TRANSPARENCY)
                    .setShaderState(new RenderStateShard.ShaderStateShard(() -> CoreClientInit.HALO_SHADER))
                    .setCullState(CULL)
                    .setOutputState(MAIN_TARGET)
                    .setWriteMaskState(COLOR_DEPTH_WRITE)
                    .createCompositeState(false));

    private static boolean postChainFlushPending = false;

    private static final LinkedList<LevelLight> levelLights = new LinkedList<>();

    private static final Vector3 offset = Vector3.ZERO.copy();

    //region Init
    public static void init() {
        // Register callback for moving block entities
        if (ProjectRedAPI.expansionAPI != null) {
            ProjectRedAPI.expansionAPI.registerBlockEntityRenderCallback(new MovingBlockEntityRenderCallback() {
                @Override
                public void onMovingPreRender(double offsetX, double offsetY, double offsetZ) {
                    offset.set(offsetX, offsetY, offsetZ);
                }

                @Override
                public void onMovingPostRender() {
                    offset.set(0, 0, 0);
                }
            });
        }
    }
    //endregion

    //region World renderer
    public static void addLight(BlockPos pos, int colour, Cuboid6 box) {
        addLight(new Translation(pos), colour, box);
    }

    public static void addLight(Transformation t, int colour, Cuboid6 box) {
        Transformation t2 = new TransformationList(t, new Translation(offset));
        levelLights.add(new LevelLight(t2, colour, box));
        if (Configurator.lightHaloMax > -1 && levelLights.size() > Configurator.lightHaloMax) {
            levelLights.poll();
        }
    }

    public static void onRenderWorldStageEvent(final RenderLevelStageEvent event) {

        if (event.getStage() != RenderLevelStageEvent.Stage.AFTER_PARTICLES) {
            return;
        }

        if (levelLights.isEmpty()) {
            return;
        }

        if (!isFabulous()) {
            return;
        }

        // Build light list
        List<LevelLight> lightList = new LinkedList<>();
        LevelLight l;
        while ((l = levelLights.poll()) != null) {
            lightList.add(l);
        }

        // Setup post-chain
        preparePostChain();
        assert HALO_POST_CHAIN != null;
        HALO_POST_CHAIN.getInputTarget().clear(Minecraft.ON_OSX);
        HALO_POST_CHAIN.getInputTarget().copyDepthFrom(Minecraft.getInstance().getMainRenderTarget());

        Vec3 cam = event.getCamera().getPosition();
        PoseStack stack = event.getPoseStack();
        stack.pushPose();
        stack.translate(-cam.x, -cam.y, -cam.z);

        CCRenderState ccrs = CCRenderState.instance();
        ccrs.reset();
        MultiBufferSource.BufferSource buffers = Minecraft.getInstance().renderBuffers().bufferSource();

        // Render to normal render target for primary visuals
        ccrs.bind(HALO_GLOW_RENDER_TYPE, buffers, stack);
        for (LevelLight light : lightList) {
            renderToCCRS(ccrs, light.box, light.colour, light.t, HaloContext.LEVEL_RENDERER);
        }

        // Update depth buffer
        ccrs.bind(HALO_FABULOUS_DEPTH_RENDER_TYPE, buffers, stack);
        for (LevelLight light : lightList) {
            renderToCCRS(ccrs, light.box, light.colour, light.t, HaloContext.LEVEL_RENDERER);
        }

        // Render to post chain for post-processing effects
        ccrs.bind(HALO_FABULOUS_BLOOM_RENDER_TYPE, buffers, stack);
        for (LevelLight light : lightList) {
            renderToCCRS(ccrs, light.box, light.colour, light.t, HaloContext.BLOOM_RENDERER);
        }
        postChainFlushPending = true;

        buffers.endBatch();
        stack.popPose();
    }

    public static void onRenderWorldLastEvent(final RenderLevelLastEvent event) {

        // Unfabulous rendering. Batched rendering doesn't seem to work from stage events when not
        // on fabulous for some reason, so we have to do it here instead.
        if (!isFabulous()) {
            if (levelLights.isEmpty()) return;

            // Poll all pending lights from queue
            List<LevelLight> lightList = new LinkedList<>();
            LevelLight l;
            while ((l = levelLights.poll()) != null) {
                lightList.add(l);
            }

            // Prepare render
            Vec3 cam = Minecraft.getInstance().getEntityRenderDispatcher().camera.getPosition();
            PoseStack stack = event.getPoseStack();
            stack.pushPose();
            stack.translate(-cam.x, -cam.y, -cam.z);

            CCRenderState ccrs = CCRenderState.instance();
            ccrs.reset();
            MultiBufferSource.BufferSource buffers = Minecraft.getInstance().renderBuffers().bufferSource();

            // Render to normal render target for primary visuals
            ccrs.bind(HALO_GLOW_RENDER_TYPE, buffers, stack);
            for (LevelLight light : lightList) {
                renderToCCRS(ccrs, light.box, light.colour, light.t, HaloContext.LEVEL_RENDERER);
            }

            // Finish render
            buffers.endBatch();
            stack.popPose();
        }

        // Fabulous bloom post-processing effects rendered here instead of during stage
        if (isFabulous() && postChainFlushPending) {
            postChainFlushPending = false;
            assert HALO_POST_CHAIN != null;
            HALO_POST_CHAIN.process(event.getPartialTick());
            Minecraft.getInstance().getMainRenderTarget().bindWrite(false);
        }
    }

    public static void onResourceManagerReload(ResourceManager manager) {
        loadPostChain();
    }
    //endregion

    //region PostChain management
    private static void preparePostChain() {
        if (HALO_POST_CHAIN == null) {
            loadPostChain();
        }
        HALO_POST_CHAIN.resizeIfNeeded();
    }

    private static void loadPostChain() {
        unloadPostChain();

        try {
            HALO_POST_CHAIN = new HaloBloomPostChain();
            // Because for some reason this isnt done during construction
            HALO_POST_CHAIN.resize(Minecraft.getInstance().getWindow().getWidth(), Minecraft.getInstance().getWindow().getHeight());
        } catch (IOException e) {
            throw new RuntimeException("Failed to load halo post chain", e);
        }
    }

    private static void unloadPostChain() {
        if (HALO_POST_CHAIN != null) {
            HALO_POST_CHAIN.close();
            HALO_POST_CHAIN = null;
        }
    }

    private static boolean isFabulous() {
        return Configurator.fabulousLights && Minecraft.getInstance().options.graphicsMode.getId() >= GraphicsStatus.FABULOUS.getId();
    }
    //endregion

    //region Render functions
    private static void renderToCCRS(CCRenderState ccrs, Cuboid6 cuboid, int colour, Transformation t, HaloContext context) {
        ccrs.setPipeline(t);
        ccrs.baseColour = getBaseColour(colour, context);
        BlockRenderer.renderCuboid(ccrs, cuboid, 0);
    }

    public static void renderInventoryHalo(CCRenderState ccrs, PoseStack mStack, MultiBufferSource buffers, Cuboid6 cuboid, int colour, Vector3 pos) {
        RenderType type = isFabulous() ? HALO_FABULOUS_ITEM_ENTITY_RENDER_TYPE : HALO_GLOW_RENDER_TYPE;
        ccrs.reset();
        ccrs.bind(type, buffers, mStack);
        renderToCCRS(ccrs, cuboid, colour, pos.translation(), HaloContext.ITEM_RENDERER);
    }
    //endregion

    private static int getBaseColour(int colorIndex, HaloContext context) {
        return LightColours.byIndex(colorIndex).rgbaByContext(context);
    }

    private enum HaloContext {
        ITEM_RENDERER,
        LEVEL_RENDERER,
        BLOOM_RENDERER
    }

    /**
     * Colours used for Halo glow and bloom effects. They are traditional Minecraft
     * wool colours with scaled brightness components to reduce variation in
     * perceived brightness.
     */
    private enum LightColours {

        /*
        Default colour HSB values. Obvious problem is with the white colour, which
        ends up looking substantially brighter than the others.

        WHITE        rgba: ffffffffa0, hsb: [0.0,          0.0,         1.0        ]
        ORANGE       rgba: c06300ffa0, hsb: [0.0859375,    1.0,         0.7529412  ]
        MAGENTA      rgba: b51ab5ffa0, hsb: [0.8333333,    0.8563536,   0.70980394 ]
        LIGHT_BLUE   rgba: 6f84f1ffa0, hsb: [0.63974357,   0.5394191,   0.94509804 ]
        YELLOW       rgba: bfbf00ffa0, hsb: [0.16666667,   1.0,         0.7490196  ]
        LIME         rgba: 6bf100ffa0, hsb: [0.2593361,    1.0,         0.94509804 ]
        PINK         rgba: f14675ffa0, hsb: [0.954191,     0.7095436,   0.94509804 ]
        GRAY         rgba: 535353ffa0, hsb: [0.0,          0.0,         0.3254902  ]
        LIGHT_GRAY   rgba: 939393ffa0, hsb: [0.0,          0.0,         0.5764706  ]
        CYAN         rgba: 008787ffa0, hsb: [0.5,          1.0,         0.5294118  ]
        PURPLE       rgba: 5e00c0ffa0, hsb: [0.7482639,    1.0,         0.7529412  ]
        BLUE         rgba: 1313c0ffa0, hsb: [0.6666667,    0.9010417,   0.7529412  ]
        BROWN        rgba: 4f2700ffa0, hsb: [0.08227848,   1.0,         0.30980393 ]
        GREEN        rgba: 088700ffa0, hsb: [0.3234568,    1.0,         0.5294118  ]
        RED          rgba: a20f06ffa0, hsb: [0.009615381,  0.962963,    0.63529414 ]
        BLACK        rgba: 1f1f1fffa0, hsb: [0.0,          0.0,         0.12156863 ]
        */

        //@formatter:off
        WHITE     (0xFFFFFFA0, 0.9F, 0.8F, 0.8F),
        ORANGE    (0xC06300A0, 0.8F, 0.8F, 0.8F),
        MAGENTA   (0xB51AB5A0, 0.8F, 0.8F, 0.8F),
        LIGHT_BLUE(0x6F84F1A0, 0.9F, 0.9F, 0.9F),
        YELLOW    (0xBFBF00A0, 0.8F, 0.8F, 0.8F),
        LIME      (0x6BF100A0, 0.9F, 0.9F, 0.9F),
        PINK      (0xF14675A0, 0.9F, 0.9F, 0.9F),
        GRAY      (0x535353A0, 0.5F, 0.5F, 0.5F),
        LIGHT_GRAY(0x939393A0, 0.6F, 0.6F, 0.6F),
        CYAN      (0x008787A0, 0.8F, 0.8F, 0.8F),
        PURPLE    (0x5E00C0A0, 0.8F, 0.8F, 0.8F),
        BLUE      (0x1313C0A0, 0.8F, 0.8F, 0.8F),
        BROWN     (0x4F2700A0, 0.5F, 0.5F, 0.5F),
        GREEN     (0x088700A0, 0.7F, 0.7F, 0.7F),
        RED       (0xA20F06A0, 0.8F, 0.8F, 0.8F),
        BLACK     (0x1F1F1FA0, 0.4F, 0.4F, 0.4F);
        //@formatter:on

        //noinspection FieldCanBeLocal
        private final int rgba;
        private final float glowBrightness;
        private final float itemGlowBrightness;
        private final float bloomBrightness;

        public final int blockGlowRgba;
        public final int itemGlowRgba;
        public final int bloomRgba;

        LightColours(int rgba, float glowBrightness, float itemGlowBrightness, float bloomBrightness) {
            this.rgba = rgba;
            this.glowBrightness = glowBrightness;
            this.itemGlowBrightness = itemGlowBrightness;
            this.bloomBrightness = bloomBrightness;

            this.blockGlowRgba = scaleBrightness(rgba, glowBrightness);
            this.itemGlowRgba = scaleBrightness(rgba, itemGlowBrightness);
            this.bloomRgba = scaleBrightness(rgba, bloomBrightness);
        }

        public int rgbaByContext(HaloContext context) {
            return switch (context) {
                case ITEM_RENDERER -> itemGlowRgba;
                case LEVEL_RENDERER -> blockGlowRgba;
                case BLOOM_RENDERER -> bloomRgba;
            };
        }

        private static int scaleBrightness(int baseRgba, float brightness) {
            int r = (baseRgba >> 24) & 0xFF;
            int g = (baseRgba >> 16) & 0xFF;
            int b = (baseRgba >> 8) & 0xFF;
            int a = baseRgba & 0xFF;

            float[] hsb = new float[3];
            Color.RGBtoHSB(r, g, b, hsb);

            return Color.HSBtoRGB(hsb[0], hsb[1], brightness) << 8 | a;
        }

        public static LightColours byIndex(int index) {
            return values()[index];
        }
    }

    private record LevelLight(Transformation t, int colour, Cuboid6 box) {

    }
}
