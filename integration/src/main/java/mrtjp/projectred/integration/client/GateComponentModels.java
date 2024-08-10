package mrtjp.projectred.integration.client;

import codechicken.lib.colour.Colour;
import codechicken.lib.colour.EnumColour;
import codechicken.lib.math.MathHelper;
import codechicken.lib.render.BlockRenderer;
import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.lighting.LightModel;
import codechicken.lib.render.lighting.PlanarLightModel;
import codechicken.lib.render.model.OBJParser;
import codechicken.lib.render.pipeline.ColourMultiplier;
import codechicken.lib.render.pipeline.IVertexOperation;
import codechicken.lib.texture.AtlasRegistrar;
import codechicken.lib.texture.TextureUtils;
import codechicken.lib.vec.*;
import codechicken.lib.vec.uv.*;
import com.google.common.collect.ImmutableSet;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.core.BundledSignalsLib;
import mrtjp.projectred.core.client.HaloRenderer;
import mrtjp.projectred.lib.VecLib;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.Style;
import net.minecraft.resources.ResourceLocation;

import javax.annotation.Nullable;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiConsumer;

import static mrtjp.projectred.integration.ProjectRedIntegration.MOD_ID;

@SuppressWarnings("NotNullFieldNotInitialized")
public class GateComponentModels {

    public static final CCModel base = loadBaseModel("base");
    public static final CCModel lightChip = loadModel("chip");
    public static final CCModel leverOn = loadModel("leveron").apply(new Translation(0, 2 / 16D, 0));
    public static final CCModel leverOff = loadModel("leveroff").apply(new Translation(0, 2 / 16D, 0));
    public static final CCModel solarArray = loadModel("solar");
    public static final CCModel rainSensor = loadModel("rainsensor");
    public static final CCModel pointer = loadModel("pointer");
    public static final CCModel busXcvr = loadModel("array/busxcvr");
    public static final CCModel lightPanel1 = loadModel("array/lightpanel1");
    public static final CCModel lightPanel2 = loadModel("array/lightpanel2");
    public static final CCModel busRand = loadModel("array/busrand");
    public static final CCModel busConv = loadModel("array/busconv");
    public static final CCModel signalPanel = loadModel("array/signalpanel");
    public static final CCModel busInput = loadModel("array/businput");
    public static final CCModel icBundled = loadModel("array/icbundled");

    public static final Map<String, CCModel> nullCell = loadModels("array/null_cell", (k, v) -> v.apply(new Translation(0.5, 0, 0.5)));
    public static final Map<String, CCModel> logicCell = loadModels("array/logic_cell", (k, v) -> v.apply(new Translation(0.5, 0, 0.5)));
    public static final Map<String, CCModel> andCell = loadModels("array/and_cell", (k, v) -> v.apply(new Translation(0.5, 0, 0.5)));
    public static final Map<String, CCModel> transparentLatchCell = loadModels("array/transparent_latch_cell", (k, v) -> v.apply(new Translation(0.5, 0, 0.5)));
    public static final Map<String, CCModel> sevenSeg = loadModels("array/7seg");
    public static final Map<String, CCModel> sixteenSeg = loadModels("array/16seg");
    public static final CCModel segbus = loadModel("array/segbus");

    public static final Map<String, CCModel> fabIC = loadModels("fab_ic", (k, v) -> v.apply(new Translation(0.5, 0, 0.5)));
    public static final Map<String, CCModel> ioRedstoneConnector = loadModels("io_redstone_connector");
    public static final Map<String, CCModel> ioBundledConnector = loadModels("io_bundled_connector");
    public static final Map<String, CCModel> ioBuffer = loadModels("io_buffer");
    public static final Map<String, CCModel> ioBundledBuffer = loadModels("io_bundled_buffer");
    public static final Map<String, CCModel> ioBundledBus = loadModels("io_bundled_bus");

    public static IconTransformation baseIcon;
    public static IconTransformation wireBorderIcon;
    public static IconTransformation wireOffIcon;
    public static IconTransformation wireOnIcon;
    public static IconTransformation redstoneTorchOffIcon;
    public static IconTransformation redstoneTorchOnIcon;
    public static IconTransformation yellowChipOffIcon;
    public static IconTransformation yellowChipOnIcon;
    public static IconTransformation redChipOffIcon;
    public static IconTransformation redChipOnIcon;
    public static IconTransformation minusChipOffIcon;
    public static IconTransformation minusChipOnIcon;
    public static IconTransformation plusChipOffIcon;
    public static IconTransformation plusChipOnIcon;
    public static IconTransformation leverIcon;
    public static IconTransformation solarDualMode;
    public static IconTransformation solarSkyMode;
    public static IconTransformation solarBlockMode;
    public static IconTransformation rainSensorIcon;
    public static IconTransformation pointerIcon;
    public static IconTransformation busXcvrIcon;
    public static IconTransformation nullCellIcon;
    public static IconTransformation logicCellIcon;
    public static IconTransformation andCellIcon;
    public static IconTransformation transparentLatchCellIcon;
    public static IconTransformation busRandIcon;
    public static IconTransformation busConvIcon;
    public static IconTransformation busInputIcon;
    public static IconTransformation segment;
    public static IconTransformation segmentDisp;
    public static IconTransformation icChipIcon;
    public static IconTransformation icChipIconOff;
    public static IconTransformation icHousingIcon;
    public static IconTransformation ioRedstoneConnectorIcon;
    public static IconTransformation ioBundledConnectorIcon;
    public static IconTransformation ioBufferIcon;
    public static IconTransformation ioBundledBufferIcon;
    public static IconTransformation ioBundledBusIcon;

    public static void registerIcons(AtlasRegistrar registrar) {
        //@formatter:off
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/base"),                    i -> baseIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/wire_material_border"),    i -> wireBorderIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/wire_material_off"),       i -> wireOffIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/wire_material_on"),        i -> wireOnIcon = new IconTransformation(i));

        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/redstone_torch_off"),      i -> redstoneTorchOffIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/redstone_torch"),          i -> redstoneTorchOnIcon = new IconTransformation(i));

        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/yellow_chip_off"),         i -> yellowChipOffIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/yellow_chip_on"),          i -> yellowChipOnIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/red_chip_off"),            i -> redChipOffIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/red_chip_on"),             i -> redChipOnIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/minus_chip_off"),          i -> minusChipOffIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/minus_chip_on"),           i -> minusChipOnIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/plus_chip_off"),           i -> plusChipOffIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/plus_chip_on"),            i -> plusChipOnIcon = new IconTransformation(i));

        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/solar_dual_mode"),         i -> solarDualMode = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/solar_sky_mode"),          i -> solarSkyMode = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/solar_block_mode"),        i -> solarBlockMode = new IconTransformation(i));

        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/rain_sensor"),             i -> rainSensorIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/lever"),                   i -> leverIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/pointer"),                 i -> pointerIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/bus_xcvr"),                i -> busXcvrIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/null_cell"),               i -> nullCellIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/logic_cell"),              i -> logicCellIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/and_cell"),                i -> andCellIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/transparent_latch_cell"),  i -> transparentLatchCellIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/bus_randomizer"),          i -> busRandIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/bus_converter"),           i -> busConvIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/bus_input_panel"),         i -> busInputIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/segment_display"),         i -> segment = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/segment_display_digit"),   i -> segmentDisp = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/ic_active"),               i -> icChipIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/ic_inert"),                i -> icChipIconOff = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/ic_housing"),              i -> icHousingIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/io_redstone_connector"),   i -> ioRedstoneConnectorIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/io_bundled_connector"),    i -> ioBundledConnectorIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/io_buffer"),               i -> ioBufferIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/io_bundled_buffer"),       i -> ioBundledBufferIcon = new IconTransformation(i));
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/io_bundled_bus"),          i -> ioBundledBusIcon = new IconTransformation(i));
        //@formatter:on
    }

    public static Map<String, CCModel> loadModels(String path) {
        return loadModels(path, (k, v) -> { });
    }

    public static Map<String, CCModel> loadModels(String path, BiConsumer<String, CCModel> operation) {
        Map<String, CCModel> models = new OBJParser(new ResourceLocation(MOD_ID, "obj/" + path + ".obj"))
                .ignoreMtl()
                .quads()
                .parse();
        models.replaceAll((k, v) -> v.backfacedCopy());

        for (Map.Entry<String, CCModel> m : models.entrySet()) {
            operation.accept(m.getKey(), m.getValue());
            m.getValue().computeNormals();
            m.getValue().shrinkUVs(0.0005);
        }

        return models;
    }

    public static CCModel loadModel(String path) {
        return Objects.requireNonNull(CCModel.combine(loadModels(path).values()));
    }

    public static CCModel loadBaseModel(String path) {
        CCModel model = loadModel(path);
        model.apply(new Translation(0.5, 0, 0.5));
        for (int i = 0; i < model.verts.length; i++) {
            model.verts[i].vec.subtract(model.normals()[i].copy().multiply(0.0002));
        }
        return model;
    }

    public static Transformation orientT(int orient) {
        Transformation t = Rotation.sideOrientation(orient % 24 >> 2, orient & 3);
        if (orient >= 24) t = new Scale(-1, 1, 1).with(t);
        return t.at(Vector3.CENTER);
    }

    public static Transformation dynamicT(int orient) {
        return orient == 0 ? RedundantTransformation.INSTANCE : new Scale(-1, 1, 1).at(Vector3.CENTER);
    }

    public static CCModel bakeCopy(CCModel base, int orient) {
        CCModel m = base.copy();
        if (orient >= 24) reverseFacing(m);
        m.apply(orientT(orient)).computeLighting(LightModel.standardLightModel);
        return m;
    }

    public static CCModel[] bakeOrients(CCModel base) {
        CCModel[] models = new CCModel[48];
        for (int i = 0; i < 48; i++) {
            models[i] = bakeCopy(base, i);
        }
        return models;
    }

    public static CCModel[] bakeDynamic(CCModel base) {
        return new CCModel[] { base.copy(), reverseFacing(base.copy()) };
    }

    public static CCModel reverseFacing(CCModel m) {
        for (int i = 0; i < m.verts.length; i += 4) {
            Vertex5 vtmp = m.verts[i + 1];
            Vector3 ntmp = m.normals()[i + 1];
            m.verts[i + 1] = m.verts[i + 3];
            m.normals()[i + 1] = m.normals()[i + 3];
            m.verts[i + 3] = vtmp;
            m.normals()[i + 3] = ntmp;
        }
        return m;
    }

    public static WireModel[] generateWireModels(String name, int count) {
        WireModel[] models = new WireModel[count];
        for (int i = 0; i < count; i++) {
            String fullName = name + "-" + i;
            models[i] = new WireModel3D(fullName); //TODO flat models
        }
        return models;
    }

    /**
     * Maintains a cache of small models to be placed upon a 32x32 grid on a gate surface. Avoids needing to create
     * copies of the same model at the same position, each of which is 48 CCModel arrays.
     */
    public static abstract class PositionalGateComponentModelBakery {

        private final CCModel[][] cache;
        private final int numStates;

        public PositionalGateComponentModelBakery(int numStates) {
            this.cache = new CCModel[32 * 32 * numStates][];
            this.numStates = numStates;
        }

        public void clearCache() {
            Arrays.fill(cache, null);
        }

        /**
         * Fetches a component model baked at given position
         *
         * @param x     X position, 0 to 16, on 0.5 grid
         * @param z     Z position, 0 to 16, on 0.5 grid
         * @param state Implementation specific state, 0 to numStates - 1
         * @return 48-element orient-baked CCModel array
         */
        public CCModel[] getOrCreateModel(double x, double z, int state) {
            int key = modelKey(x, z, state);
            CCModel[] models = cache[key];
            if (models == null) {
                CCModel base = createBaseModel(x, z, state);
                models = bakeOrients(base);
                cache[key] = models;
            }
            return models;
        }

        private int modelKey(double x, double z, int state) {
            if ((x / 0.5) % 1 != 0 || (z / 0.5) % 1 != 0) {
                throw new IllegalArgumentException("Position (" + x + ", " + z + ") must be on a 0.5 grid");
            }
            if (x < 0 || x > 16 || z < 0 || z > 16) {
                throw new IllegalArgumentException("Position (" + x + ", " + z + ") must be in range [0, 16]");
            }
            if (state >= numStates) {
                throw new IllegalArgumentException("State " + state + " is out of range [0, " + numStates + ")");
            }
            int xi = (int) (x / 0.5);
            int zi = (int) (z / 0.5);
            return (state << 10) | zi << 5 | xi;
        }

        /**
         * Creates a base model to be baked. Called on cache misses.
         *
         * @param x     X position, 0 to 16, on 0.5 grid
         * @param z     Z position, 0 to 16, on 0.5 grid
         * @param state Implementation specific state, 0 to numStates - 1
         * @return Base model to be baked
         */
        protected abstract CCModel createBaseModel(double x, double z, int state);
    }

    /**
     * Component bakery that uses a unique model per state
     */
    public static class MultiModelComponentBakery extends PositionalGateComponentModelBakery {

        private final CCModel[] baseModels;

        /**
         * Creates a new component bakery
         *
         * @param baseModels Base models for each state, expected to be at (side0/rotation0/x0/z0)
         */
        public MultiModelComponentBakery(CCModel[] baseModels) {
            super(baseModels.length);
            this.baseModels = baseModels;
        }

        @Override
        protected CCModel createBaseModel(double x, double z, int state) {
            Transformation t = new Translation(x / 16D, 0, z / 16D);
            return baseModels[state].copy().apply(t);
        }
    }

    /**
     * Component bakery for stateless models
     */
    public static class StaticModelComponentBakery extends PositionalGateComponentModelBakery {

        private final CCModel baseModel;

        public StaticModelComponentBakery(CCModel baseModel) {
            super(1);
            this.baseModel = baseModel;
        }

        public CCModel[] getOrCreateModel(double x, double z) {
            return super.getOrCreateModel(x, z, 0);
        }

        @Override
        protected CCModel createBaseModel(double x, double z, int state) {
            Transformation t = new Translation(x / 16D, 0, z / 16D);
            return baseModel.copy().apply(t);
        }
    }

    /**
     * Redstone torch model bakery
     */
    public static class RedstoneTorchComponentBakery extends PositionalGateComponentModelBakery {

        public RedstoneTorchComponentBakery() {
            super(16);
        }

        @Override
        protected CCModel createBaseModel(double x, double z, int state) {
            return generateModel(x, z, state);
        }

        private static CCModel generateModel(double x, double z, int h) {
            CCModel m = CCModel.quadModel(20);
            m.verts[0] = new Vertex5(7 / 16D, 10 / 16D, 9 / 16D, 7 / 16D, 8 / 16D);
            m.verts[1] = new Vertex5(9 / 16D, 10 / 16D, 9 / 16D, 9 / 16D, 8 / 16D);
            m.verts[2] = new Vertex5(9 / 16D, 10 / 16D, 7 / 16D, 9 / 16D, 6 / 16D);
            m.verts[3] = new Vertex5(7 / 16D, 10 / 16D, 7 / 16D, 7 / 16D, 6 / 16D);
            m.generateBlock(4, 6 / 16D, (10 - h) / 16D, 7 / 16D, 10 / 16D, 11 / 16D, 9 / 16D, 0x33);
            m.generateBlock(12, 7 / 16D, (10 - h) / 16D, 6 / 16D, 9 / 16D, 11 / 16D, 10 / 16D, 0xF);
            m.apply(new Translation(-0.5 + x / 16, (h - 10) / 16D, -0.5 + z / 16));
            m.computeNormals();
            m.shrinkUVs(0.0005);
            m.apply(new Scale(1.0005));
            return m;
        }

        private static CCModel generateFlippedModel(double x, double y, double z, int h) {
            return generateModel(x, z, h).backfacedCopy()
                    .apply(new Scale(1, -1, 1))
                    .apply(new Translation(0, y / 16D, 0));
        }
    }

    public static abstract class ComponentModel {

        public abstract void renderModel(Transformation t, int orient, CCRenderState ccrs);

        protected void registerIcons(AtlasRegistrar registrar) {
        }
    }

    public static abstract class SingleComponentModel extends ComponentModel {

        protected abstract CCModel[] models();

        protected abstract UVTransformation getUVT();

        @Override
        public void renderModel(Transformation t, int orient, CCRenderState ccrs) {
            models()[orient].render(ccrs, t, getUVT());
        }
    }

    public static abstract class StaticComponentModel extends SingleComponentModel {

        private final CCModel[] models;

        public StaticComponentModel(CCModel base) {
            this.models = bakeOrients(base);
        }

        @Override
        protected CCModel[] models() {
            return models;
        }
    }

    public static abstract class MultiComponentModel extends ComponentModel {

        public int state = 0;

        protected abstract CCModel[] models(int state);

        protected abstract UVTransformation getUVT();

        @Override
        public void renderModel(Transformation t, int orient, CCRenderState ccrs) {
            models(state)[orient].render(ccrs, t, getUVT());
        }
    }

    public static abstract class OnOffModel extends SingleComponentModel {

        public boolean on = false;
    }

    public static abstract class StateIconModel extends SingleComponentModel {

        public int state = 0;
    }

    public static class BaseComponentModel extends StaticComponentModel {

        public static final BaseComponentModel INSTANCE = new BaseComponentModel();

        private BaseComponentModel() {
            super(base);
        }

        @Override
        protected UVTransformation getUVT() {
            return baseIcon;
        }
    }

    public static abstract class WireModel extends ComponentModel {

        public boolean on = false;
        public boolean disabled = false;

        public static List<Rectangle4i> rectangulate(Colour[] data) {

            boolean[] wireCorners = new boolean[32 * 32];

            for (int y = 0; y < 30; y++) {
                for (int x = 0; x < 31; x++) {
                    if (data[y * 32 + x].rgba() != -1) continue;
                    if (overlap(wireCorners, x, y)) continue;
                    if (!segment2by2(data, x, y)) {
                        throw new RuntimeException("Wire segment is not 2x2 at " + x + ", " + y);
                    }
                    wireCorners[y * 32 + x] = true;
                }
            }

            LinkedList<Rectangle4i> rects = new LinkedList<>();

            for (int i = 0; i < wireCorners.length; i++) {
                if (!wireCorners[i]) continue;

                Rectangle4i rect = new Rectangle4i(i % 32, i / 32, 0, 0);
                int x = rect.x + 2;
                while (x < 30 && wireCorners[rect.y * 32 + x]) { x += 2; }
                rect.w = x - rect.x;

                int y = rect.y + 2;
                while (y < 30) {
                    boolean advance = true;
                    int dx = rect.x;
                    while (dx < rect.x + rect.w && advance) {
                        if (!wireCorners[y * 32 + dx]) advance = false;
                        dx += 2;
                    }

                    if (!advance) break;
                    y += 2;
                }
                rect.h = y - rect.y;

                for (int dy = rect.y; dy < rect.y + rect.h; dy += 2) {
                    for (int dx = rect.x; dx < rect.x + rect.w; dx += 2) {
                        wireCorners[dy * 32 + dx] = false;
                    }
                }

                rects.add(rect);
            }

            return rects;
        }

        private static boolean overlap(boolean[] wireCorners, int x, int y) {
            return wireCorners[y * 32 + x - 1] ||
                    (y > 0 && wireCorners[(y - 1) * 32 + x]) ||
                    (y > 0 && wireCorners[(y - 1) * 32 + x - 1]);
        }

        private static boolean segment2by2(Colour[] data, int x, int y) {
            return data[y * 32 + x + 1].rgba() == -1 &&
                    data[((y + 1) * 32 + x)].rgba() == -1 &&
                    data[((y + 1) * 32 + x + 1)].rgba() == -1;
        }

        public static Rectangle4i border(Rectangle4i wire) {
            Rectangle4i border = new Rectangle4i(wire.x - 2, wire.y - 2, wire.w + 4, wire.h + 4);
            if (border.x < 0) {
                border.w += border.x;
                border.x = 0;
            }
            if (border.y < 0) {
                border.h += border.y;
                border.y = 0;
            }
            if (border.x + border.w >= 32) border.w -= border.x + border.w - 32;
            if (border.y + border.h >= 32) border.h -= border.y + border.h - 32;
            return border;
        }
    }

    public static class WireModel3D extends WireModel {

        private static final ConcurrentHashMap<String, CCModel[]> cache = new ConcurrentHashMap<>();

        private final String textureName;

        private @Nullable MultiIconTransformation wireOnIcons;
        private @Nullable MultiIconTransformation wireOffIcons;

        public WireModel3D(String textureName) {
            this.textureName = textureName;
        }

        protected CCModel[] models() {
            return getOrGenerateModels(textureName);
        }

        protected UVTransformation getUVT() {
            // Regenerate if icons are different
            if (wireOnIcons == null || wireOffIcons == null || wireOnIcons.icons[0] != wireBorderIcon.icon) {
                wireOnIcons = new MultiIconTransformation(wireBorderIcon.icon, wireOnIcon.icon);
                wireOffIcons = new MultiIconTransformation(wireBorderIcon.icon, wireOffIcon.icon);
            }
            return disabled ? wireBorderIcon : on ? wireOnIcons : wireOffIcons;
        }

        @Override
        public void renderModel(Transformation t, int orient, CCRenderState ccrs) {
            models()[orient].render(ccrs, t, getUVT());
        }

        public static void regenerateModels() {
            Set<String> textures = new HashSet<>(cache.keySet());
            cache.clear();

            for (String k : textures) {
                getOrGenerateModels(k);
            }
        }

        private static CCModel[] getOrGenerateModels(String textureName) {
            return cache.computeIfAbsent(textureName, WireModel3D::generateModels);
        }

        private static CCModel[] generateModels(String textureName) {
            Colour[] data = TextureUtils.loadTextureColours(new ResourceLocation(MOD_ID, "textures/block/surface/" + textureName + ".png"));
            List<Rectangle4i> rects = rectangulate(data);
            CCModel model = CCModel.quadModel(rects.size() * 40);
            int i = 0;
            for (Rectangle4i rect : rects) {
                // Generate border model
                generateWireSegment(model, i, border(rect), 0.01, 0);
                // Generate wire model
                generateWireSegment(model, i + 20, rect, 0.02, 1);
                i += 40;
            }
            model.computeNormals();
            model.shrinkUVs(0.0005);
            return bakeOrients(model);
        }

        private static void generateWireSegment(CCModel model, int i, Rectangle4i rect, double h, int icon) {
            double x1 = rect.x / 32D;
            double x2 = (rect.x + rect.w) / 32D;
            double z1 = rect.y / 32D;
            double z2 = (rect.y + rect.h) / 32D;
            double d = 0.0004 - h / 50D; // little offset for wires go on top of the border
            model.generateBlock(i, x1 + d, 0.125, z1 + d, x2 - d, 0.125 + h, z2 - d, 1);
            for (int v = 0; v < 20; v++) {
                model.verts[i + v].uv.tex = icon;
            }
        }
    }

    public interface IRedstoneTorchComponentModel {

        boolean isLit();

        Vector3 getLightPos();
    }

    public static class RedstoneTorchModel extends OnOffModel implements IRedstoneTorchComponentModel {

        private static final RedstoneTorchComponentBakery bakery = new RedstoneTorchComponentBakery();

        private final CCModel[] models;
        private final Vector3 lightPos;

        public RedstoneTorchModel(double x, double z, int h) {
            this.models = bakery.getOrCreateModel(x, z, h);
            lightPos = new Vector3(x, h - 1, z).multiply(1 / 16D);
        }

        @Override
        protected CCModel[] models() {
            return models;
        }

        @Override
        protected UVTransformation getUVT() {
            return on ? redstoneTorchOnIcon : redstoneTorchOffIcon;
        }

        @Override
        public boolean isLit() {
            return on;
        }

        @Override
        public Vector3 getLightPos() {
            return lightPos;
        }
    }

    public static class FlippedRedstoneTorchModel extends OnOffModel implements IRedstoneTorchComponentModel {

        private final CCModel[] models;
        private final Vector3 lightPos;

        public FlippedRedstoneTorchModel(double x, double y, double z, int h) {
            models = bakeOrients(RedstoneTorchComponentBakery.generateFlippedModel(x, y, z, h));
            lightPos = new Vector3(x, y - h, z).multiply(1 / 16D);
        }

        @Override
        protected CCModel[] models() {
            return models;
        }

        @Override
        protected UVTransformation getUVT() {
            return on ? redstoneTorchOnIcon : redstoneTorchOffIcon;
        }

        @Override
        public boolean isLit() {
            return on;
        }

        @Override
        public Vector3 getLightPos() {
            return lightPos;
        }
    }

    public static class LeverModel extends MultiComponentModel {

        private static final MultiModelComponentBakery bakery = new MultiModelComponentBakery(new CCModel[] { leverOn, leverOff });

        private final CCModel[][] models;

        public LeverModel(double x, double z) {

            models = new CCModel[][] { bakery.getOrCreateModel(x, z, 0), bakery.getOrCreateModel(x, z, 1) };
        }

        @Override
        protected CCModel[] models(int state) {
            return models[state];
        }

        @Override
        protected UVTransformation getUVT() {
            return leverIcon;
        }
    }

    public abstract static class LightChipModel extends OnOffModel {

        private static final StaticModelComponentBakery bakery = new StaticModelComponentBakery(lightChip);

        private final CCModel[] models;

        public LightChipModel(double x, double z) {
            this.models = bakery.getOrCreateModel(x, z, 0);
        }

        @Override
        protected CCModel[] models() {
            return models;
        }
    }

    public static class YellowChipModel extends LightChipModel {

        public YellowChipModel(double x, double z) {
            super(x, z);
        }

        @Override
        protected UVTransformation getUVT() {
            return on ? yellowChipOnIcon : yellowChipOffIcon;
        }
    }

    public static class RedChipModel extends LightChipModel {

        public RedChipModel(double x, double z) {
            super(x, z);
        }

        @Override
        protected UVTransformation getUVT() {
            return on ? redChipOnIcon : redChipOffIcon;
        }
    }

    public static class MinusChipModel extends LightChipModel {

        public MinusChipModel(double x, double z) {
            super(x, z);
        }

        @Override
        protected UVTransformation getUVT() {
            return on ? minusChipOnIcon : minusChipOffIcon;
        }
    }

    public static class PlusChipModel extends LightChipModel {

        public PlusChipModel(double x, double z) {
            super(x, z);
        }

        @Override
        protected UVTransformation getUVT() {
            return on ? plusChipOnIcon : plusChipOffIcon;
        }
    }

    public static class SolarModel extends StateIconModel {

        private static final StaticModelComponentBakery bakery = new StaticModelComponentBakery(solarArray);

        private final CCModel[] models;

        public SolarModel(double x, double z) {
            this.models = bakery.getOrCreateModel(x, z, 0);
        }

        @Override
        protected CCModel[] models() {
            return models;
        }

        @Override
        protected UVTransformation getUVT() {
            return state == 0 ? solarDualMode : state == 1 ? solarSkyMode : solarBlockMode;
        }
    }

    public static class RainSensorModel extends SingleComponentModel {

        private static final StaticModelComponentBakery bakery = new StaticModelComponentBakery(rainSensor);

        private final CCModel[] models;

        public RainSensorModel(double x, double z) {
            this.models = bakery.getOrCreateModel(x, z, 0);
        }

        @Override
        protected CCModel[] models() {
            return models;
        }

        @Override
        protected UVTransformation getUVT() {
            return rainSensorIcon;
        }
    }

    public static class PointerModel extends ComponentModel {

        private final CCModel[] models;
        private final Vector3 position;

        public double angle = 0;

        public PointerModel(double x, double y, double z, double scale) {
            models = bakeDynamic(pointer.copy().apply(new Scale(scale, 1, scale)));
            position = new Vector3(x, y - 1, z).multiply(1 / 16D);
        }

        public PointerModel(double x, double y, double z) {
            this(x, y, z, 1);
        }

        @Override
        public void renderModel(Transformation t, int orient, CCRenderState ccrs) {
            models[orient].render(
                    ccrs,
                    new Rotation(-angle + MathHelper.pi, 0, 1, 0)
                            .with(position.translation())
                            .with(dynamicT(orient))
                            .with(t),
                    pointerIcon,
                    LightModel.standardLightModel);
        }
    }

    public static abstract class BundledCableModel extends SingleComponentModel {

        private final CCModel[] models;

        public BundledCableModel(CCModel model, Vector3 pos, double uCenter, double vCenter) {
            models = new CCModel[48];

            Translation p = pos.copy().multiply(1 / 16D).translation();
            CCModel translatedModel = model.copy().apply(p);
            for (int orient = 0; orient < 48; orient++) {
                models[orient] = bakeCopy(translatedModel, orient);

                int side = orient % 24 >> 2;
                int r = orient & 3;
                boolean reflect = orient >= 24;
                boolean rotate = (r + BundledSignalsLib.bundledCableBaseRotationMap[side]) % 4 >= 2;

                UVTransformation t = RedundantUVTransformation.INSTANCE;
                if (reflect) {
                    t = t.with(new UVScale(-1, 1));
                }
                if (rotate) {
                    t = t.with(new UVRotation(180 * MathHelper.torad));
                }

                models[orient].apply(t.at(new UV(uCenter, vCenter)));
            }
        }

        @Override
        protected CCModel[] models() {
            return models;
        }
    }

    public static class BusXcvrCableModel extends BundledCableModel {

        public static final BusXcvrCableModel INSTANCE = new BusXcvrCableModel();

        private BusXcvrCableModel() {
            super(busXcvr, new Vector3(8, 0, 8), 10 / 32D, 14 / 32D);
        }

        @Override
        protected UVTransformation getUVT() {
            return busXcvrIcon;
        }
    }

    public static class BusRandCableModel extends BundledCableModel {

        public static final BusRandCableModel INSTANCE = new BusRandCableModel();

        private BusRandCableModel() {
            super(busRand, new Vector3(8, 0, 8), 7 / 32D, 12 / 32D);
        }

        @Override
        protected UVTransformation getUVT() {
            return busRandIcon;
        }
    }

    public static class BusConvCableModel extends BundledCableModel {

        public static final BusConvCableModel INSTANCE = new BusConvCableModel();

        private BusConvCableModel() {
            super(busConv, new Vector3(8, 0, 8), 7 / 32D, 12 / 32D);
        }

        @Override
        protected UVTransformation getUVT() {
            return busConvIcon;
        }
    }

    public static class BusInputPanelCableModel extends BundledCableModel {

        public static final BusInputPanelCableModel INSTANCE = new BusInputPanelCableModel();

        private BusInputPanelCableModel() {
            super(busInput, new Vector3(8, 0, 8), 16 / 32D, 16 / 32D);
        }

        @Override
        protected UVTransformation getUVT() {
            return busInputIcon;
        }
    }

    public static class SignalPanelModel extends ComponentModel {

        private final Vector3 pos;
        private final int r;

        private final CCModel[] displayModels;
        private final CCModel[] models;
        private final CCModel[] modelsSI;

        public final boolean sideIndicator = true;
        public int signal = 0;
        public int disableMask = 0;

        public int offColour = 0x420000FF;
        public int onColour = 0xEC0000FF;
        public final int disableColour = EnumColour.GRAY.rgba();

        public SignalPanelModel(double x, double z, int r) {
            this(new Vector3(x, 0, z), r);
        }

        public SignalPanelModel(Vector3 pos, int r) {

            this.pos = pos.copy().multiply(1 / 16D);
            this.r = r;

            displayModels = new CCModel[16];
            for (int i = 0; i < 16; i++) {
                CCModel m = CCModel.quadModel(4);
                int x = i % 4;
                int z = i / 4;
                double y = 10 / 32D + 0.0001D;

                m.verts[0] = new Vertex5(x, y, z + 1, x, z);
                m.verts[1] = new Vertex5(x + 1, y, z + 1, x + 1, z);
                m.verts[2] = new Vertex5(x + 1, y, z, x + 1, z + 1);
                m.verts[3] = new Vertex5(x, y, z, x, z + 1);
                m.apply(new Scale(1 / 16D, 1, 1 / 16D).with(new Translation(-2 / 16D, 0, -2 / 16D)));
                m.apply(new UVTranslation(22, 0));
                m.apply(new UVScale(1 / 32D));
                m.computeNormals();
                m.shrinkUVs(0.0005);
                displayModels[i] = m;
            }

            CCModel base = lightPanel2.copy().apply(Rotation.quarterRotations[r]).apply(this.pos.translation());
            CCModel baseSI = lightPanel1.copy().apply(Rotation.quarterRotations[r]).apply(this.pos.translation());

            models = bakeOrients(base);
            modelsSI = bakeOrients(baseSI);
        }

        @Override
        public void renderModel(Transformation t, int orient, CCRenderState ccrs) {

            IconTransformation iconT = busXcvrIcon;
            (sideIndicator ? modelsSI : models)[orient].render(ccrs, t, iconT);

            Vector3 dPos = pos.copy();
            if (orient >= 24) {
                dPos.x = 1 - dPos.x;
            }

            Transformation dispT = Rotation.quarterRotations[r].with(dPos.translation()).with(orientT(orient % 24)).with(t);

            for (int i = 0; i < 16; i++) {
                int colour = (signal & 1 << i) != 0 ? onColour : (disableMask & 1 << i) != 0 ? disableColour : offColour;
                displayModels[i].render(ccrs, dispT, iconT, PlanarLightModel.standardLightModel, ColourMultiplier.instance(colour));
            }
        }
    }

    public static class SignalBarModel extends ComponentModel {

        private final CCModel[] models;
        private final CCModel[] bars = new CCModel[16];
        private final CCModel[] barsInv = new CCModel[16];
        private final CCModel barsBg;
        private final CCModel barsBgInv;

        private final Vector3 pos;

        public int signal = 0;
        public boolean inverted = false;

        public SignalBarModel(double x, double z) {
            pos = new Vector3(x, 0, z).multiply(1 / 16D);

            for (int i = 0; i < 16; i++) {
                CCModel bar = CCModel.quadModel(4);
                double y = 12 / 32D + 0.0001D;
                bar.verts[0] = new Vertex5(0, y, 0, 0, 0);
                bar.verts[1] = new Vertex5(0, y, i + 1, 0, i + 1);
                bar.verts[2] = new Vertex5(1, y, i + 1, 2, i + 1);
                bar.verts[3] = new Vertex5(1, y, 0, 2, 0);
                bar.apply(new UVTranslation(22, 0));
                bar.apply(new UVScale(1 / 32D, 1 / 128D));
                bar.shrinkUVs(0.0005);

                CCModel bar1 = bar.backfacedCopy();
                bar1.apply(new Translation(-0.5, 0, -12));
                bar1.apply(new Scale(1 / 16D, 1, -1 / 64D));
                bar1.computeNormals();
                CCModel bar2 = bar.copy();
                bar2.apply(new Translation(-0.5, 0, -1.25 * 4));
                bar2.apply(new Scale(1 / 16D, 1, 1 / 64D));
                bar2.computeNormals();

                bars[i] = bar1;
                barsInv[i] = bar2;
            }

            Transformation t = new Scale(4 / 8D + 1, 0.9999D, 4 / 32D + 1);
            barsBg = bars[15].copy().apply(t);
            barsBgInv = barsInv[15].copy().apply(t);

            CCModel base = signalPanel.copy().apply(pos.translation());
            models = bakeOrients(base);
        }

        @Override
        public void renderModel(Transformation t, int orient, CCRenderState ccrs) {
            IconTransformation iconT = busConvIcon;
            models[orient].render(ccrs, t, iconT);

            Transformation position = pos.translation().with(orientT(orient % 24)).with(t);

            CCModel bgModel = inverted ? barsBgInv : barsBg;
            CCModel barsModel = (inverted ? barsInv : bars)[signal];

            bgModel.render(ccrs, position, iconT, PlanarLightModel.standardLightModel, ColourMultiplier.instance(0x535353FF));
            barsModel.render(ccrs, position, iconT, PlanarLightModel.standardLightModel, ColourMultiplier.instance(0xEC0000FF));
        }
    }

    public static class InputPanelButtonsModel extends ComponentModel {

        private static final Cuboid6[] UNPRESSED_BOXES = VecLib.buildCubeArray(4, 4, new Cuboid6(3, 1, 3, 13, 3, 13), new Vector3(-0.25, 0, -0.25));
        private static final Cuboid6[] PRESSED_BOXES = VecLib.buildCubeArray(4, 4, new Cuboid6(3, 1, 3, 13, 2.5, 13), new Vector3(-0.25, 0, -0.25));
        private static final Cuboid6[] LIGHT_BOXES = VecLib.buildCubeArray(4, 4, new Cuboid6(3, 1, 3, 13, 2.5, 13), new Vector3(-0.25, 0, -0.25).add(0.2));

        public int pressMask = 0;

        @Override
        public void renderModel(Transformation t, int orient, CCRenderState ccrs) {
            IconTransformation iconT = baseIcon;
            for (int i = 0; i < 16; i++) {
                ccrs.setPipeline(PlanarLightModel.standardLightModel, orientT(orient).with(t), iconT, ColourMultiplier.instance(EnumColour.values()[i].rgba()));
                BlockRenderer.renderCuboid(ccrs, ((pressMask & 1 << i) != 0 ? PRESSED_BOXES : UNPRESSED_BOXES)[i], 1);
            }
        }

        public void renderLights(CCRenderState ccrs, BlockPos lightPos, PoseStack mStack, MultiBufferSource buffers, Transformation t) {
            Transformation t2 = t.with(new Translation(lightPos));
            for (int i = 0; i < 16; i++) {
                if ((pressMask & 1 << i) != 0) {
                    HaloRenderer.addLight(t2, LIGHT_BOXES[i], i);
                }
            }
        }
    }

    public static abstract class CellWireModel extends ComponentModel {

        public byte signal = 0;

        protected int signalColour(byte signal) {
            return (signal & 0xFF) / 2 + 60 << 24 | 0xFF;
        }

        protected IVertexOperation colourMult() {
            return ColourMultiplier.instance(signalColour(signal));
        }
    }

    public static abstract class CellTopWireModel extends CellWireModel {

        private final CCModel[] leftModels;
        private final CCModel[] rightModels;
        private final CCModel[] topModels;

        public int conn = 0;

        public CellTopWireModel(CCModel[] leftModels, CCModel[] rightModels, CCModel[] topModels) {
            this.leftModels = leftModels;
            this.rightModels = rightModels;
            this.topModels = topModels;
        }

        protected abstract UVTransformation getIconT();

        @Override
        public void renderModel(Transformation t, int orient, CCRenderState ccrs) {
            topModels[orient].render(ccrs, t, getIconT(), colourMult());
            if ((conn & 2) == 0) rightModels[orient].render(ccrs, t, getIconT(), colourMult());
            if ((conn & 8) == 0) leftModels[orient].render(ccrs, t, getIconT(), colourMult());
        }
    }

    public static abstract class CellBottomWireModel extends CellWireModel {

        private final CCModel[] models;

        public CellBottomWireModel(CCModel[] models) {
            this.models = models;
        }

        protected abstract UVTransformation getIconT();

        @Override
        public void renderModel(Transformation t, int orient, CCRenderState ccrs) {
            models[orient].render(ccrs, t, getIconT(), colourMult());
        }
    }

    public static class NullCellTopWireModel extends CellTopWireModel {

        private static final CCModel[] leftModels;
        private static final CCModel[] rightModels;
        private static final CCModel[] topModels;

        static {
            //TODO only needs 24 unflipped models
            topModels = bakeOrients(nullCell.get("top_wire"));
            leftModels = bakeOrients(nullCell.get("left_wire"));
            rightModels = bakeOrients(nullCell.get("right_wire"));
        }

        public NullCellTopWireModel() {
            super(leftModels, rightModels, topModels);
        }

        protected UVTransformation getIconT() {
            return nullCellIcon;
        }
    }

    public static class NullCellBottomWireModel extends CellBottomWireModel {

        private static final CCModel[] bottomWireModels = bakeOrients(nullCell.get("bottom_wire"));

        public NullCellBottomWireModel() {
            super(bottomWireModels);
        }

        @Override
        protected UVTransformation getIconT() {
            return nullCellIcon;
        }
    }

    public static class NullCellBaseModel extends StaticComponentModel {

        private static final CCModel nullCellBaseModel;

        static {
            // Base and wood frame
            nullCellBaseModel = Objects.requireNonNull(CCModel.combine(ImmutableSet.of(nullCell.get("base"), nullCell.get("frame"))));
        }

        public static final NullCellBaseModel INSTANCE = new NullCellBaseModel();

        private NullCellBaseModel() {
            super(nullCellBaseModel);
        }

        @Override
        protected UVTransformation getUVT() {
            return nullCellIcon;
        }
    }

    public static class LogicCellTopWireModel extends CellTopWireModel {

        private static final CCModel[] leftModels;
        private static final CCModel[] rightModels;
        private static final CCModel[] topModels;

        static {
            //TODO only needs 24 unflipped models
            topModels = bakeOrients(logicCell.get("top_wire"));
            leftModels = bakeOrients(logicCell.get("left_wire"));
            rightModels = bakeOrients(logicCell.get("right_wire"));
        }

        public LogicCellTopWireModel() {
            super(leftModels, rightModels, topModels);
        }

        protected UVTransformation getIconT() {
            return nullCellIcon;
        }
    }

    public static class LogicCellBottomWireModel extends CellBottomWireModel {

        private static final CCModel[] bottomWireModels = bakeOrients(logicCell.get("bottom_wire"));

        public LogicCellBottomWireModel() {
            super(bottomWireModels);
        }

        @Override
        protected UVTransformation getIconT() {
            return logicCellIcon;
        }
    }

    public static class LogicCellBaseModel extends StaticComponentModel {

        private static final CCModel logicCellBaseModel;

        static {
            logicCellBaseModel = Objects.requireNonNull(CCModel.combine(ImmutableSet.of(
                    logicCell.get("base"),
                    logicCell.get("frame"),
                    logicCell.get("plate"))));
        }

        public static final LogicCellBaseModel INSTANCE = new LogicCellBaseModel();

        private LogicCellBaseModel() {
            super(logicCellBaseModel);
        }

        @Override
        protected UVTransformation getUVT() {
            return logicCellIcon;
        }
    }

    public static class AndCellTopWireModel extends CellTopWireModel {

        private static final CCModel[] leftModels;
        private static final CCModel[] rightModels;
        private static final CCModel[] topModels;

        static {
            //TODO only needs 24 unflipped models
            topModels = bakeOrients(andCell.get("top_wire"));
            leftModels = bakeOrients(andCell.get("left_wire"));
            rightModels = bakeOrients(andCell.get("right_wire"));
        }

        public AndCellTopWireModel() {
            super(leftModels, rightModels, topModels);
        }

        protected UVTransformation getIconT() {
            return andCellIcon;
        }
    }

    public static class AndCellBaseModel extends StaticComponentModel {

        private static final CCModel andCellBaseModel;

        static {
            andCellBaseModel = Objects.requireNonNull(CCModel.combine(ImmutableSet.of(
                    andCell.get("base"),
                    andCell.get("frame"),
                    andCell.get("plate"))));
        }

        public static final AndCellBaseModel INSTANCE = new AndCellBaseModel();

        private AndCellBaseModel() {
            super(andCellBaseModel);
        }

        @Override
        protected UVTransformation getUVT() {
            return andCellIcon;
        }
    }

    public static class TransparentLatchCellTopWireModel extends CellTopWireModel {

        private static final CCModel[] leftModels;
        private static final CCModel[] rightModels;
        private static final CCModel[] topModels;

        static {
            //TODO only needs 24 unflipped models
            topModels = bakeOrients(transparentLatchCell.get("top_wire"));
            leftModels = bakeOrients(transparentLatchCell.get("left_wire"));
            rightModels = bakeOrients(transparentLatchCell.get("right_wire"));
        }

        public TransparentLatchCellTopWireModel() {
            super(leftModels, rightModels, topModels);
        }

        protected UVTransformation getIconT() {
            return transparentLatchCellIcon;
        }
    }

    public static class TransparentLatchCellBaseModel extends StaticComponentModel {

        private static final CCModel transparentLatchCellBaseModel;

        static {
            transparentLatchCellBaseModel = Objects.requireNonNull(CCModel.combine(ImmutableSet.of(
                    transparentLatchCell.get("base"),
                    transparentLatchCell.get("frame"))));
        }

        public static final TransparentLatchCellBaseModel INSTANCE = new TransparentLatchCellBaseModel();

        private TransparentLatchCellBaseModel() {
            super(transparentLatchCellBaseModel);
        }

        @Override
        protected UVTransformation getUVT() {
            return transparentLatchCellIcon;
        }
    }

    public static abstract class SegmentDisplayModel extends SingleComponentModel {

        private final Transformation dPos;
        private final int segmentCount;

        private final CCModel[] models;
        private final CCModel[] segmentModels;

        public int signal = 0;
        public int onColour = EnumColour.RED.rgba();
        public final int offColour = EnumColour.BLACK.rgba();

        public SegmentDisplayModel(double x, double z, Map<String, CCModel> modelMap) {
            dPos = new Vector3(x, 0, z).multiply(1 / 16D).translation();
            segmentCount = modelMap.size() - 1; // Minus 1 for base model

            models = bakeOrients(modelMap.get("base").copy().apply(dPos));
            segmentModels = new CCModel[segmentCount];
            for (int i = 0; i < segmentCount; i++) {
                segmentModels[i] = modelMap.get(String.valueOf(i));
            }
        }

        public void setColourByIndex(int index) {
            onColour = EnumColour.values()[index].rgba();
        }

        @Override
        protected CCModel[] models() {
            return models;
        }

        protected abstract UVTransformation getSegmentUVT();

        @Override
        public void renderModel(Transformation t, int orient, CCRenderState ccrs) {
            super.renderModel(t, orient, ccrs);
            UVTransformation iconT = getSegmentUVT();
            Transformation dispT = dPos.with(orientT(orient % 24)).with(t);

            for (int i = 0; i < segmentCount; i++) {
                segmentModels[i].render(ccrs, dispT, iconT, PlanarLightModel.standardLightModel, ColourMultiplier.instance((signal & 1 << i) != 0 ? onColour : offColour));
            }
        }
    }

    public static class SevenSegmentDisplayModel extends SegmentDisplayModel {

        public SevenSegmentDisplayModel(double x, double z) {
            super(x, z, sevenSeg);
        }

        @Override
        protected UVTransformation getUVT() {
            return segment;
        }

        @Override
        protected UVTransformation getSegmentUVT() {
            return segmentDisp;
        }
    }

    public static class SixteenSegmentDisplayModel extends SegmentDisplayModel {

        public SixteenSegmentDisplayModel(double x, double z) {
            super(x, z, sixteenSeg);
        }

        @Override
        protected UVTransformation getUVT() {
            return segment;
        }

        @Override
        protected UVTransformation getSegmentUVT() {
            return segmentDisp;
        }
    }

    public static class SegmentDisplayBusCableModel extends BundledCableModel {

        public static final SegmentDisplayBusCableModel INSTANCE = new SegmentDisplayBusCableModel();

        private SegmentDisplayBusCableModel() {
            super(segbus, new Vector3(8, 0, 8), 9 / 32D, 16.5 / 32D);
        }

        @Override
        protected UVTransformation getUVT() {
            return segment;
        }
    }

    public static class SidedICBundledCableModel extends BundledCableModel {

        public int sidemask = 0;

        public SidedICBundledCableModel() {
            super(icBundled, new Vector3(8, 0, 8), 7 / 32D, 12 / 32D);
        }

        @Override
        protected UVTransformation getUVT() {
            return busConvIcon;
        }

        @Override
        public void renderModel(Transformation t, int orient, CCRenderState ccrs) {
            for (int r = 0; r < 4; r++) {
                if ((sidemask & 1 << r) != 0) {
                    super.renderModel(t, orient & 0xFC | ((orient & 3) + r) % 4, ccrs);
                }
            }
        }
    }

    public static class SidedWireModel extends ComponentModel {

        public int sidemask = 0;

        public final WireModel[] wires;

        public SidedWireModel(WireModel[] wires) {
            this.wires = wires;
        }

        @Override
        public void renderModel(Transformation t, int orient, CCRenderState ccrs) {
            for (int r = 0; r < 4; r++) {
                if ((sidemask & 1 << r) != 0) {
                    wires[r].renderModel(t, orient, ccrs);
                }
            }
        }
    }

    public static class FabricatedICModel extends ComponentModel {

        public static final FabricatedICModel INSTANCE = new FabricatedICModel();

        private static final Style UNIFORM = Style.EMPTY.withFont(new ResourceLocation("minecraft", "uniform"));
        private static final CCModel[] platformModel = bakeOrients(fabIC.get("platform"));
        private static final CCModel[] icChipModel = bakeOrients(fabIC.get("ic"));

        @Override
        public void renderModel(Transformation t, int orient, CCRenderState ccrs) {
            platformModel[orient].render(ccrs, t, icHousingIcon);
            icChipModel[orient].render(ccrs, t, icChipIcon);
        }

        public void renderName(String name, Transformation t1, PoseStack mStack, MultiBufferSource bufferSource, int argb, int packedLight) {

            Component nameComponent = Component.literal(name).withStyle(UNIFORM);
            Font fr = Minecraft.getInstance().font;

            // Calculate font scale
            int tw = fr.width(nameComponent);
            int th = fr.lineHeight;
            double wScale = 8/16D * (1.0/tw); // Cap width to 8/16 block
            double hScale = 2/16D * (1.0/th); // Cap height to 2/16 block
            double scale = Math.min(wScale, hScale); // Use the limiting scale

            // Create the transform
            Transformation t = new Rotation(90 * MathHelper.torad, 1, 0, 0)
                    .with(new Scale(scale, 1, scale))
                    .with(new Translation(8/16D, 2.2501/16D, 11.5/16D))
                    .with(new Translation(-(tw / 2.0) * scale, 0, -(th / 2.0) * scale))
                    .with(t1);
            Matrix4 m = new Matrix4();
            t.apply(m);

            // Draw text
            mStack.pushPose();
            mStack.mulPoseMatrix(m.toMatrix4f());
            fr.drawInBatch(nameComponent, 0, 0, argb, false, mStack.last().pose(), bufferSource, false, 0, packedLight);
            mStack.popPose();
        }

        public void renderGlass(Transformation t, CCRenderState ccrs) {
            fabIC.get("glass").render(ccrs, t, icHousingIcon);
        }
    }

    public static class IORedstoneConnectorModel extends StaticComponentModel {

        public static final IORedstoneConnectorModel INSTANCE = new IORedstoneConnectorModel();

        private IORedstoneConnectorModel() {
            super(ioRedstoneConnector.get("connector")
                    .copy()
                    .apply(new Translation(0.5, 2/16D, 0.5)));
        }

        @Override
        protected UVTransformation getUVT() {
            return ioRedstoneConnectorIcon;
        }
    }

    public static class IORedstoneConnectorWireModel extends CellWireModel {

        private static final CCModel[] models = bakeOrients(ioRedstoneConnector.get("redalloy").copy().apply(new Translation(0.5, 2/16D, 0.5)));

        private IconTransformation getUVT() {
            return ioRedstoneConnectorIcon;
        }

        @Override
        public void renderModel(Transformation t, int orient, CCRenderState ccrs) {
            models[orient].render(ccrs, t, getUVT(), colourMult());
        }
    }

    public static class IOBufferModel extends ComponentModel {

        public int colour = EnumColour.WHITE.ordinal();
        public boolean isInput = true;

        private final CCModel[] boxModels;
        private final CCModel[] inputArrowModels;
        private final CCModel[] outputArrowModels;

        public IOBufferModel(double x, double z) {
            Transformation t = new Translation(x/16D, 2/16D, z/16D);

            CCModel m = ioBuffer.get("box").copy().apply(t);
            boxModels = bakeOrients(m);

            m = ioBuffer.get("arrow").copy().apply(t);
            inputArrowModels = bakeOrients(m);

            m = ioBuffer.get("arrow").copy()
                    .apply(new Rotation(180 * MathHelper.torad, 0, 1, 0))
                    .apply(t);
            outputArrowModels = bakeOrients(m);
        }

        private UVTransformation getColourUVT() {
            return new UVTranslation((colour%2) * 4 / 32D, (colour/2) * 4 / 32D).with(ioBufferIcon);
        }

        private UVTransformation getBoxUVT() {
            return ioBufferIcon;
        }

        @Override
        public void renderModel(Transformation t, int orient, CCRenderState ccrs) {
            boxModels[orient].render(ccrs, t, getBoxUVT());
            (isInput ? inputArrowModels : outputArrowModels)[orient].render(ccrs, t, getColourUVT());
        }
    }

    public static class IOBundledConnectorModel extends StaticComponentModel {

        public static final IOBundledConnectorModel INSTANCE = new IOBundledConnectorModel();

        private IOBundledConnectorModel() {
            super(Objects.requireNonNull(CCModel.combine(ioBundledConnector.values()))
                    .copy()
                    .apply(new Translation(0.5, 2/16D, 0.5)));
        }

        @Override
        protected UVTransformation getUVT() {
            return ioBundledConnectorIcon;
        }
    }

    public static class IOBundledBufferModel extends ComponentModel {

        public int colour = EnumColour.WHITE.ordinal();
        public boolean isInput = true;
        public boolean showInsulated = true;

        private final CCModel[] boxModels;
        private final CCModel[] insulatedWireModels;
        private final CCModel[] inputArrowModels;
        private final CCModel[] outputArrowModels;
        private final CCModel[][] selectorModels;

        public IOBundledBufferModel(double x, double z) {
            Transformation t = new Translation(x/16D, 2/16D, z/16D);

            CCModel m = ioBundledBuffer.get("box").copy().apply(t);
            boxModels = bakeOrients(m);

            m = ioBundledBuffer.get("insulated_wire").copy().apply(t);
            insulatedWireModels = bakeOrients(m);

            m = ioBundledBuffer.get("arrow").copy().apply(t);
            inputArrowModels = bakeOrients(m);

            m = ioBundledBuffer.get("arrow").copy()
                    .apply(new Rotation(180 * MathHelper.torad, 0, 1, 0))
                    .apply(t);
            outputArrowModels = bakeOrients(m);

            // Create 15 selector models
            selectorModels = new CCModel[16][];
            for (int i = 0; i < 16; i++) {
                // Each color is 0.5 wide, and starts at x = -4.
                double xPos = -4.0 + 0.25 + i * 0.5;

                m = ioBundledBuffer.get("selector").copy()
                        .apply(new Translation(xPos/16D, 0, 0))
                        .apply(t);
                selectorModels[i] = bakeOrients(m);
            }
        }

        private UVTransformation getColourUVT() {
            return new UVTranslation((colour%2) * 4 / 32D, (colour/2) * 4 / 32D).with(ioBundledBufferIcon);
        }

        private UVTransformation getBoxUVT() {
            return ioBundledBufferIcon;
        }

        @Override
        public void renderModel(Transformation t, int orient, CCRenderState ccrs) {
            boxModels[orient].render(ccrs, t, getBoxUVT());
            selectorModels[colour][orient].render(ccrs, t, getBoxUVT());
            (isInput ? inputArrowModels : outputArrowModels)[orient].render(ccrs, t, getColourUVT());
            if (showInsulated) {
                insulatedWireModels[orient].render(ccrs, t, getColourUVT());
            }
        }
    }

    public static class IOBundledBusCableModel extends BundledCableModel {

        public static final IOBundledBusCableModel INSTANCE = new IOBundledBusCableModel();

        private final CCModel[] boxModels;

        private IOBundledBusCableModel() {
            super(ioBundledBus.get("cable"), new Vector3(8, 0, 8), 7 / 32D, 12 / 32D);

            Transformation t = new Translation(8/16D, 0, 8/16D);
            CCModel m = ioBundledBus.get("box").copy().apply(t);
            boxModels = bakeOrients(m);
        }

        @Override
        protected UVTransformation getUVT() {
            return ioBundledBusIcon;
        }

        @Override
        public void renderModel(Transformation t, int orient, CCRenderState ccrs) {
            super.renderModel(t, orient, ccrs);
            boxModels[orient].render(ccrs, t, getUVT());
        }
    }

    private static class RedundantUVTransformation extends UVTransformation {

        public static final RedundantUVTransformation INSTANCE = new RedundantUVTransformation();

        private RedundantUVTransformation() {
        }

        @Override
        public void apply(UV vec) {
        }

        @Override
        public UVTransformation at(UV point) {
            return this;
        }

        @Override
        public UVTransformation inverse() {
            return this;
        }

        @Override
        public UVTransformation merge(UVTransformation next) {
            return next;
        }

        @Override
        public boolean isRedundant() {
            return true;
        }

        @Override
        public String toString() {
            return "Nothing()";
        }

        @Override
        public UVTransformation copy() {
            return this;
        }
    }
}
