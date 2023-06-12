package mrtjp.projectred.expansion.client;

import codechicken.lib.raytracer.VoxelShapeCache;
import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.lighting.LightModel;
import codechicken.lib.render.model.OBJParser;
import codechicken.lib.texture.AtlasRegistrar;
import codechicken.lib.vec.*;
import codechicken.lib.vec.uv.IconTransformation;
import com.google.common.collect.ImmutableSet;
import mrtjp.projectred.lib.ModelVoxelShape;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.phys.shapes.VoxelShape;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;

public class FrameModelRenderer {

    private static Map<String, CCModel> frameModels = loadModels("frame");

    private static CCModel[] models = new CCModel[64];
    private static VoxelShape[] shapes = new VoxelShape[64];
    private static IconTransformation frameIcon;

    public static TextureAtlasSprite getFrameIcon() {
        return frameIcon.icon;
    }

    public static void renderStatic(CCRenderState ccrs, int mask) {
        CCModel m = getOrGenerateModel(mask);
        m.render(ccrs, frameIcon);
    }

    public static VoxelShape getShape(int mask) {
        return getOrGenerateShape(mask);
    }

    public static void registerIcons(AtlasRegistrar registrar) {
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/frame"), i -> frameIcon = new IconTransformation(i));
    }

    private static Map<String, CCModel> loadModels(String path) {
        Map<String, CCModel> models = new OBJParser(new ResourceLocation(MOD_ID, "obj/" + path + ".obj"))
                .ignoreMtl()
                .quads()
                .parse();
        models.replaceAll((k, v) -> v.backfacedCopy());

        for (Map.Entry<String, CCModel> m : models.entrySet()) {
            m.getValue().apply(new Translation(0.5, 0, 0.5));
            m.getValue().computeNormals();
            m.getValue().shrinkUVs(0.0005);
            m.getValue().computeLighting(LightModel.standardLightModel);
        }
        return models;
    }

    private static CCModel getOrGenerateModel(int mask) {
        CCModel m = models[mask & 0x3F];
        if (m == null) {
            m = generateModel(mask);
            models[mask & 0x3F] = m;
        }
        return m;
    }

    private static VoxelShape getOrGenerateShape(int mask) {
        VoxelShape s = shapes[mask & 0x3F];
        if (s == null) {
            s = generateShape(mask);
            shapes[mask & 0x3F] = s;
        }
        return s;
    }

    private static CCModel generateModel(int mask) {
        List<CCModel> m = new LinkedList<>();
        m.add(frameModels.get("frame"));
        for (int i = 0; i < 6; i++) {
            if ((mask & (1 << i)) == 0) {
                m.add(frameModels.get("cross_" + i));
            }
        }

        return CCModel.combine(m);
    }

    private static VoxelShape generateShape(int mask) {

        // bottom face cuboids
        double th = 2/16D; // Frame thickness
        Cuboid6 e0 = new Cuboid6(0, 0, 0, 1, th, th);
        Cuboid6 e1 = e0.copy().apply(Rotation.quarterRotations[1].at(Vector3.CENTER));
        Cuboid6 e2 = e0.copy().apply(Rotation.quarterRotations[2].at(Vector3.CENTER));
        Cuboid6 e3 = e0.copy().apply(Rotation.quarterRotations[3].at(Vector3.CENTER));

        List<VoxelShape> faceShapes = new LinkedList<>();
        for (int s = 0; s < 6; s++) {
            Transformation t = Rotation.sideOrientation(s, 0).at(Vector3.CENTER);
            ImmutableSet.Builder<VoxelShape> fb = ImmutableSet.builder();
            fb.add(VoxelShapeCache.getShape(e0.copy().apply(t)));
            fb.add(VoxelShapeCache.getShape(e2.copy().apply(t)));
            fb.add(VoxelShapeCache.getShape(e1.copy().apply(t)));
            fb.add(VoxelShapeCache.getShape(e3.copy().apply(t)));
            faceShapes.add(VoxelShapeCache.merge(fb.build()));
        }

        VoxelShape parent = VoxelShapeCache.merge(ImmutableSet.copyOf(faceShapes));
        return new ModelVoxelShape(parent, getOrGenerateModel(mask));
    }
}
