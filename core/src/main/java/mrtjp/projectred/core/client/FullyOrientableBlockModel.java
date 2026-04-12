package mrtjp.projectred.core.client;

import codechicken.lib.model.PerspectiveModel;
import codechicken.lib.model.PerspectiveModelState;
import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.buffer.BakedQuadVertexBuilder;
import codechicken.lib.util.TransformUtils;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Vector3;
import codechicken.lib.vec.uv.MultiIconTransformation;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.renderer.block.model.ItemOverrides;
import net.minecraft.core.Direction;
import net.minecraft.util.RandomSource;
import net.minecraft.world.level.block.state.BlockState;
import net.neoforged.neoforge.client.ChunkRenderTypeSet;
import net.neoforged.neoforge.client.model.IDynamicBakedModel;
import net.neoforged.neoforge.client.model.data.ModelData;
import net.neoforged.neoforge.client.model.generators.ConfiguredModel;

import javax.annotation.Nullable;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

/**
 * Needs to exist because Vanilla's default {@link ConfiguredModel} only supports a single x rotation and y rotation,
 * making it very difficult to render blocks that can not only be oriented to all 6 sides, but also rotated about those
 * sides. With only x/y rotations, we would have to use UV-mapping to rotate the textures themselves rather than the model.
 */
public abstract class FullyOrientableBlockModel implements IDynamicBakedModel, PerspectiveModel {
    // State -> Side -> Quads list
    private final HashMap<BlockState, HashMap<Integer, List<BakedQuad>>> modelMap = new HashMap<>();

    //region Implementation abstracts
    protected abstract RenderType getBlockRenderLayer(@Nullable BlockState state);

    protected abstract RenderData getBlockRenderData(@Nullable BlockState state);

    protected abstract BlockState getItemRenderState();
    //endregion

    @Override
    public List<BakedQuad> getQuads(@Nullable BlockState state, @Nullable Direction side, RandomSource rand, ModelData date, @Nullable RenderType renderType) {
        // Render type must match
        if (renderType != null && renderType != getBlockRenderLayer(state)) {
            return List.of();
        }
        // Full block, so no uncullable sides
        if (side == null) {
            return List.of();
        }

        if (state == null) {
            state = getItemRenderState();
        }

        return getOrGenerateQuads(state, side);
    }

    private List<BakedQuad> getOrGenerateQuads(BlockState state, Direction side) {
        HashMap<Integer, List<BakedQuad>> sideMap = modelMap.get(state);
        if (sideMap != null) return sideMap.get(side.ordinal());

        synchronized (modelMap) {
            // Re-check after waiting for sync
            sideMap = modelMap.get(state);
            if (sideMap == null) {
                sideMap = generateSideMap(state);
                modelMap.put(state, sideMap);
            }
        }

        return sideMap.get(side.ordinal());
    }

    private HashMap<Integer, List<BakedQuad>> generateSideMap(BlockState state) {
        // Prep render
        CCRenderState ccrs = CCRenderState.instance();
        ccrs.reset();
        ccrs.computeLighting = false;
        ccrs.brightness = 0;
        BakedQuadVertexBuilder builder = new BakedQuadVertexBuilder();
        ccrs.bind(builder, DefaultVertexFormat.BLOCK);

        // Render full block model with orient transform
        RenderData data = getBlockRenderData(state);
        CCModel m = CCModel.quadModel(24)
                .generateBlock(0, Cuboid6.full, 0)
                .apply(Rotation.sideOrientation(data.side, data.rotation).at(Vector3.CENTER))
                .computeNormals()
                .shrinkUVs(0.0005);

        m.render(ccrs, data.iconT);

        // Bake and separate sides
        HashMap<Integer, List<BakedQuad>> sideMap = new HashMap<>();
        List<BakedQuad> blockQuads = builder.bake();
        for (int s = 0; s < 6; s++) {
            LinkedList<BakedQuad> sideQuads = new LinkedList<>();
            for (BakedQuad quad : blockQuads) {
                if (quad.getDirection().ordinal() == s) {
                    sideQuads.add(quad);
                }
            }
            sideMap.put(s, sideQuads);
        }

        return sideMap;
    }

    @Override
    public ChunkRenderTypeSet getRenderTypes(BlockState state, RandomSource rand, ModelData data) {
        return ChunkRenderTypeSet.of(RenderType.solid());
    }

    public record RenderData(int side, int rotation, MultiIconTransformation iconT) {
    }

    //region BakedModel
    //@formatter:off
    @Override public boolean useAmbientOcclusion() { return true; }
    @Override public boolean isGui3d() { return true; }
    @Override public boolean usesBlockLight() { return true; }
    @Override public boolean isCustomRenderer() { return false; }
    @Override public ItemOverrides getOverrides() { return ItemOverrides.EMPTY; }
    @Override public @Nullable PerspectiveModelState getModelState() { return TransformUtils.DEFAULT_BLOCK; }
    //@formatter:on
    //endregion
}
