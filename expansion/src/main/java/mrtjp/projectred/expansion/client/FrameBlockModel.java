package mrtjp.projectred.expansion.client;

import codechicken.lib.model.PerspectiveModel;
import codechicken.lib.model.PerspectiveModelState;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.buffer.BakedQuadVertexBuilder;
import codechicken.lib.util.TransformUtils;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.model.BakedQuad;
import net.minecraft.client.renderer.block.model.ItemOverrides;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.core.Direction;
import net.minecraft.util.RandomSource;
import net.minecraft.world.level.block.state.BlockState;
import net.neoforged.neoforge.client.ChunkRenderTypeSet;
import net.neoforged.neoforge.client.model.IDynamicBakedModel;
import net.neoforged.neoforge.client.model.data.ModelData;
import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.List;

public class FrameBlockModel implements IDynamicBakedModel, PerspectiveModel {

    // Mask -> List<BakedQuads>
    private static final HashMap<Integer, List<BakedQuad>> bakedQuads = new HashMap<>();

    public FrameBlockModel() {
    }

    @Override
    public List<BakedQuad> getQuads(@Nullable BlockState state, @Nullable Direction side, RandomSource rand, ModelData extraData, @Nullable RenderType renderType) {
        // Cutout only
        if (renderType != null && renderType != RenderType.cutout()) return List.of();

        // No cull yet
        if (side != null) return List.of();

        var frameData = extraData.get(FrameModelData.DATA);
        int mask = frameData != null ? frameData.mask() : 0;

        return getOrGenerateQuads(mask);
    }

    @Override
    public @Nullable PerspectiveModelState getModelState() {
        return TransformUtils.DEFAULT_BLOCK;
    }

    @Override
    public ChunkRenderTypeSet getRenderTypes(BlockState state, RandomSource rand, ModelData data) {
        return ChunkRenderTypeSet.of(RenderType.cutout());
    }

    private static List<BakedQuad> getOrGenerateQuads(int mask) {
        var quads = bakedQuads.get(mask);
        if (quads != null) return quads;

        synchronized (bakedQuads) {
            // Re-check after waiting for sync
            quads = bakedQuads.get(mask);
            if (quads != null) return quads;

            quads = generateQuads(mask);
            bakedQuads.put(mask, quads);
        }

        return quads;
    }

    private static List<BakedQuad> generateQuads(int mask) {
        CCRenderState ccrs = CCRenderState.instance();
        ccrs.reset();
        ccrs.computeLighting = false; // No lighting whilst baking!
        ccrs.brightness = 0;
        BakedQuadVertexBuilder builder = new BakedQuadVertexBuilder();
        ccrs.bind(builder, DefaultVertexFormat.BLOCK);
        FrameModelRenderer.renderStatic(ccrs, mask);

        return builder.bake();
    }

    //region BakedModel
    //@formatter:off
    @Override public boolean useAmbientOcclusion() { return true; }
    @Override public boolean isGui3d() { return true; }
    @Override public boolean usesBlockLight() { return true; }
    @Override public boolean isCustomRenderer() { return false; }
    @Override public TextureAtlasSprite getParticleIcon() { return FrameModelRenderer.getFrameIcon(); }
    @Override public ItemOverrides getOverrides() { return ItemOverrides.EMPTY; }
    //@formatter:on
    //endregion
}
