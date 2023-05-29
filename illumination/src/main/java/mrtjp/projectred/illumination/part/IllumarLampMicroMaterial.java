package mrtjp.projectred.illumination.part;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.RedundantTransformation;
import codechicken.lib.vec.Vector3;
import codechicken.microblock.api.BlockMicroMaterial;
import codechicken.microblock.api.MicroMaterialClient;
import codechicken.microblock.part.MicroblockPart;
import codechicken.microblock.util.MaskedCuboid;
import codechicken.multipart.util.PartRayTraceResult;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.core.client.HaloRenderer;
import mrtjp.projectred.illumination.block.IllumarLampBlock;
import net.minecraft.client.particle.ParticleEngine;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.model.ItemTransforms;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.level.block.Block;
import org.jetbrains.annotations.Nullable;

import java.util.function.Consumer;
import java.util.function.Supplier;

public class IllumarLampMicroMaterial extends BlockMicroMaterial {

    private final Supplier<Block> block;

    public IllumarLampMicroMaterial(Supplier<Block> block) {
        super(block.get().defaultBlockState());
        this.block = block;
    }

    public int getLightColor() {
        return ((IllumarLampBlock) block.get()).getColor();
    }

    @Override
    public void initializeClient(Consumer<MicroMaterialClient> cons) {
        super.initializeClient(superMicroMaterial -> cons.accept(new MicroMaterialClient() {

            // Provide RenderDynamic implementation
            @Override
            public void renderDynamic(MicroblockPart part, ItemTransforms.TransformType transform, PoseStack pStack, MultiBufferSource buffers, int packedLight, int packedOverlay, float partialTicks) {
                CCRenderState ccrs = CCRenderState.instance();
                Cuboid6 cuboid = part.getBounds().copy().expand(0.025D);

                if (transform != null) { // Inventory rendering
                    HaloRenderer.renderInventoryHalo(ccrs, pStack, buffers, cuboid, getLightColor(), Vector3.ZERO);
                } else {
                    HaloRenderer.addLight(part.pos(), getLightColor(), cuboid);
                }
            }

            @Override
            public RenderType getItemRenderLayer() {
                return RenderType.cutout();
            }

            // Delegate the rest to super material
            //@formatter:off
            @Override public boolean renderCuboids(CCRenderState ccrs, @Nullable RenderType layer, Iterable<MaskedCuboid> cuboids) { return superMicroMaterial.renderCuboids(ccrs, layer, cuboids); }
            @Override public void addHitEffects(MicroblockPart part, PartRayTraceResult hit, ParticleEngine engine) { superMicroMaterial.addHitEffects(part, hit, engine); }
            @Override public void addDestroyEffects(MicroblockPart part, PartRayTraceResult hit, ParticleEngine engine) { superMicroMaterial.addDestroyEffects(part, hit, engine); }
            @Override public void addLandingEffects(MicroblockPart part, PartRayTraceResult hit, Vector3 entity, int numberOfParticles) { superMicroMaterial.addLandingEffects(part, hit, entity, numberOfParticles); }
            @Override public void addRunningEffects(MicroblockPart part, PartRayTraceResult hit, Entity entity) { superMicroMaterial.addRunningEffects(part, hit, entity); }
            //@formatter:on
        }));
    }

    //TODO dynamic light emission
    @Override
    public int getLightEmission() {
        return super.getLightEmission();
    }

//    public int calculateLightLevel(TileMultipart tile) {
//        // Calculate how much of the 1x1x1 volume is occupied by a lamp microblock (0 -> 1)
//        double lightVolume = tile.getPartList().stream()
//                .filter(p -> p instanceof MicroblockPart)
//                .filter(p -> ((MicroblockPart) p).getMaterial() instanceof IllumarLampMicroMaterial)
//                .mapToDouble(p -> ((MicroblockPart) p).getBounds().volume())
//                .sum();
//
//        // Calc brightness between 10 and 15, linearly towards 15 based on amount of light volume
//        return (int) Math.min(15, 10 + 5*lightVolume);
//    }
}
