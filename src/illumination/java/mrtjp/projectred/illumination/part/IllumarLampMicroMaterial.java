package mrtjp.projectred.illumination.part;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.*;
import codechicken.microblock.HollowMicroblock;
import codechicken.microblock.Microblock;
import codechicken.microblock.MicroblockClient;
import codechicken.microblock.api.BlockMicroMaterial;
import codechicken.multipart.block.TileMultiPart;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.projectred.core.RenderHalo;
import mrtjp.projectred.illumination.block.IllumarLampBlock;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.Atlases;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraft.client.renderer.model.ItemCameraTransforms;
import net.minecraft.item.ItemStack;

import java.util.LinkedList;
import java.util.List;
import java.util.function.Supplier;

import static codechicken.lib.vec.Vector3.CENTER;

public class IllumarLampMicroMaterial extends BlockMicroMaterial implements IllumarLampMicroblockMixinMarker {

    private final Supplier<Block> block;

    public IllumarLampMicroMaterial(Supplier<Block> block) {
        super(block.get().defaultBlockState());
        this.block = block;
    }

    public int getLightColor() {
        return ((IllumarLampBlock) block.get()).getColor();
    }

    @Override
    public boolean renderItem(CCRenderState ccrs, ItemStack stack, ItemCameraTransforms.TransformType transformType, MatrixStack mStack, IRenderTypeBuffer buffers, Matrix4 mat, MicroblockClient part) {
        // Render part
        ccrs.bind(Atlases.translucentItemSheet(), buffers, mat);
        part.render(null, ccrs);

        // Render halo
        MatrixStack stack2 = new MatrixStack();
        stack2.last().pose().set(mat.toMatrix4f());
        renderHalo(ccrs, stack2, buffers, (Microblock) part);
        return true;
    }

    public void renderHalo(CCRenderState ccrs, MatrixStack mStack, IRenderTypeBuffer buffers, Microblock part) {
        List<Cuboid6> boxes = new LinkedList<>();

        if (part instanceof HollowMicroblock) {
            byte shape = part.shape();
            int size = ((HollowMicroblock) part).getHollowSize();
            double d1 = 0.5 - size / 32D;
            double d2 = 0.5 + size / 32D;
            double t = (shape >> 4) / 8D;
            double ex = 0.025D;

            boxes.add(new Cuboid6(0 - ex, 0 - ex, 0 - ex, 1 + ex, t + ex, d1 + ex));
            boxes.add(new Cuboid6(0 - ex, 0 - ex, d2 - ex, 1 + ex, t + ex, 1 + ex));
            boxes.add(new Cuboid6(0 - ex, 0 - ex, d1 + ex, d1 + ex, t + ex, d2 - ex));
            boxes.add(new Cuboid6(d2 - ex, 0 - ex, d1 + ex, 1 + ex, t + ex, d2 - ex));

            Transformation tr = Rotation.sideRotations[shape & 0xF].at(CENTER);
            boxes.forEach(b -> b.apply(tr));
        } else {
            boxes.add(part.getBounds().copy().expand(0.025D));
        }

        RenderHalo.prepareRenderState(ccrs, mStack, buffers);
        for (Cuboid6 box : boxes) {
            RenderHalo.renderToCCRS(ccrs, box, getLightColor(), new RedundantTransformation());
        }
    }

    public int calculateLightLevel(TileMultiPart tile) {
        // Calculate how much of the 1x1x1 volume is occupied by a lamp microblock (0 -> 1)
        double lightVolume = tile.getPartList().stream()
                .filter(p -> p instanceof Microblock)
                .filter(p -> ((Microblock) p).getMaterial() instanceof IllumarLampMicroMaterial)
                .mapToDouble(p -> ((Microblock) p).getBounds().volume())
                .sum();

        // Calc brightness between 10 and 15, linearly towards 15 based on amount of light volume
        return (int) Math.min(15, 10 + 5*lightVolume);
    }
}
