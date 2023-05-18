//package mrtjp.projectred.illumination.part;
//
//import codechicken.lib.render.CCRenderState;
//import codechicken.microblock.Microblock;
//import codechicken.microblock.api.MicroBlockTrait;
//import codechicken.multipart.block.TileMultiPart;
//import com.mojang.blaze3d.matrix.MatrixStack;
//import net.minecraft.client.renderer.IRenderTypeBuffer;
//
///**
// * TODO actually extend Microblock once MixinGenerator supports it
// */
//@MicroBlockTrait(IllumarLampMicroblockMixinMarker.class)
//public class IllumarLampMicroblock /* extends codechicken.microblock.Microblock */ {
//
//    //region Synthetic overrides
//    /* @Override */
//    public void renderDynamic(MatrixStack mStack, IRenderTypeBuffer buffers, int packedLight, int packedOverlay, float partialTicks) {
//        getMaterial().renderHalo(CCRenderState.instance(), mStack, buffers, asMicroblock());
//    }
//
//    /* @Override */
//    public int getLightValue() {
//        return getMaterial().calculateLightLevel(tile());
//    }
//    //endregion
//
//    private Microblock asMicroblock() {
//        return (Microblock) ((Object) this);
//    }
//
//    private IllumarLampMicroMaterial getMaterial() {
//        return (IllumarLampMicroMaterial) asMicroblock().getMaterial();
//    }
//
//    private TileMultiPart tile() {
//        return asMicroblock().tile();
//    }
//}
