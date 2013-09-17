package mrtjp.projectred.core;

import java.util.Map;

import mrtjp.projectred.core.BlockBasics.EnumBasics;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.IBlockAccess;

import org.lwjgl.opengl.GL11;

import codechicken.lib.lighting.LightModel;
import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.IconTransformation;
import codechicken.lib.render.RenderUtils;
import codechicken.lib.render.TextureUtils;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Translation;

public class RenderAlloySmelter {

    private static Map<String, CCModel> models = CCModel.parseObjModels(new ResourceLocation("projectred:textures/obj/machines/presser.obj"), 7, new InvertX());
    private static CCModel base = models.get("base");
    private static CCModel plate = models.get("plate");
    private static CCModel shaft = models.get("shaft");
        
    static {
        base.shrinkUVs(0.001);
        plate.shrinkUVs(0.001);
        shaft.shrinkUVs(0.001);
        
        base.computeLighting(LightModel.standardLightModel);
        plate.computeLighting(LightModel.standardLightModel);
        shaft.computeLighting(LightModel.standardLightModel);
    }
    
    public static void renderStatic(IBlockAccess world, int x, int y, int z, Block block, int modelId, RenderBlocks renderer) {
        CCRenderState.reset();
        CCRenderState.useModelColours(true);
        TextureUtils.bindAtlas(0);
        
        if (renderer.overrideBlockTexture == null)
            base.render(new Translation(x+.5, y, z+.5), new IconTransformation(EnumBasics.ALLOYSMELTER.icons[0]));
        else
            RenderUtils.renderBlock(Cuboid6.full, 0, new Translation(x, y, z), new IconTransformation(renderer.overrideBlockTexture), null);
    }
    
    public static void renderDynamic(double x, double y, double z, double frame, TileAlloySmelter presser) {
        double yHeight = 0;
        double color = 0;
        
        if (presser.burnTimeForRecipe <= 0)
            yHeight = 0.3D;
        else {

            double percent = presser.progress*100/presser.burnTimeForRecipe;
            yHeight = Math.max(0.3D-(percent/20D), 0.0D);
            if (percent > 80)
                yHeight = Math.min((0.3D/20)*(percent-80), 0.3D);
            if (percent > 20 && percent < 80)
                color = Math.sin(percent/30D);
        }

        TextureUtils.bindAtlas(0);
        CCRenderState.reset();
        CCRenderState.useNormals(true);
        CCRenderState.pullLightmap();
        CCRenderState.startDrawing(7);
        BasicRenderUtils.setFullColor();
        BasicRenderUtils.setFullBrightness();
        shaft.render(new Translation(x+.5, y+yHeight, z+.5), new IconTransformation(EnumBasics.ALLOYSMELTER.icons[0]));
        if (color > 0)
            Tessellator.instance.setColorOpaque_F((float)color, 0, 0);
        plate.render(new Translation(x+.5, y+yHeight, z+.5), new IconTransformation(EnumBasics.ALLOYSMELTER.icons[0]));
        CCRenderState.draw();
    }
    
    public static void renderInventory() {
        GL11.glPushMatrix();
        GL11.glTranslated(0, -0.1D, 0);
        GL11.glScalef(1, 1, 1);
        TextureUtils.bindAtlas(0);
        CCRenderState.reset();
        CCRenderState.useNormals(true);
        CCRenderState.pullLightmap();
        CCRenderState.startDrawing(7);
        base.render(new Translation(.5, 0, .5), new IconTransformation(EnumBasics.ALLOYSMELTER.icons[0]));
        plate.render(new Translation(.5, 0, .5), new IconTransformation(EnumBasics.ALLOYSMELTER.icons[0]));
        shaft.render(new Translation(.5, 0, .5), new IconTransformation(EnumBasics.ALLOYSMELTER.icons[0]));
        CCRenderState.draw();
        GL11.glPopMatrix();
    }
}
