package mrtjp.projectred.illumination;

import java.util.Map;

import mrtjp.projectred.ProjectRedIllumination;
import mrtjp.projectred.core.InvertX;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.client.IItemRenderer;

import org.lwjgl.opengl.GL11;

import codechicken.lib.lighting.LightModel;
import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.IconTransformation;
import codechicken.lib.render.RenderUtils;
import codechicken.lib.render.TextureUtils;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.Vector3;

public class RenderLantern implements IItemRenderer {
    public static RenderLantern instance = new RenderLantern();

    private static Map<String, CCModel> models;
    private static Cuboid6 box = new Cuboid6(0.35D, 0.25D, 0.35D, 0.65D, 0.75D, 0.65D).expand(-1/64D);
    private static Cuboid6 breakbox = new Cuboid6(0.35D, 0.25D, 0.35D, 0.65D, 0.75D, 0.65D);

    public static Icon[] onIcons;
    public static Icon[] offIcons;

    static
    {
        models = CCModel.parseObjModels(new ResourceLocation("projectred", "textures/obj/lights/lantern.obj"), 7, new InvertX());
        for (CCModel c : models.values()) {
            c.apply(new Translation(.5, 0, .5));
            c.computeLighting(LightModel.standardLightModel);
            c.shrinkUVs(0.0005);
        }
    }
    
    public static void registerIcons(IconRegister reg) {
        offIcons = new Icon[16];
        for (int i = 0; i < 16; i++)
            offIcons[i] = reg.registerIcon("projectred:lights/lanternoff/" + i);
        
        onIcons = new Icon[16];
        for (int i = 0; i < 16; i++)
            onIcons[i] = reg.registerIcon("projectred:lights/lanternon/" + i);
    }
    
    public void renderLantern(LanternPart l) {
        Icon icon = l.isOn()?onIcons[l.type]:offIcons[l.type];
        TextureUtils.bindAtlas(0);
        CCRenderState.reset();
        CCRenderState.setBrightness(l.world(), l.x(), l.y(), l.z());
        CCRenderState.useModelColours(true);
        renderLanternBulb(icon, l.x(), l.y(), l.z(), l.side);
        if (l.isOn())
            LastEventBasedHaloRenderer.addLight(l.x(), l.y(), l.z(), l.type, 6, box);
    }
    
    public void renderBreaking(int x, int y, int z, Icon icon) {
        RenderUtils.renderBlock(breakbox, 0, new Translation(x, y, z), new IconTransformation(icon), null);
    }
    
    public void renderLanternBulb(Icon icon, double x, double y, double z, int rotation) {
        renderPart(icon, models.get("bulb"), x, y, z, 2);
        if (rotation == 0) {
            renderPart(icon, models.get("standbottom"), x, y, z, 2);
            renderPart(icon, models.get("goldringbottom"), x, y, z, 2);
        } else if (rotation == 1) {
            renderPart(icon, models.get("standtop"), x, y, z, 2);
            renderPart(icon, models.get("goldringtop"), x, y, z, 2);
        } else {
            renderPart(icon, models.get("standside"), x, y, z, rotation);
            renderPart(icon, models.get("goldringtop"), x, y, z, rotation);
        }
    }

    @Override
    public boolean handleRenderType(ItemStack item, ItemRenderType type) {
        return true;
    }

    @Override
    public boolean shouldUseRenderHelper(ItemRenderType type, ItemStack item, ItemRendererHelper helper) {
        return true;
    }

    @Override
    public void renderItem(ItemRenderType type, ItemStack item, Object... data) {
        Icon icon = item.getItem() == ProjectRedIllumination.itemPartInvLantern ? onIcons[item.getItemDamage()] : offIcons[item.getItemDamage()];
        switch (type) {
        case ENTITY:
            renderInventory(icon, -0.25D, 0D, -0.25D, 1D);
            return;
        case EQUIPPED:
            renderInventory(icon, -0.15D, -0.15D, -0.15D, 2D);
            return;
        case EQUIPPED_FIRST_PERSON:
            renderInventory(icon, -0.15D, -0.15D, -0.15D, 2D);
            return;
        case INVENTORY:
            renderInventory(icon, 0D, -0.05D, 0D, 2D);
            return;
        default:
            return;
        }        
    }
    
    public void renderInventory(Icon icon, double x, double y, double z, double scale) {
        GL11.glPushMatrix();
        GL11.glTranslated(x, y, z);
        GL11.glScaled(scale, scale, scale);
        CCRenderState.reset();
        TextureUtils.bindAtlas(0);
        CCRenderState.useNormals(true);
        CCRenderState.startDrawing(7);
        renderPart(icon, models.get("bulb"), x, y, z, 2);
        renderPart(icon, models.get("goldringtop"), x, y, z, 2);
        CCRenderState.draw();
        GL11.glPopMatrix();
    }
    
    public void renderPart(Icon icon, CCModel cc, double x, double y, double z, int rot) {
        cc.render(0, cc.verts.length, 
                Rotation.sideOrientation(0, Rotation.rotationTo(0, rot)).at(Vector3.center)
                .with(new Translation(x, y, z)), new IconTransformation(icon), null);
    }

}
