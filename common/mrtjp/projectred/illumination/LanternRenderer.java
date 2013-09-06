package mrtjp.projectred.illumination;

import java.util.Map;

import mrtjp.projectred.ProjectRedIllumination;
import mrtjp.projectred.core.BasicRenderUtils;
import mrtjp.projectred.core.InvertX;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.client.IItemRenderer;

import org.lwjgl.opengl.GL11;

import codechicken.lib.lighting.LightModel;
import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.IUVTransformation;
import codechicken.lib.render.IconTransformation;
import codechicken.lib.render.TextureUtils;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.TransformationList;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.Vector3;

public class LanternRenderer implements IItemRenderer {

    private Map<String, CCModel> models;
    private Icon lampIcon;
    private RenderBlocks render;

    public static LanternRenderer instance = new LanternRenderer();

    public LanternRenderer() {
        models = CCModel.parseObjModels(new ResourceLocation("projectred", "textures/obj/lantern.obj"), 7, new InvertX());
        for (CCModel c : models.values()) {
            c.apply(new Translation(.5, 0, .5));
            c.computeLighting(LightModel.standardLightModel);
            c.shrinkUVs(0.0005);
        }
    }

    public void bindTexture(LanternPart lan) {
        lampIcon = lan.getLightValue() == 15 ? lan.type.onIcon : lan.type.offIcon;
    }

    public void renderLamp(LanternPart lan, RenderBlocks b) {
        render = b;
        bindTexture(lan);
        TextureUtils.bindAtlas(0);
        CCRenderState.reset();
        BasicRenderUtils.setBrightnessDirect(lan.world(), lan.x(), lan.y(), lan.z());
        CCRenderState.useModelColours(true);
        renderLampBulb(lan.x(), lan.y(), lan.z(), lan.rotation);
        if (lan.getLightValue() == 15 && b == null)
            renderLampShade(lan.x(), lan.y(), lan.z(), lan.type.meta);
    }

    public void renderPart(CCModel cc, double x, double y, double z, int rot) {
        TransformationList tl = new TransformationList();
        tl.with(Rotation.sideOrientation(0, Rotation.rotationTo(0, rot)).at(Vector3.center));
        tl.with(new Translation(0, 0, 0)).with(new Translation(x, y, z));

        IUVTransformation uv = null;
        Icon override = render == null ? null : render.overrideBlockTexture;
        if (override != null) {
            uv = new IconTransformation(override);
        } else {
            uv = new IconTransformation(lampIcon);
        }
        cc.render(0, cc.verts.length, tl, uv, null);
    }

    public void renderLampBulb(double x, double y, double z, int rotation) {
        renderPart(models.get("covertop"), x, y, z, 2);
        renderPart(models.get("coverbottom"), x, y, z, 2);
        renderPart(models.get("lamp"), x, y, z, 2);
        if (rotation == 0) {
            renderPart(models.get("standbottom"), x, y, z, 2);
            renderPart(models.get("goldringbottom"), x, y, z, 2);
        } else if (rotation == 1) {
            renderPart(models.get("standtop"), x, y, z, 2);
            renderPart(models.get("goldringtop"), x, y, z, 2);
        } else {
            renderPart(models.get("standside"), x, y, z, rotation);
            renderPart(models.get("goldringtop"), x, y, z, rotation);
        }
    }

    public void renderInventory(double x, double y, double z, float scale) {
        GL11.glPushMatrix();
        GL11.glTranslated(x, y, z);
        GL11.glScalef(scale, scale, scale);
        CCRenderState.reset();
        CCRenderState.useNormals(true);
        CCRenderState.startDrawing(7);
        renderPart(models.get("covertop"), x, y, z, 2);
        renderPart(models.get("coverbottom"), x, y, z, 2);
        renderPart(models.get("lamp"), x, y, z, 2);
        renderPart(models.get("goldringtop"), x, y, z, 2);
        CCRenderState.draw();
        GL11.glPopMatrix();
    }

    public void renderLampShade(int x, int y, int z, int tint) {
        Cuboid6 box = new Cuboid6(0.35D, 0.25D, 0.35D, 0.65D, 0.75D, 0.65D).expand(-1/64D);
        LastEventBasedHaloRenderer.addLight(x, y, z, tint, 6, box);
    }

    @Override
    public void renderItem(ItemRenderType type, ItemStack item, Object... data) {
        lampIcon = item.getItem() == ProjectRedIllumination.itemPartInvLantern ? EnumLantern.get(item.getItemDamage()).onIcon : EnumLantern.get(item.getItemDamage()).offIcon;
        switch (type) {
        case ENTITY:
            renderInventory(-.25f, 0f, -.25f, 1f);
            return;
        case EQUIPPED:
            renderInventory(-.15f, -.15f, -.15f, 2f);
            return;
        case EQUIPPED_FIRST_PERSON:
            renderInventory(-.15f, -.15f, -.15f, 2f);
            return;
        case INVENTORY:
            renderInventory(0f, -.05f, 0f, 2f);
            return;
        default:
            return;
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
}
