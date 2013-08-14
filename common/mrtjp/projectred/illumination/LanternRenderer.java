package mrtjp.projectred.illumination;

import java.util.Map;

import mrtjp.projectred.ProjectRedIllumination;
import mrtjp.projectred.core.BasicRenderUtils;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.InvertX;
import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.illumination.LastEventBasedHaloRenderer.HaloObject;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.client.IItemRenderer;
import net.minecraftforge.client.event.RenderWorldLastEvent;

import org.lwjgl.opengl.GL11;

import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.IUVTransformation;
import codechicken.lib.render.IconTransformation;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.TransformationList;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.PartMap;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

public class LanternRenderer implements IItemRenderer {

    private Map<String, CCModel> models;
    private Icon lampIcon;
    private RenderBlocks render;

    public static LanternRenderer instance = new LanternRenderer();

    public LanternRenderer() {
        models = CCModel.parseObjModels(new ResourceLocation("projectred", "textures/obj/lantern.obj"), 7, new InvertX());
        for (CCModel c : models.values()) {
            c.apply(new Translation(.5, 0, .5));
            c.shrinkUVs(0.0005);
        }
    }

    public void bindTexture(LanternPart lan) {
        lampIcon = lan.getLightValue() == 15 ? lan.type.onIcon : lan.type.offIcon;
    }

    public void renderLamp(LanternPart lan, RenderBlocks b) {
        render = b;
        bindTexture(lan);
        CCRenderState.reset();
        BasicRenderUtils.bindTerrainResource();
        BasicRenderUtils.setBrightnessDirect(lan.world(), lan.x(), lan.y(), lan.z());
        renderLampBulb(lan.x(), lan.y(), lan.z(), lan.rotation);
        if (lan.getLightValue() == 15 && b == null) {
            renderLampShade(lan.x(), lan.y(), lan.z(), lan.type.meta);
        }
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
        // CCRenderState.useNormals(true);
        // CCRenderState.startDrawing(7);
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
        // CCRenderState.draw();
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

    public void renderLampShade(final double x, final double y, final double z, final int tint) {
        HaloObject r = new HaloObject((int) x, (int) y, (int) z) {
            @Override
            public boolean render(RenderWorldLastEvent event) {
                TileMultipart t = BasicUtils.getTileEntity(event.context.theWorld, new BlockCoord(posX, posY, posZ), TileMultipart.class);
                if (t != null) {
                    TMultiPart p = t.partMap(PartMap.CENTER.i);
                    if (!(p instanceof LanternPart)) {
                        return false;
                    }
                    if (p instanceof LanternPart) {
                        if (p.getLightValue() != 15) {
                            return false;
                        }
                    }
                } else {
                    return false;
                }
                renderPart(models.get("lampshade"), x, y, z, 2);
                return true;
            }

            @Override
            public void preRender() {
                Tessellator.instance.setColorRGBA_I(PRColors.get(tint).hex, 128);
            }

            @Override
            public void postRender() {
            }
        };

        LastEventBasedHaloRenderer.addObjectToRender(r);
    }

    @Override
    public void renderItem(ItemRenderType type, ItemStack item, Object... data) {
        lampIcon = item.getItem() == ProjectRedIllumination.itemPartInvLantern ? EnumLantern.get(item.getItemDamage()).onIcon : EnumLantern.get(item.getItemDamage()).offIcon;
        switch (type) {
        case ENTITY:
            renderInventory(-.3f, 0f, -.3f, 1.4f);
            return;
        case EQUIPPED:
            renderInventory(0f, 0f, 0f, 2f);
            return;
        case EQUIPPED_FIRST_PERSON:
            renderInventory(0f, 0f, 0f, 2f);
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
