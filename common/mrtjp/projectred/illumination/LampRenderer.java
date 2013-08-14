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
import net.minecraftforge.client.model.IModelCustom;

import org.lwjgl.opengl.GL11;

import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.IUVTransformation;
import codechicken.lib.render.IconTransformation;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.TransformationList;
import codechicken.lib.vec.Translation;
import codechicken.multipart.PartMap;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

public class LampRenderer implements IItemRenderer {

    private Map<String, CCModel> models;
    private Icon lampIcon;
    private RenderBlocks render;

    public static LampRenderer instance = new LampRenderer();

    public LampRenderer() {
        models = CCModel.parseObjModels(new ResourceLocation("projectred", "textures/obj/lamp.obj"), 7, new InvertX());
        for (CCModel c : models.values()) {
            c.shrinkUVs(0.0005);
        }
    }

    private IModelCustom model;

    public void render() {
        model.renderAll();
    }

    public void renderPart(String part) {
        model.renderPart(part);
    }
    
    public void bindTexture(LampPart lamp) {
        lampIcon = lamp.getLightValue() == 15 ? lamp.type.onIcon : lamp.type.offIcon;
    }

    public void renderLamp(LampPart lamp, RenderBlocks b) {
        render = b;
        bindTexture(lamp);
        CCRenderState.reset();
        BasicRenderUtils.bindTerrainResource();
        BasicRenderUtils.setBrightnessDirect(lamp.world(), lamp.x(), lamp.y(), lamp.z());
        renderLampBulb(lamp.x(), lamp.y(), lamp.z());
        if (lamp.getLightValue() == 15) {
            renderLampShade(lamp.x(), lamp.y(), lamp.z(), lamp.type.meta);
        }
        
    }
    
    public void renderPart(CCModel cc, double x, double y, double z) {
        TransformationList tl = new TransformationList();
        tl.with(new Translation(1, 0, 1)).with(new Translation(x, y, z));

        IUVTransformation uv = null;
        Icon override = render == null ? null : render.overrideBlockTexture;
        if (override != null) {
            uv = new IconTransformation(override);
        } else {
            uv = new IconTransformation(lampIcon);
        }
        cc.render(0, cc.verts.length, tl, uv, null);
    }

    
    public void renderLampBulb(double x, double y, double z) {
        renderPart(models.get("lamp"), x, y, z);
    }

    public void renderLampShade(final double x, final double y, final double z, final int tint) {
        HaloObject r = new HaloObject((int) x, (int) y, (int) z) {
            @Override
            public boolean render(RenderWorldLastEvent event) {
                TileMultipart t = BasicUtils.getTileEntity(event.context.theWorld, new BlockCoord(posX, posY, posZ), TileMultipart.class);
                if (t != null) {
                    TMultiPart p = t.partMap(PartMap.CENTER.i);
                    if (!(p instanceof LampPart)) {
                        return false;
                    }
                    if (p instanceof LampPart) {
                        if (p.getLightValue() != 15) {
                            return false;
                        }
                    }
                } else {
                    return false;
                }
                renderPart(models.get("shade"), x, y, z);
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
    
    public void renderInventory(double x, double y, double z, float scale) {
        GL11.glPushMatrix();
        GL11.glTranslated(x, y, z);
        GL11.glScalef(scale, scale, scale);
        BasicRenderUtils.bindTerrainResource();
        GL11.glDisable(GL11.GL_LIGHTING);
        CCRenderState.reset();
        CCRenderState.useNormals(true);
        CCRenderState.startDrawing(7);
        BasicRenderUtils.setFullColor();
        renderPart(models.get("lamp"), x, y, z);
        CCRenderState.draw();
        GL11.glEnable(GL11.GL_LIGHTING);

        GL11.glPopMatrix();
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
        lampIcon = item.getItem() == ProjectRedIllumination.itemPartInvLamp ? EnumLamp.get(item.getItemDamage()).onIcon : EnumLamp.get(item.getItemDamage()).offIcon;
        switch (type) {
        case ENTITY:
            renderInventory(0f, 0f, 0f, .5f);
            return;
        case EQUIPPED:
            renderInventory(0f, 0f, 0f, 1f);
            return;
        case EQUIPPED_FIRST_PERSON:
            renderInventory(0f, 0f, 0f, 1f);
            return;
        case INVENTORY:
            renderInventory(0f, -.05f, 0f, 1f);
            return;
        default:
            return;
        }

    }
}
