package mrtjp.projectred.transportation;

import static mrtjp.projectred.transmission.RenderWire.finishModel;
import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.transmission.RenderWire.UVT;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.entity.RenderItem;
import net.minecraft.client.renderer.entity.RenderManager;
import net.minecraft.entity.item.EntityItem;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;

import org.lwjgl.opengl.GL11;

import codechicken.lib.lighting.LazyLightMatrix;
import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.IUVTransformation;
import codechicken.lib.render.IconTransformation;
import codechicken.lib.render.RenderUtils;
import codechicken.lib.render.Vertex5;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.Vector3;

public class RenderPipe {
    private static final EntityItem dummyEntityItem = new EntityItem(null);
    private static final RenderItem customRenderItem;

    static {
        customRenderItem = new RenderItem() {
            @Override
            public boolean shouldBob() {return false;}
            @Override
            public boolean shouldSpreadItems() {return false;}
        };
        customRenderItem.setRenderManager(RenderManager.instance);
    }

    private static class WireFrameModelGenerator
    {
        double w = 2/8D;
        double d = 1/16D-0.002;//little offset for compensating for the slight uv stretch to eliminate seams

        public static void generateModels() {
            WireFrameModelGenerator gen_inst = new WireFrameModelGenerator();
            gen_inst.generateCenterModel();
            gen_inst.generateSideModels();

            gen_inst.finishModels();
        }

        public void generateCenterModel() {
            CCModel model = CCModel.quadModel(48);

            model.verts[0] = new Vertex5(0.5-w, 0.5-w, 0.5-w, 20, 8);
            model.verts[1] = new Vertex5(0.5+w, 0.5-w, 0.5-w, 28, 8);
            model.verts[2] = new Vertex5(0.5+w, 0.5-w, 0.5+w, 28, 0);
            model.verts[3] = new Vertex5(0.5-w, 0.5-w, 0.5+w, 20, 0);

            model.verts[4] = new Vertex5(0.5-w, 0.5-w+d, 0.5+w, 20, 8);
            model.verts[5] = new Vertex5(0.5+w, 0.5-w+d, 0.5+w, 28, 8);
            model.verts[6] = new Vertex5(0.5+w, 0.5-w+d, 0.5-w, 28, 0);
            model.verts[7] = new Vertex5(0.5-w, 0.5-w+d, 0.5-w, 20, 0);

            model.generateSidedParts(0, Vector3.center);
            frameModels[6] = model;
        }

        public void generateSideModels() {
            CCModel model = CCModel.quadModel(36);

            model.verts[0] = new Vertex5(0.5-w, 0, 0.5+w, 16, 0);
            model.verts[1] = new Vertex5(0.5+w, 0, 0.5+w, 16, 8);
            model.verts[2] = new Vertex5(0.5+w, 0.5-w, 0.5+w, 20, 8);
            model.verts[3] = new Vertex5(0.5-w, 0.5-w, 0.5+w, 20, 0);

            model.verts[4] = new Vertex5(0.5+w, 0, 0.5+w-d, 16, 0);
            model.verts[5] = new Vertex5(0.5-w, 0, 0.5+w-d, 16, 8);
            model.verts[6] = new Vertex5(0.5-w, 0.5-w, 0.5+w-d, 20, 8);
            model.verts[7] = new Vertex5(0.5+w, 0.5-w, 0.5+w-d, 20, 0);

            for(int r = 1; r < 4; r++)
                model.apply(Rotation.quarterRotations[r].at(Vector3.center), 0, r*8, 8);

            model.verts[32] = new Vertex5(0.5-w, 0, 0.5-w, 24, 32);
            model.verts[33] = new Vertex5(0.5+w, 0, 0.5-w, 32, 32);
            model.verts[34] = new Vertex5(0.5+w, 0, 0.5+w, 32, 24);
            model.verts[35] = new Vertex5(0.5-w, 0, 0.5+w, 24, 24);

            frameModels[0] = model;
            for(int s = 1; s < 6; s++) {
                frameModels[s] = model.copy().apply(Rotation.sideRotations[s].at(Vector3.center));

                if(s%2 == 1) {
                    Vertex5[] verts = frameModels[s].verts;
                    UVT t = new UVT(Rotation.quarterRotations[2].at(new Vector3(24, 0, 4)));
                    for(int i = 0; i < 32; i++)
                        verts[i].apply(t);
                }
            }
        }

        public void finishModels() {
            for(CCModel m : frameModels)
                finishModel(m);
        }
    }

    public static void reverseOrder(Vertex5[] verts) {
        for(int k = 0; k < verts.length; k+=4) {
            Vertex5 tmp = verts[k+1];
            verts[k+1] = verts[k+3];
            verts[k+3] = tmp;
        }
    }

    public static CCModel[] frameModels = new CCModel[7];

    private static LazyLightMatrix dynamicLight = new LazyLightMatrix();

    static {
        WireFrameModelGenerator.generateModels();
    }

    public static void render(BasicPipePart w, Vector3 pos) {
        dynamicLight.setPos(w.world(), w.x(), w.y(), w.z());
        render(w, pos, dynamicLight);
    }

    public static void render(BasicPipePart w, Vector3 pos, LazyLightMatrix olm) {
        WireFrameModelGenerator.generateModels();
        int key = w.connMap|1<<6;
        Transformation t = new Translation(pos);
        IUVTransformation uvt = new IconTransformation(w.getIcon(6));

        frameModels[6].render(t, uvt);
        for(int s = 0; s < 6; s++)
            if((key&1<<s) != 0) {
                uvt = new IconTransformation(w.getIcon(s));
                frameModels[s].render(t, uvt);
            }

    }

    public static void renderBreakingOverlay(Icon icon, BasicPipePart wire) {
        for(Cuboid6 box : wire.getCollisionBoxes())
            RenderUtils.renderBlock(box, 0, new Translation(wire.x(), wire.y(), wire.z()), new IconTransformation(icon), null);
    }

    public static void renderInv(Transformation t, Icon icon) {
        IUVTransformation uvt = new IconTransformation(icon);

        CCRenderState.setColour(-1);

        frameModels[6].render(t, uvt);
        for(int s = 0; s < 2; s++)
            frameModels[s].render(t, uvt);

    }

    public static void renderItemFlow(BasicPipePart p, Vector3 pos, float frame) {
        GL11.glPushMatrix();
        GL11.glDisable(2896 /* GL_LIGHTING */);

        for (RoutedPayload r : p.itemFlow) {
            float partial = r.getSpeed() * frame;
            float frameX = (float) (pos.x+r.x-p.x());
            float frameY = (float) (pos.y+r.y-p.y());
            float frameZ = (float) (pos.z+r.z-p.z());

            switch (r.isEntering ? r.input : r.output) {
            case UP: frameY = frameY + partial; break;
            case DOWN: frameY = frameY - partial; break;
            case SOUTH: frameZ = frameZ + partial; break;
            case NORTH: frameZ = frameZ - partial; break;
            case EAST: frameX = frameX + partial; break;
            case WEST: frameX = frameX - partial; break;
            default:
            }

            doRenderItem(r, frameX, frameY, frameZ);
        }
        GL11.glEnable(2896 /* GL_LIGHTING */);
        GL11.glPopMatrix();
    }

    private static void doRenderItem(RoutedPayload r, double x, double y, double z) {
        if (r == null || r.getItemStack() == null)
            return;
        float renderScale = 0.7f;
        ItemStack itemstack = r.getItemStack();
        GL11.glPushMatrix();
        GL11.glTranslatef((float) x, (float) y, (float) z);
        GL11.glTranslatef(0, 0.25F, 0);
        GL11.glScalef(renderScale, renderScale, renderScale);
        dummyEntityItem.setEntityItemStack(itemstack);
        customRenderItem.doRenderItem(dummyEntityItem, 0, 0, 0, 0, 0);

        prepareRenderState();
        GL11.glEnable(GL11.GL_LIGHTING);

        Tessellator.instance.setColorRGBA_I(PRColors.get(r.priority.color).rgb, 32);
        GL11.glScalef(.5f, .5f, .5f);
        RenderUtils.renderBlock(Cuboid6.full, 0, new Translation(-.5, -.5, -.5), null, null);
        restoreRenderState();

        GL11.glPopMatrix();
    }
    
    private static void prepareRenderState() {
        GL11.glEnable(GL11.GL_BLEND);
        GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE);
        GL11.glDisable(GL11.GL_TEXTURE_2D);
        GL11.glDisable(GL11.GL_LIGHTING);
        GL11.glDisable(GL11.GL_CULL_FACE);
        GL11.glDepthMask(false);
        CCRenderState.reset();
        CCRenderState.startDrawing(7);
    }

    private static void restoreRenderState() {
        CCRenderState.draw();
        GL11.glDepthMask(true);
        GL11.glColor3f(1, 1, 1);
        GL11.glEnable(GL11.GL_CULL_FACE);
        GL11.glEnable(GL11.GL_LIGHTING);
        GL11.glEnable(GL11.GL_TEXTURE_2D);
        GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA);
        GL11.glDisable(GL11.GL_BLEND);
    }
}
