package mrtjp.projectred.transportation;

import mrtjp.projectred.core.PRColors;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.entity.RenderItem;
import net.minecraft.client.renderer.entity.RenderManager;
import net.minecraft.entity.item.EntityItem;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraftforge.common.ForgeDirection;

import org.lwjgl.opengl.GL11;

import codechicken.lib.lighting.LazyLightMatrix;
import codechicken.lib.lighting.LightModel;
import codechicken.lib.math.MathHelper;
import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.IUVTransformation;
import codechicken.lib.render.IconTransformation;
import codechicken.lib.render.RenderUtils;
import codechicken.lib.render.UV;
import codechicken.lib.render.UVScale;
import codechicken.lib.render.Vertex5;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.Vector3;

public class RenderPipe
{
    private static final EntityItem dummyEntityItem = new EntityItem(null);
    private static final RenderItem customRenderItem;

    static
    {
        customRenderItem = new RenderItem() {
            @Override
            public boolean shouldBob()
            {
                return false;
            }

            @Override
            public boolean shouldSpreadItems()
            {
                return false;
            }
        };
        customRenderItem.setRenderManager(RenderManager.instance);
    }

    public static class UVT implements IUVTransformation
    {

        public Transformation t;
        private Vector3 vec = new Vector3();

        public UVT(Transformation t)
        {
            this.t = t;
        }

        @Override
        public void transform(UV uv)
        {
            vec.set(uv.u, 0, uv.v).apply(t);
            uv.set(vec.x, vec.z);
        }
    }

    private static class PipeModelGenerator
    {
        double w = 2 / 8D;
        double d = 1 / 16D - 0.002;// little offset for compensating for the
                                   // slight uv stretch to eliminate seams

        public static void generateModels()
        {
            PipeModelGenerator gen_inst = new PipeModelGenerator();
            gen_inst.generateCenterModel();
            gen_inst.generateCrossExclusiveModels();
            gen_inst.generateSideModels();

            gen_inst.finishModels();
        }

        public void generateCenterModel()
        {
            CCModel model = CCModel.quadModel(48);

            model.verts[0] = new Vertex5(0.5 - w, 0.5 - w, 0.5 - w, 4, 8);
            model.verts[1] = new Vertex5(0.5 + w, 0.5 - w, 0.5 - w, 12, 8);
            model.verts[2] = new Vertex5(0.5 + w, 0.5 - w, 0.5 + w, 12, 0);
            model.verts[3] = new Vertex5(0.5 - w, 0.5 - w, 0.5 + w, 4, 0);

            model.verts[4] = new Vertex5(0.5 - w, 0.5 - w + d, 0.5 + w, 4, 8);
            model.verts[5] = new Vertex5(0.5 + w, 0.5 - w + d, 0.5 + w, 12, 8);
            model.verts[6] = new Vertex5(0.5 + w, 0.5 - w + d, 0.5 - w, 12, 0);
            model.verts[7] = new Vertex5(0.5 - w, 0.5 - w + d, 0.5 - w, 4, 0);

            model.generateSidedParts(0, Vector3.center);
            centerModels[3] = model;
        }
        
        public void generateCrossExclusiveModels()
        {
            CCModel model = CCModel.quadModel(48);
            
            model.verts[0] = new Vertex5(0.5 - w, 0.5 - w, 0.5 - w, 0, 16);
            model.verts[1] = new Vertex5(0.5 + w, 0.5 - w, 0.5 - w, 8, 16);
            model.verts[2] = new Vertex5(0.5 + w, 0.5 - w, 0.5 + w, 8, 8);
            model.verts[3] = new Vertex5(0.5 - w, 0.5 - w, 0.5 + w, 0, 8);

            model.verts[4] = new Vertex5(0.5 - w, 0.5 - w + d, 0.5 + w, 0, 16);
            model.verts[5] = new Vertex5(0.5 + w, 0.5 - w + d, 0.5 + w, 8, 16);
            model.verts[6] = new Vertex5(0.5 + w, 0.5 - w + d, 0.5 - w, 8, 8);
            model.verts[7] = new Vertex5(0.5 - w, 0.5 - w + d, 0.5 - w, 0, 8);

            for(int s = 1; s < 4; s++)
                model.generateSidedPart(0, s, Vector3.center, 0, 8*s, 8);

            //TODO ask CB about this
            for (int i = 0; i < 48; i++)
                if (model.verts[i] == null)
                    model.verts[i] = new Vertex5();
            
            centerModels[0] = model.copy().apply(Rotation.sideOrientation(2, 1).at(Vector3.center));
            centerModels[1] = model.copy().apply(Rotation.sideOrientation(0, 1).at(Vector3.center));
            centerModels[2] = model;
        }

        public void generateSideModels()
        {
            CCModel model = CCModel.quadModel(36);

            model.verts[0] = new Vertex5(0.5 - w, 0, 0.5 + w, 0, 0);
            model.verts[1] = new Vertex5(0.5 + w, 0, 0.5 + w, 0, 8);
            model.verts[2] = new Vertex5(0.5 + w, 0.5 - w, 0.5 + w, 4, 8);
            model.verts[3] = new Vertex5(0.5 - w, 0.5 - w, 0.5 + w, 4, 0);

            model.verts[4] = new Vertex5(0.5 + w, 0, 0.5 + w - d, 0, 0);
            model.verts[5] = new Vertex5(0.5 - w, 0, 0.5 + w - d, 0, 8);
            model.verts[6] = new Vertex5(0.5 - w, 0.5 - w, 0.5 + w - d, 4, 8);
            model.verts[7] = new Vertex5(0.5 + w, 0.5 - w, 0.5 + w - d, 4, 0);

            for (int r = 1; r < 4; r++)
                model.apply(Rotation.quarterRotations[r].at(Vector3.center), 0, r * 8, 8);

            model.verts[32] = new Vertex5(0.5 - w, 0, 0.5 - w, 8, 16);
            model.verts[33] = new Vertex5(0.5 + w, 0, 0.5 - w, 16, 16);
            model.verts[34] = new Vertex5(0.5 + w, 0, 0.5 + w, 16, 8);
            model.verts[35] = new Vertex5(0.5 - w, 0, 0.5 + w, 8, 8);

            sideModels[0] = model;
            for (int s = 1; s < 6; s++)
            {
                sideModels[s] = model.copy().apply(Rotation.sideRotations[s].at(Vector3.center));

                if (s % 2 == 1)
                {
                    Vertex5[] verts = sideModels[s].verts;
                    UVT t = new UVT(Rotation.quarterRotations[2].at(new Vector3(8, 0, 4)));
                    for (int i = 0; i < 32; i++)
                        verts[i].apply(t);
                }
            }
        }

        public void finishModels()
        {
            for (CCModel m : sideModels)
                finishModel(m);
            for (CCModel m : centerModels)
                finishModel(m);
        }
    }

    public static CCModel finishModel(CCModel m)
    {
        m.apply(new UVScale(1 / 16D));
        m.shrinkUVs(0.0005);
        m.computeNormals();
        m.computeLighting(LightModel.standardLightModel);

        return m;
    }

    public static void reverseOrder(Vertex5[] verts)
    {
        for (int k = 0; k < verts.length; k += 4)
        {
            Vertex5 tmp = verts[k + 1];
            verts[k + 1] = verts[k + 3];
            verts[k + 3] = tmp;
        }
    }

    public static CCModel[] sideModels = new CCModel[6];
    public static CCModel[] centerModels = new CCModel[4];

    private static LazyLightMatrix dynamicLight = new LazyLightMatrix();

    static
    {
        PipeModelGenerator.generateModels();
    }

    public static void render(BasicPipePart w, Vector3 pos)
    {
        dynamicLight.setPos(w.world(), w.x(), w.y(), w.z());
        render(w, pos, dynamicLight);
    }

    public static void render(BasicPipePart p, Vector3 pos, LazyLightMatrix olm)
    {
        Transformation t = new Translation(pos);
        IUVTransformation uvt = new IconTransformation(EnumPipe.VALID_PIPE[p.meta].sprites[0]);
        
        int connMap = p.connMap;

        if (Integer.bitCount(connMap) == 2 && ((connMap&3) == 3 || (connMap&12) == 12 || (connMap&48) == 48))
            for (int a = 0; a < 3; a++)
            {
                if (connMap >> a * 2 == 3)
                    centerModels[a].render(t, uvt);
            }
        else
            centerModels[3].render(t, uvt);     
        
        for (int s = 0; s < 6; s++)
            if ((connMap & 1 << s) != 0)
            {
                uvt = new IconTransformation(p.getIcon(s));
                sideModels[s].render(t, uvt);
            }
    }

    public static void renderBreakingOverlay(Icon icon, BasicPipePart wire)
    {
        for (Cuboid6 box : wire.getCollisionBoxes())
            RenderUtils.renderBlock(box, 0, new Translation(wire.x(), wire.y(), wire.z()), new IconTransformation(icon), null);
    }

    public static void renderInv(Transformation t, Icon icon)
    {
        IUVTransformation uvt = new IconTransformation(icon);

        CCRenderState.setColour(-1);

        centerModels[3].render(t, uvt);
        for (int s = 0; s < 2; s++)
            sideModels[s].render(t, uvt);

    }

    public static void renderItemFlow(BasicPipePart p, Vector3 pos, float frame)
    {
        GL11.glPushMatrix();
        GL11.glDisable(GL11.GL_LIGHTING);

        for (RoutedPayload r : p.itemFlow)
        {
            ForgeDirection dir = r.isEntering ? r.input : r.output;
            
            double prog = r.progress + (r.speed * frame);
            
            double frameX = pos.x + 0.5D;
            double frameY = pos.y + 0.25D;
            double frameZ = pos.z + 0.5D;

            switch (dir) {
            case UP: frameY = (pos.y - 0.25D) + prog; break;
            case DOWN: frameY = (pos.y - 0.25D) + (1.0D - prog); break;
            case SOUTH: frameZ = pos.z + prog; break;
            case NORTH: frameZ = pos.z + (1.0D - prog); break;
            case EAST: frameX = pos.x + prog; break;
            case WEST: frameX = pos.x  + (1.0D - prog); break;
            default:
            }

            doRenderItem(r, frameX, frameY, frameZ);
        }
        GL11.glEnable(GL11.GL_LIGHTING);
        GL11.glPopMatrix();
    }

    private static void doRenderItem(RoutedPayload r, double x, double y, double z)
    {
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
        GL11.glScalef(0.5f, 0.5f, 0.5f);
        RenderUtils.renderBlock(Cuboid6.full, 0, new Translation(-0.5, -0.5, -0.5), null, null);
        restoreRenderState();

        GL11.glPopMatrix();
    }

    private static void prepareRenderState()
    {
        GL11.glEnable(GL11.GL_BLEND);
        GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE);
        GL11.glDisable(GL11.GL_TEXTURE_2D);
        GL11.glDisable(GL11.GL_LIGHTING);
        GL11.glDisable(GL11.GL_CULL_FACE);
        GL11.glDepthMask(false);
        CCRenderState.reset();
        CCRenderState.startDrawing(7);
    }

    private static void restoreRenderState()
    {
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
