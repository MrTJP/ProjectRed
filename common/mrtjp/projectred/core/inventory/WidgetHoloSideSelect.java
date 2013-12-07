package mrtjp.projectred.core.inventory;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;

import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.core.inventory.ClickRotation.ClickRotationHook;
import mrtjp.projectred.core.utils.DirectionalRayTracer;
import mrtjp.projectred.core.utils.DirectionalRayTracer.HitCoord;
import mrtjp.projectred.core.utils.MathLib;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.texture.TextureManager;
import net.minecraft.client.renderer.texture.TextureMap;
import net.minecraft.client.renderer.tileentity.TileEntityRenderer;
import net.minecraft.tileentity.TileEntity;
import net.minecraftforge.common.ForgeDirection;

import org.lwjgl.input.Mouse;
import org.lwjgl.opengl.GL11;

import codechicken.lib.render.CCModel;
import codechicken.lib.render.Vertex5;
import codechicken.lib.vec.Rotation;

public class WidgetHoloSideSelect extends GhostWidget
{

    private boolean activeHighlight = false;

    private RenderBlocks renderBlocks = new RenderBlocks();
    private ClickRotationHook rotHook = new ClickRotationHook(0, 40);

    private boolean init = false;
    
    private int sides = 0;
    private boolean exclusiveSide = false;
    private ForgeDirection lastSideHovered;
    private int ticksHeld = 0;

    private TileEntity te = null;
    private Block block = null;
    private int meta = 0;
    
    private int color = PRColors.LIME.rgb;
    private double scale;
    

    public WidgetHoloSideSelect(int x, int y, int width, int height, double scale) {
        super(x, y, width, height);
        this.scale = scale;
    }

    public WidgetHoloSideSelect setObject(TileEntity te) {
        this.te = te;
        return this;
    }

    public WidgetHoloSideSelect setObject(Block block, int meta) {
        this.block = block;
        this.meta = meta;
        return this;
    }
    
    public WidgetHoloSideSelect setSideHighlighting(boolean flag) {
        activeHighlight = flag;
        return this;
    }
    
    public WidgetHoloSideSelect setColor(int color) {
        this.color = color;
        return this;
    }
    
    public WidgetHoloSideSelect setExclusiveSides(boolean flag) {
        exclusiveSide = flag;
        return this;
    }

    public WidgetHoloSideSelect setSides(EnumSet<ForgeDirection> dirs) {
        for (ForgeDirection dir : ForgeDirection.VALID_DIRECTIONS)
            if (dirs.contains(dir))
                sides |= 1<<dir.ordinal();
        return this;
    }
    
    @Override
    public void drawBack(int mouseX, int mouseY, float frame) {
        if (init == false || Mouse.isButtonDown(2)) {
            rotHook.setTransform(MathLib.createEntityRotateMatrix(mc.renderViewEntity));
            init = true;
        }

        GL11.glPushMatrix();
        Tessellator t = Tessellator.instance;
        GL11.glTranslated(x + (scale / 2), y + (scale / 2), scale);
        GL11.glScaled(scale, -scale, scale);

        rotHook.update(mouseX - width, -(mouseY - height), pointInside(mouseX, mouseY));
        
        if (te != null)
            TileEntityRenderer.instance.renderTileEntityAt(te, -0.5, -0.5, -0.5, 0.0F);
        else
            drawBlock(mc.renderEngine, t);

        DirectionalRayTracer tracer = new DirectionalRayTracer(0.5);

        HitCoord coord = tracer.getNearestHit();

        if (coord != null && !Mouse.isButtonDown(0))
            drawHighlight(t, coord.side, 0x444444);

        if (activeHighlight) {
            for (ForgeDirection dir : ForgeDirection.VALID_DIRECTIONS)
                if ((sides & 1<<dir.ordinal()) != 0)
                    drawHighlight(t, dir, 0xCC0000);
        }

        lastSideHovered = coord == null ? ForgeDirection.UNKNOWN : coord.side;

        GL11.glPopMatrix();
    }


    static
    {
        genHighlightModel();
    }
    
    private static CCModel[] highlights;
    
    private static void genHighlightModel() {
        CCModel model = CCModel.quadModel(4);
        model.verts[0] = new Vertex5(-0.5, -0.5, -0.5, 0, 0);
        model.verts[1] = new Vertex5(0.5, -0.5, -0.5, 0, 0);
        model.verts[2] = new Vertex5(0.5, -0.5, 0.5, 0, 0);
        model.verts[3] = new Vertex5(-0.5, -0.5, 0.5, 0, 0);
        
        highlights = new CCModel[6];
        highlights[0] = model;
        for (int s = 1; s < 6; s++)
            highlights[s] = model.copy().apply(Rotation.sideRotations[s]);
    }

    private void drawHighlight(Tessellator t, ForgeDirection side2, int i) {
        genHighlightModel();
        GL11.glDisable(GL11.GL_LIGHTING);
        GL11.glEnable(GL11.GL_BLEND);
        GL11.glDisable(GL11.GL_DEPTH_TEST);
        GL11.glDisable(GL11.GL_TEXTURE_2D);
        GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA);
        
        t.startDrawingQuads();
        t.setColorRGBA_I(color, 64);
        highlights[side2.ordinal()].render();
        t.draw();

        GL11.glEnable(GL11.GL_DEPTH_TEST);
        GL11.glEnable(GL11.GL_TEXTURE_2D);
        GL11.glDisable(GL11.GL_BLEND);
    }

    private void drawBlock(TextureManager renderEngine, Tessellator t) {
        GL11.glColor4f(1, 1, 1, 1);
        renderEngine.bindTexture(TextureMap.locationBlocksTexture);
        
        renderBlocks.setRenderBounds(0, 0, 0, 1, 1, 1);
        t.startDrawingQuads();

        renderBlocks.renderFaceXNeg(Block.stone, -0.5, -0.5, -0.5, block.getIcon(4, meta));
        renderBlocks.renderFaceXPos(Block.stone, -0.5, -0.5, -0.5, block.getIcon(5, meta));
        renderBlocks.renderFaceYPos(Block.stone, -0.5, -0.5, -0.5, block.getIcon(1, meta));
        renderBlocks.renderFaceYNeg(Block.stone, -0.5, -0.5, -0.5, block.getIcon(0, meta));
        renderBlocks.renderFaceZNeg(Block.stone, -0.5, -0.5, -0.5, block.getIcon(2, meta));
        renderBlocks.renderFaceZPos(Block.stone, -0.5, -0.5, -0.5, block.getIcon(3, meta));

        t.draw();
    }
    
    @Override
    public void mouseClicked(int x, int y, int button) {
        ticksHeld = 0;
        lastSideHovered = null;
    }
    
    @Override
    public void mouseMovedOrUp(int x, int y, int button) {
        if (button == 0 && ticksHeld < 5 && lastSideHovered != null && lastSideHovered != ForgeDirection.UNKNOWN) {
            toggleSide(lastSideHovered.ordinal());
            ticksHeld = 5;
        }
    }
    
    private void toggleSide(int side) {
        int old = sides;
        
        sides ^= 1 << side;
        
        if (exclusiveSide)
            sides &= 1<<side;
                
        if (old != sides)
            onSideChanged();
    }
    
    public void clearSides() {
        int old = sides;
        sides = 0;
        if (old != sides)
            onSideChanged();
    }
    
    @Override
    public void mouseDragged(int x, int y, int button, long time) {
        ticksHeld++;
    }
    
    public EnumSet<ForgeDirection> getSides() {
        EnumSet<ForgeDirection> set = EnumSet.noneOf(ForgeDirection.class);
        
        for (int i = 0; i < 6; i++)
            if ((sides & 1<<i) != 0)
                set.add(ForgeDirection.getOrientation(i));
        
        return set;
    }
    
    public void onSideChanged() {
    }
}
