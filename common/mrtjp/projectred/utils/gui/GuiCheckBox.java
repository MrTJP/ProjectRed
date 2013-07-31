package mrtjp.projectred.utils.gui;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiButton;
import net.minecraft.client.renderer.Tessellator;

import org.lwjgl.opengl.GL11;

public class GuiCheckBox extends GuiButton {
	
	private boolean state = false;
	
	public GuiCheckBox(int par1, int par2, int par3, int par4, int par5, boolean startState) {
		super(par1, par2, par3, par4, par5, "");
		state = startState;
	}

    /**
     * Draws this button to the screen.
     */
    @Override
	public void drawButton(Minecraft minecraft, int par2, int par3)
    {
        if (this.drawButton)
        {
            boolean var5 = par2 >= this.xPosition && par3 >= this.yPosition && par2 < this.xPosition + this.width && par3 < this.yPosition + this.height;
            int var6 = this.getHoverState(var5);
            GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);
            Tessellator var9 = Tessellator.instance;
            var9.startDrawingQuads();
            var9.addVertexWithUV(xPosition			, yPosition + height	, zLevel, 0	, 1);
            var9.addVertexWithUV(xPosition + width	, yPosition + height	, zLevel, 1	, 1);
            var9.addVertexWithUV(xPosition + width	, yPosition				, zLevel, 1	, 0);
            var9.addVertexWithUV(xPosition			, yPosition				, zLevel, 0	, 0);
            var9.draw();
/*
            drawTexturedModalRect(xPosition  			, yPosition				, 0		, 0 ,0);
            drawTexturedModalRect(xPosition + width / 2	, yPosition				, 0		, 1, 0);
            
            drawTexturedModalRect(xPosition  			, yPosition + height / 2, 0		, 0 ,1);
            drawTexturedModalRect(xPosition + width / 2	, yPosition + height / 2, 0		, 1, 1);
*/
            mouseDragged(minecraft, par2, par3);
        }
    }

	public boolean change() {
		return state = !state;
	}
	
	public boolean getState() {
		return state;
	}
	
	public void setState(boolean flag) {
		state = flag;
	}
}
