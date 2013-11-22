package mrtjp.projectred.core.inventory;

import java.util.ArrayList;
import java.util.List;

import mrtjp.projectred.core.utils.ItemKeyStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.renderer.OpenGlHelper;
import net.minecraft.client.renderer.RenderHelper;
import net.minecraft.client.renderer.entity.RenderItem;
import net.minecraft.item.ItemStack;

import org.lwjgl.opengl.GL11;
import org.lwjgl.opengl.GL12;

import codechicken.core.gui.GuiDraw;
import codechicken.lib.render.FontUtils;

public class WidgetItemSelection extends GhostWidget {

    int rows;
    int columns;
    int squareSize = 20;
    int page = 0;
    
    boolean waitingForList = true;
    boolean downloadFinished = false;
    
    private ItemKeyStack selection = null;
    private ItemKeyStack hover = null;
    private int xLast, yLast;
    private String filter = "";
    
    public List<ItemKeyStack> displayList = new ArrayList<ItemKeyStack>();

    public ItemKeyStack getSelection() {
        return selection;
    }
    
    public WidgetItemSelection(int x, int y, int width, int height) {
        super(x, y, width, height);
        rows = height / squareSize;
        columns = width / squareSize;
    }
    
    public void setDisplayList(List<ItemKeyStack> list) {
        displayList = list;
        waitingForList = false;
    }
    
    public void setNewFilter(String filt) {
        filter = filt;
        xLast = yLast = -1;
        page = 0;
    }
    
    public void pageUp() {
        int needed = displayList.size()/(rows*columns);
        page++;
        if (page > needed)
            page = needed;
    }
    
    public void pageDown() {
        page--;
        if (page < 0)
            page = 0;
    }
    
    public void resetDownloadStats() {
        waitingForList = true;
        downloadFinished = false;
    }
    
    private boolean filterAllows(ItemKeyStack stack) {
        if (stringsCanSearch(stack.getName().toLowerCase(), filter.toLowerCase()))
            return true;
        //TODO add id searches
        return false;
    }
    
    private boolean stringsCanSearch(String name, String filter) {
        boolean flag = true;
        
        for (String s : filter.split(" "))
            if (!name.contains(s))
                flag = false;
        
        return flag;
    }
    
    private void drawLoadingScreen() {
        int barSizeX = width/2;
        int percent = ((int) (System.currentTimeMillis()/(waitingForList?40:3)) % barSizeX);
        
        if (!waitingForList && percent == 0)
            downloadFinished = true;
        
        int xStart = x+width/2-(barSizeX/2);
        int yStart = y+height/3;
        
        FontUtils.drawCenteredString("downloading data", (x+width)/2, (y+height)/3+squareSize, 0xff165571);
        
        int xSize = percent;
        int ySize = 9;
        
        this.drawRect(xStart, yStart, xStart+xSize, yStart+ySize, 0xff165571);
    }

    private void drawAllItems(int mx, int my) {
        hover = null;
        selection = null;
        int xOffset = x+2;
        int yOffset = y+2;
        
        int renderPointerX = 1;
        int renderPointerY = 0;
        int itemNumber = 0;
        glItemPre();
        for (ItemKeyStack keystack : displayList) {
            if (!filterAllows(keystack))
                continue;
            
            int minNum = rows * columns * page;
            int maxNum = minNum + (rows * columns);
            if (itemNumber < minNum)
                continue;
            if (itemNumber > maxNum)
                break;
            
            int localX = xOffset + (renderPointerX * squareSize);
            int localY = yOffset + (renderPointerY * squareSize);
            
            if (mx > localX && mx < localX + squareSize && my > localY && my < localY + squareSize)
                hover = keystack;
            
            if (xLast > localX && xLast < localX + squareSize && yLast > localY && yLast < localY + squareSize)
                selection = keystack;
            
            if (selection != null && selection.equals(keystack)) {
                drawRect(localX - 2, localY - 2, localX + squareSize - 2, localY + squareSize - 2, 0xff000000);
                drawRect(localX - 1, localY - 1, localX + squareSize - 3, localY + squareSize - 3, 0xffd2d2d2);
                drawRect(localX, localY, localX + squareSize - 4, localY + squareSize - 4, 0xff595959);
            }
            
            inscribeItemStack(localX, localY, keystack.makeStack());
            
            renderPointerX++;
            if (renderPointerX+1 >= columns) {
                renderPointerX = 1;
                renderPointerY++;
            }
            if (renderPointerY >= rows)
                break;
            itemNumber++;
        }
        glItemPost();
    }
    
    protected RenderItem renderItem = new RenderItem();
    private void inscribeItemStack(int xPos, int yPos, ItemStack stack) {
        FontRenderer font = stack.getItem().getFontRenderer(stack);
        if (font == null)
            font = fontRenderer;

        renderItem.zLevel = 100.0F;
        GL11.glEnable(GL11.GL_DEPTH_TEST);
        GL11.glEnable(GL11.GL_LIGHTING);
        
        renderItem.renderItemAndEffectIntoGUI(font, renderEngine, stack, xPos, yPos);
        renderItem.renderItemOverlayIntoGUI(font, renderEngine, stack, xPos, yPos, "");
        
        GL11.glDisable(GL11.GL_LIGHTING);
        GL11.glDisable(GL11.GL_DEPTH_TEST);
        renderItem.zLevel = 0.0F;
        
        String s;
        if (stack.stackSize == 1)
            s = "";
        else if (stack.stackSize < 1000)
            s = stack.stackSize + "";
        else if (stack.stackSize < 100000)
            s = stack.stackSize / 1000 + "K";
        else if (stack.stackSize < 1000000)
            s = "0M" + stack.stackSize / 100000;
        else
            s = stack.stackSize / 1000000 + "M";
        font.drawStringWithShadow(s, xPos + 19 - 2 - font.getStringWidth(s), yPos + 6 + 3, 16777215);
    }

    private static void glItemPre() {
        GL11.glPushMatrix();

        GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);
        RenderHelper.enableGUIStandardItemLighting();
        GL11.glEnable(GL12.GL_RESCALE_NORMAL);
        OpenGlHelper.setLightmapTextureCoords(OpenGlHelper.lightmapTexUnit, 240 / 1.0F, 240 / 1.0F);
        GL11.glDisable(GL11.GL_DEPTH_TEST);
        GL11.glDisable(GL11.GL_LIGHTING);
    }

    private static void glItemPost() {
        GL11.glEnable(GL11.GL_DEPTH_TEST);
        GL11.glPopMatrix();
    }

    @Override
    public void drawBack(int mousex, int mousey, float frame) {
        drawGradientRect(x, y, x+width, y+height, 0xff808080, 0xff808080);
        if (!downloadFinished)
            drawLoadingScreen();
        else
            drawAllItems(mousex, mousey);
    }

    @Override
    public void drawFront(int mousex, int mousey) {
        if (hover != null)
            GuiDraw.drawMultilineTip(mousex+12, mousey-12, hover.makeStack().getTooltip(Minecraft.getMinecraft().thePlayer, Minecraft.getMinecraft().gameSettings.advancedItemTooltips));
    }
    
    @Override
    public void mouseClicked(int x, int y, int button) {
        if (pointInside(x, y)) {
            xLast = x;
            yLast = y;
        }
    }

}
