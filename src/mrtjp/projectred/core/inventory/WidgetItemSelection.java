package mrtjp.projectred.core.inventory;

import codechicken.core.gui.GuiDraw;
import codechicken.lib.render.FontUtils;
import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.core.utils.ItemKeyStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.Gui;
import net.minecraft.client.renderer.OpenGlHelper;
import net.minecraft.client.renderer.RenderHelper;
import net.minecraft.client.renderer.entity.RenderItem;
import net.minecraft.item.ItemStack;
import org.lwjgl.opengl.GL11;
import org.lwjgl.opengl.GL12;

import java.util.ArrayList;
import java.util.List;

public class WidgetItemSelection extends GhostWidget
{
    int rows;
    int columns;
    int squareSize = 20;
    int currentPage = 0;
    int pagesNeeded = 0;

    boolean waitingForList = true;
    boolean downloadFinished = false;

    private ItemKeyStack selection = null;
    private ItemKeyStack hover = null;
    private int xLast, yLast;
    private String filter = "";

    public List<ItemKeyStack> displayList = new ArrayList<ItemKeyStack>();

    public ItemKeyStack getSelection()
    {
        return selection;
    }

    public WidgetItemSelection(int x, int y, int width, int height)
    {
        super(x, y, width, height);
        rows = height / squareSize;
        columns = width / squareSize;
    }

    public void setDisplayList(List<ItemKeyStack> list)
    {
        displayList = list;
        waitingForList = false;
        currentPage = 0;
    }

    public void setNewFilter(String filt)
    {
        filter = filt;
        xLast = yLast = -1;
        currentPage = 0;
    }

    public void pageUp()
    {
        currentPage++;
        if (currentPage > pagesNeeded)
            currentPage = pagesNeeded;
    }

    public void pageDown()
    {
        currentPage--;
        if (currentPage < 0)
            currentPage = 0;
    }

    public void resetDownloadStats()
    {
        waitingForList = true;
        downloadFinished = false;
    }

    private boolean filterAllows(ItemKeyStack stack)
    {
        if (stringsCanSearch(stack.key().getName().toLowerCase(), filter.toLowerCase()))
            return true;
        // TODO Add more conditions to the filter, such as ID searching, etc.
        return false;
    }

    private int getSeachedCount()
    {
        int count = 0;
        for (ItemKeyStack stack : displayList)
            if (filterAllows(stack))
                count++;
        return count;
    }

    private boolean stringsCanSearch(String name, String filter)
    {
        boolean flag = true;

        for (String s : filter.split(" "))
            if (!name.contains(s))
                flag = false;

        return flag;
    }

    private void drawLoadingScreen()
    {
        int barSizeX = width()/2;
        long time = System.currentTimeMillis() / (waitingForList ? 40 : 8);
        int percent = (int) (time % barSizeX);

        if (!waitingForList && percent > barSizeX - 8)
            downloadFinished = true;

        int xStart = x()+width()/2-barSizeX/2;
        int yStart = y()+height()/3;

        FontUtils.drawCenteredString("downloading data", (x()+width()) / 2, (y()+height()) / 3 + squareSize, 0xff165571);

        int xSize = percent;
        int ySize = 9;

        Gui.drawRect(xStart, yStart, xStart + xSize, yStart + ySize, 0xff165571);
    }

    private void drawAllItems(int mx, int my)
    {
        hover = null;
        selection = null;
        int xOffset = x()-(squareSize - 2);
        int yOffset = y()+2;

        int renderPointerX = 1;
        int renderPointerY = 0;
        int itemNumber = 0;
        glItemPre();
        for (ItemKeyStack keystack : displayList)
        {
            if (!filterAllows(keystack))
                continue;

            itemNumber++;
            if (itemNumber <= rows * columns * currentPage)
                continue;
            if (itemNumber > (rows * columns) * (currentPage + 1))
                break;

            int localX = xOffset + renderPointerX * squareSize;
            int localY = yOffset + renderPointerY * squareSize;

            if (mx > localX && mx < localX + squareSize && my > localY && my < localY + squareSize)
                hover = keystack;

            if (xLast > localX && xLast < localX + squareSize && yLast > localY && yLast < localY + squareSize)
                selection = keystack;

            if (selection != null && selection.equals(keystack))
            {
                drawRect(localX - 2, localY - 2, localX + squareSize - 2, localY + squareSize - 2, 0xff000000);
                drawRect(localX - 1, localY - 1, localX + squareSize - 3, localY + squareSize - 3, 0xffd2d2d2);
                drawRect(localX, localY, localX + squareSize - 4, localY + squareSize - 4, 0xff595959);
            }

            inscribeItemStack(localX, localY, keystack.makeStack());

            renderPointerX++;
            if (renderPointerX > columns)
            {
                renderPointerX = 1;
                renderPointerY++;
            }
            if (renderPointerY > rows)
                break;
        }
        glItemPost();
    }

    protected RenderItem renderItem = new RenderItem();

    private void inscribeItemStack(int xPos, int yPos, ItemStack stack)
    {
        FontRenderer font = stack.getItem().getFontRenderer(stack);
        if (font == null)
            font = fontRenderer();

        renderItem.zLevel = 100.0F;
        GL11.glEnable(GL11.GL_DEPTH_TEST);
        GL11.glEnable(GL11.GL_LIGHTING);

        renderItem.renderItemAndEffectIntoGUI(font, renderEngine(), stack, xPos, yPos);
        renderItem.renderItemOverlayIntoGUI(font, renderEngine(), stack, xPos, yPos, "");

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

    private static void glItemPre()
    {
        GL11.glPushMatrix();

        GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);
        RenderHelper.enableGUIStandardItemLighting();
        GL11.glEnable(GL12.GL_RESCALE_NORMAL);
        OpenGlHelper.setLightmapTextureCoords(OpenGlHelper.lightmapTexUnit, 240 / 1.0F, 240 / 1.0F);
        GL11.glDisable(GL11.GL_DEPTH_TEST);
        GL11.glDisable(GL11.GL_LIGHTING);
    }

    private static void glItemPost()
    {
        GL11.glEnable(GL11.GL_DEPTH_TEST);
        GL11.glPopMatrix();
    }

    @Override
    public void drawBack(int mousex, int mousey, float frame)
    {
        drawGradientRect(x(), y(), x()+width(), y()+height(), 0xff808080, 0xff808080);
        pagesNeeded = (getSeachedCount() - 1) / (rows * columns);
        if (pagesNeeded < 0)
            pagesNeeded = 0;

        if (currentPage > pagesNeeded)
            currentPage = pagesNeeded;

        if (!downloadFinished)
            drawLoadingScreen();
        else
            drawAllItems(mousex, mousey);
    }

    @Override
    public void drawFront(int mousex, int mousey)
    {
        if (hover != null)
            GuiDraw.drawMultilineTip(mousex + 12, mousey - 12, hover.makeStack().getTooltip(Minecraft.getMinecraft().thePlayer, Minecraft.getMinecraft().gameSettings.advancedItemTooltips));

        FontUtils.drawCenteredString("Page: " + (currentPage + 1) + "/" + (pagesNeeded + 1), x()+(width()/2), y()+height() + 6, PRColors.BLACK.rgb);
    }

    @Override
    public void mouseClicked(int x, int y, int button)
    {
        if (pointInside(x, y))
        {
            xLast = x;
            yLast = y;
        }
    }
}
