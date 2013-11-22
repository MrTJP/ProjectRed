package mrtjp.projectred.expansion;

import mrtjp.projectred.core.BasicGuiUtils;
import mrtjp.projectred.core.inventory.GhostGuiContainer;
import net.minecraft.inventory.Container;
import codechicken.lib.render.CCRenderState;

public class GuiInterfacePipe extends GhostGuiContainer {

    RoutedInterfacePipePart pipe;
    
    public GuiInterfacePipe(Container inventorySlots, RoutedInterfacePipePart pipe) {
        super(inventorySlots, null, 176, 200);
        this.pipe = pipe;
    }

    @Override
    public void actionPerformed(String ident, Object... params) {
    }

    @Override
    public void addWidgets() {
    }
    
    @Override
    public void drawBackground() {
        CCRenderState.changeTexture("projectred:textures/gui/guiinterfacepipe.png");
        
        drawTexturedModalRect(0, 0, 0, 0, xSize, ySize);

        BasicGuiUtils.drawPlayerInventoryBackground(mc, 8, 118);
    }
    
    @Override
    public void drawForeground() {
        CCRenderState.changeTexture("projectred:textures/gui/guiinterfacepipe.png");
        
        float oldZ = zLevel;
        zLevel = 300;
        
        for (int i = 0; i < 4; i++) {
            int x = 19;
            int y = 10+(i*26);
            int u = 178;
            int v = inventorySlots.getSlot(i).getStack() == null ? 107 : 85;
            
            drawTexturedModalRect(x, y, u, v, 25, 20);
        }
        
        zLevel = oldZ;
    }

}
