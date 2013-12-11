package mrtjp.projectred.core.inventory;

import net.minecraft.client.gui.GuiScreen;

public interface IStackableGui
{
    public GuiScreen getPreviousScreen();

    public void prepareReDisplay();
}
