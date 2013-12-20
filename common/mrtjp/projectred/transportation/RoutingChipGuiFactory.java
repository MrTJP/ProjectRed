package mrtjp.projectred.transportation;

import java.util.EnumSet;
import java.util.LinkedList;
import java.util.List;

import mrtjp.projectred.core.BasicGuiUtils;
import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.core.inventory.GhostContainer2.SlotExtended;
import mrtjp.projectred.core.inventory.GhostGuiContainer;
import mrtjp.projectred.core.inventory.WidgetButton;
import mrtjp.projectred.core.inventory.WidgetButton.WidgetDotSelector;
import mrtjp.projectred.core.inventory.WidgetButton.WidgetSimpleButton;
import mrtjp.projectred.core.inventory.WidgetHoloSideSelect;
import mrtjp.projectred.core.utils.Pair2;
import mrtjp.projectred.transportation.ItemRoutingChip.EnumRoutingChip;
import mrtjp.projectred.transportation.RoutingChipContainerFactory.ChipGhostContainer;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.client.renderer.texture.TextureMap;
import net.minecraft.inventory.Container;
import net.minecraft.tileentity.TileEntityChest;
import net.minecraft.util.EnumChatFormatting;
import net.minecraft.util.Icon;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.FontUtils;

public class RoutingChipGuiFactory
{
    public static GuiContainer getGui(ChipGhostContainer c, int meta)
    {
        if (meta == EnumRoutingChip.ITEMRESPONDER.ordinal() || meta == EnumRoutingChip.ITEMTERMINATOR.ordinal())
            return new GuiChipItemResponder(c);
        else if (meta == EnumRoutingChip.ITEMEXTRACTOR.ordinal())
            return new GuiChipItemExtractor(c);
        else if (meta == EnumRoutingChip.ITEMBROADCASTER.ordinal())
            return new GuiChipItemBroadcaster(c);
        else if (meta == EnumRoutingChip.ITEMSTOCKKEEPER.ordinal())
            return new GuiChipItemStockKeeper(c);
        else if (meta == EnumRoutingChip.DYNAMICITEMRESPONDER.ordinal())
            return new GuiChipDynamicItemResponder(c);
        else if (meta == EnumRoutingChip.ITEMOVERFLOWRESPONDER.ordinal())
            return new GuiChipItemOverflowResponder(c);

        return null;
    }

    public static abstract class GuiChipContainerWidget<T extends RoutingChipset> extends GhostGuiContainer
    {
        protected static final ResourceLocation RL_chipCont = new ResourceLocation("projectred:textures/gui/chipcontainer.png");
        private ChipGhostContainer chipContainer;
        
        public GuiChipContainerWidget(ChipGhostContainer inventorySlots, GuiScreen previous)
        {
            super(inventorySlots, previous);
            chipContainer = inventorySlots;
        }

        public T getChip()
        {
            return (T) chipContainer.getChip();
        }

        public ChipGhostContainer<T> getCleanContainer()
        {
            return chipContainer.getNewInstance();
        }

        public GuiChipContainerWidget(Container inventorySlots, GuiScreen previous, int x, int y)
        {
            super(inventorySlots, previous, x, y);
        }

        @Override
        public void keyTyped(char par1, int id)
        {
            if (id >= 2 && id <= 10)
            {
                int actualKeyboardButton = id - 1;
                if (actualKeyboardButton == Minecraft.getMinecraft().thePlayer.inventory.currentItem + 1)
                    return;
            }
            else
                super.keyTyped(par1, id);
        }

        @Override
        public void drawBackground()
        {
            drawChipContainerBackground();
            drawChipIcon();
            if (getPreviousScreen() != null)
                drawChipOverlay();
        }
        
        /** Utils **/
        
        public void drawChipContainerBackground()
        {
            CCRenderState.changeTexture(RL_chipCont);
            drawTexturedModalRect(0, 0, 0, 0, xSize, ySize);
        }

        public void drawChipIcon()
        {
            CCRenderState.changeTexture(TextureMap.locationItemsTexture);
            drawTexturedModelRectFromIcon(55, 14, getChip().getChipType().icon, 64, 64);
        }

        public void drawChipOverlay()
        {
            drawGradientRect(5, 5, 171, 80, -1072689136, -804253680);
        }

        /** Abstracts **/
        @Override
        public abstract void actionPerformed(String ident, Object... params);

        @Override
        public abstract void addWidgets();

    }

    private static class GuiChipItemResponder extends GuiChipContainerWidget<RoutingChipset_ItemResponder>
    {
        public GuiChipItemResponder(ChipGhostContainer inv)
        {
            super(inv, null);
        }

        @Override
        public void actionPerformed(String ident, Object... params)
        {
            if (ident.equals("pri"))
            {
                ChipGhostContainer<RoutingChipset_ItemResponder> g = getCleanContainer();
                g.addPlayerInventory(8, 86);
                shiftScreen(new GuiChipItemResponder_Priority(g, this), true);
            }
            else if (ident.equals("filt"))
            {
                ChipGhostContainer<RoutingChipset_ItemResponder> g = getCleanContainer();
                g.addPlayerInventory(8, 86);
                int s = 0;
                for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(20, 15, 3, 3, 0, 0))
                    g.addCustomSlot(new SlotExtended(g.getChip().filter, s++, p.getValue1(), p.getValue2()).setLimit(1).setGhosting(true));
                shiftScreen(new GuiChipItemResponder_Filter(g, this), true);
            }
        }

        @Override
        public void addWidgets()
        {
            add(new WidgetDotSelector(80, 54) {
                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Priority");
                    getChip().addPriorityInfo(list);
                    return list;
                }
            }.setActionCommand("pri"));
            add(new WidgetDotSelector(100, 30) {
                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Filter");
                    getChip().addFilterInfo(list);
                    return list;
                }
            }.setActionCommand("filt"));
        }
    }

    private static class GuiChipItemResponder_Priority extends GuiChipContainerWidget<RoutingChipset_ItemResponder>
    {
        public GuiChipItemResponder_Priority(ChipGhostContainer inv, GuiScreen previous)
        {
            super(inv, previous);
        }

        @Override
        public void drawBackground()
        {
            super.drawBackground();
            FontUtils.drawCenteredString(getChip().preference + "", 88, 38, PRColors.WHITE.rgb);
        }

        @Override
        public void actionPerformed(String ident, Object... params)
        {
            if (ident.equals("prefUP"))
                getChip().prefUp();
            else if (ident.equals("prefDOWN"))
                getChip().prefDown();
        }

        @Override
        public void addWidgets()
        {
            add(new WidgetSimpleButton(82, 22, 12, 12).setText("+").setActionCommand("prefUP"));
            add(new WidgetSimpleButton(82, 50, 12, 12).setText("-").setActionCommand("prefDOWN"));
        }
    }

    private static class GuiChipItemResponder_Filter extends GuiChipContainerWidget<RoutingChipset_ItemResponder>
    {
        public GuiChipItemResponder_Filter(ChipGhostContainer inventorySlots, GuiScreen previous)
        {
            super(inventorySlots, previous);
        }

        @Override
        public void drawBackground()
        {
            super.drawBackground();
            for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(20, 15, 3, 3, 0, 0))
                BasicGuiUtils.drawSlotBackground(mc, p.getValue1() - 1, p.getValue2() - 1);
        }

        @Override
        public void actionPerformed(String ident, Object... params)
        {
            if (ident.equals("ext"))
                getChip().filterExclude = !getChip().filterExclude;
            else if (ident.equals("fuz"))
                getChip().fuzzyMode = !getChip().fuzzyMode;
            else if (ident.equals("fuzD"))
                getChip().shiftFuzzy();
        }

        @Override
        public void addWidgets()
        {
            add(new WidgetButton(130, 16, 14, 14) {
                @Override
                public void drawButton(boolean mouseover)
                {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    drawTexturedModalRect(x, y, getChip().filterExclude ? 1 : 17, 102, 14, 14);
                }

                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Filter mode");
                    list.add(EnumChatFormatting.GRAY + "Items are " + (getChip().filterExclude ? "blacklisted" : "whitelisted"));
                    return list;
                }
            }.setActionCommand("ext"));
            add(new WidgetButton(150, 16, 14, 14) {
                @Override
                public void drawButton(boolean mouseover)
                {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    drawTexturedModalRect(x, y, getChip().fuzzyMode ? 49 : 33, 102, 14, 14);
                }

                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Fuzzy mode");
                    list.add(EnumChatFormatting.GRAY + (getChip().fuzzyMode ? "NBT is ignored" : "NBT is checked"));
                    return list;
                }
            }.setActionCommand("fuz"));
            add(new WidgetButton(130, 35, 20, 20) {
                @Override
                public void drawButton(boolean mouseover)
                {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    int u = getChip().fuzzyDamageMode * 22 + 1;
                    drawTexturedModalRect(x, y, u, 80, 20, 20);
                }

                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Fuzzy tool damage");
                    list.add(EnumChatFormatting.GRAY + "Tools grouped at " + getChip().fuzzyPercent[getChip().fuzzyDamageMode] + "%");
                    return list;
                }
            }.setActionCommand("fuzD"));
        }
    }

    private static class GuiChipItemExtractor extends GuiChipContainerWidget<RoutingChipset_ItemExtractor>
    {
        public GuiChipItemExtractor(ChipGhostContainer inventorySlots)
        {
            super(inventorySlots, null);
        }

        @Override
        public void actionPerformed(String ident, Object... params)
        {
            if (ident.equals("filt"))
            {
                ChipGhostContainer<RoutingChipset_ItemExtractor> g = getCleanContainer();
                g.addPlayerInventory(8, 86);
                int s = 0;
                for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(20, 15, 3, 3, 0, 0))
                    g.addCustomSlot(new SlotExtended(g.getChip().filter, s++, p.getValue1(), p.getValue2()).setLimit(1).setGhosting(true));
                shiftScreen(new GuiChipItemExtractor_Filter(g, this), true);
            }
            else if (ident.equals("sneak"))
            {
                ChipGhostContainer<RoutingChipset_ItemExtractor> g = getCleanContainer();
                g.addPlayerInventory(8, 86);
                shiftScreen(new GuiChipItemExtractor_Orient(g, this), true);
            }
        }

        @Override
        public void addWidgets()
        {
            add(new WidgetDotSelector(100, 54) {
                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Orientation");
                    getChip().addOrientInfo(list);
                    return list;
                }
            }.setActionCommand("sneak"));
            add(new WidgetDotSelector(80, 35) {
                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Filter");
                    getChip().addFilterInfo(list);
                    return list;
                }
            }.setActionCommand("filt"));
        }
    }

    private static class GuiChipItemExtractor_Filter extends GuiChipContainerWidget<RoutingChipset_ItemExtractor>
    {
        public GuiChipItemExtractor_Filter(ChipGhostContainer inventorySlots, GuiScreen previous)
        {
            super(inventorySlots, previous);
        }

        @Override
        public void drawBackground()
        {
            super.drawBackground();

            for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(20, 15, 3, 3, 0, 0))
                BasicGuiUtils.drawSlotBackground(mc, p.getValue1() - 1, p.getValue2() - 1);
        }

        @Override
        public void actionPerformed(String ident, Object... params)
        {
            if (ident.equals("ext"))
                getChip().filterExclude = !getChip().filterExclude;
            else if (ident.equals("fuz"))
                getChip().fuzzyMode = !getChip().fuzzyMode;
            else if (ident.equals("fuzD"))
                getChip().shiftFuzzy();
        }

        @Override
        public void addWidgets()
        {
            add(new WidgetButton(130, 16, 14, 14) {
                @Override
                public void drawButton(boolean mouseover)
                {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    drawTexturedModalRect(x, y, getChip().filterExclude ? 1 : 17, 102, 14, 14);
                }

                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Filter mode");
                    list.add(EnumChatFormatting.GRAY + "Items are " + (getChip().filterExclude ? "blacklisted" : "whitelisted"));
                    return list;
                }
            }.setActionCommand("ext"));
            add(new WidgetButton(150, 16, 14, 14) {
                @Override
                public void drawButton(boolean mouseover)
                {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    drawTexturedModalRect(x, y, getChip().fuzzyMode ? 49 : 33, 102, 14, 14);
                }

                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Fuzzy mode");
                    list.add(EnumChatFormatting.GRAY + (getChip().fuzzyMode ? "NBT is ignored" : "NBT is checked"));
                    return list;
                }
            }.setActionCommand("fuz"));
            add(new WidgetButton(130, 35, 20, 20) {
                @Override
                public void drawButton(boolean mouseover)
                {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    int u = getChip().fuzzyDamageMode * 22 + 1;
                    drawTexturedModalRect(x, y, u, 80, 20, 20);
                }

                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Fuzzy tool damage");
                    list.add(EnumChatFormatting.GRAY + "Tools grouped at " + getChip().fuzzyPercent[getChip().fuzzyDamageMode] + "%");
                    return list;
                }
            }.setActionCommand("fuzD"));
        }
    }

    private static class GuiChipItemExtractor_Orient extends GuiChipContainerWidget<RoutingChipset_ItemExtractor>
    {
        public GuiChipItemExtractor_Orient(ChipGhostContainer inventorySlots, GuiScreen previous)
        {
            super(inventorySlots, previous);
        }

        @Override
        public void actionPerformed(String ident, Object... params)
        {
        }

        private WidgetHoloSideSelect sides = new WidgetHoloSideSelect(20, 20, 50, 50, 40) {
            @Override
            public void onSideChanged()
            {
                sideShifted();
            }
        }.setSideHighlighting(true).setExclusiveSides(true).setObject(new TileEntityChest(1)).setSides(EnumSet.of(ForgeDirection.getOrientation(getChip().extractOrient)));

        private void sideShifted()
        {
            EnumSet<ForgeDirection> dirs = sides.getSides();
            if (dirs.isEmpty())
                getChip().extractOrient = -1;
            else
            {
                for (ForgeDirection dir : ForgeDirection.VALID_DIRECTIONS)
                    if (dirs.contains(dir))
                    {
                        getChip().extractOrient = dir.ordinal();
                        break;
                    }
            }
        }

        @Override
        public void addWidgets()
        {
            add(sides);
        }

        private static final String[] names = new String[] { "bottom", "top", "North", "South", "West", "East" };

        @Override
        public void drawBackground()
        {
            super.drawBackground();

            int xOff = 90;
            fontRenderer.drawString("Extraction is", xOff, 20, PRColors.WHITE.rgb, true);
            if (getChip().extractOrient == -1)
                fontRenderer.drawString("not simulated", xOff, 30, PRColors.WHITE.rgb, true);
            else
            {
                fontRenderer.drawString("simulated from", xOff, 30, PRColors.WHITE.rgb, true);
                fontRenderer.drawString("the " + names[getChip().extractOrient], xOff, 40, PRColors.WHITE.rgb, true);
            }
        }
    }

    private static class GuiChipItemBroadcaster extends GuiChipContainerWidget<RoutingChipset_ItemBroadcaster>
    {
        public GuiChipItemBroadcaster(ChipGhostContainer inventorySlots)
        {
            super(inventorySlots, null);
        }

        @Override
        public void actionPerformed(String ident, Object... params)
        {
            if (ident.equals("filt"))
            {
                ChipGhostContainer<RoutingChipset_ItemBroadcaster> g = getCleanContainer();
                g.addPlayerInventory(8, 86);
                int s = 0;
                for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(20, 15, 3, 3, 0, 0))
                    g.addCustomSlot(new SlotExtended(g.getChip().filter, s++, p.getValue1(), p.getValue2()).setLimit(1).setGhosting(true));
                shiftScreen(new GuiChipItemBroadcaster_Filter(g, this), true);
            }
            else if (ident.equals("sneak"))
            {
                ChipGhostContainer<RoutingChipset_ItemBroadcaster> g = getCleanContainer();
                g.addPlayerInventory(8, 86);
                shiftScreen(new GuiChipItemBroadcaster_Orient(g, this), true);
            }
            else if (ident.equals("pri"))
            {
                ChipGhostContainer<RoutingChipset_ItemBroadcaster> g = getCleanContainer();
                g.addPlayerInventory(8, 86);
                shiftScreen(new GuiChipItemBroadcaster_Priority(g, this), true);
            }
        }

        @Override
        public void addWidgets()
        {
            add(new WidgetDotSelector(76, 51) {
                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Filter");
                    getChip().addFilterInfo(list);
                    return list;
                }
            }.setActionCommand("filt"));

            add(new WidgetDotSelector(89, 33) {
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Priority");
                    getChip().addPriorityInfo(list);
                    return list;
                }
            }.setActionCommand("pri"));

            add(new WidgetDotSelector(105, 47) {
                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Orientation");
                    getChip().addOrientInfo(list);
                    return list;
                }
            }.setActionCommand("sneak"));
        }
    }

    private static class GuiChipItemBroadcaster_Filter extends GuiChipContainerWidget<RoutingChipset_ItemBroadcaster>
    {
        public GuiChipItemBroadcaster_Filter(ChipGhostContainer inventorySlots, GuiScreen previous)
        {
            super(inventorySlots, previous);
        }

        @Override
        public void drawBackground()
        {
            super.drawBackground();
            for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(20, 15, 3, 3, 0, 0))
                BasicGuiUtils.drawSlotBackground(mc, p.getValue1() - 1, p.getValue2() - 1);
        }

        @Override
        public void actionPerformed(String ident, Object... params)
        {
            if (ident.equals("ext"))
                getChip().filterExclude = !getChip().filterExclude;
            else if (ident.equals("hide"))
                getChip().shiftHiding();
        }

        @Override
        public void addWidgets()
        {
            add(new WidgetButton(130, 16, 14, 14) {
                @Override
                public void drawButton(boolean mouseover)
                {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    drawTexturedModalRect(x, y, getChip().filterExclude ? 1 : 17, 102, 14, 14);
                }

                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Filter mode");
                    list.add(EnumChatFormatting.GRAY + "Items are " + (getChip().filterExclude ? "blacklisted" : "whitelisted"));
                    return list;
                }
            }.setActionCommand("ext"));
            add(new WidgetButton(130, 32, 14, 14) {
                @Override
                public void drawButton(boolean mouseover)
                {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    int u = getChip().hideMode * 16 + 1;
                    drawTexturedModalRect(x, y, u, 118, 14, 14);
                }

                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Item hiding");
                    list.add(EnumChatFormatting.GRAY + "Hide " + (getChip().hideMode == 0 ? "nothing" : getChip().hide[getChip().hideMode]));
                    return list;
                }
            }.setActionCommand("hide"));
        }
    }

    private static class GuiChipItemBroadcaster_Orient extends GuiChipContainerWidget<RoutingChipset_ItemBroadcaster>
    {
        public GuiChipItemBroadcaster_Orient(ChipGhostContainer inventorySlots, GuiScreen previous)
        {
            super(inventorySlots, previous);
        }

        @Override
        public void actionPerformed(String ident, Object... params)
        {
        }

        private WidgetHoloSideSelect sides = new WidgetHoloSideSelect(20, 20, 50, 50, 40) {
            @Override
            public void onSideChanged()
            {
                sideShifted();
            }
        }.setSideHighlighting(true).setExclusiveSides(true).setObject(new TileEntityChest(1)).setSides(EnumSet.of(ForgeDirection.getOrientation(getChip().extractOrient)));

        private void sideShifted()
        {
            EnumSet<ForgeDirection> dirs = sides.getSides();
            if (dirs.isEmpty())
                getChip().extractOrient = -1;
            else
            {
                for (ForgeDirection dir : ForgeDirection.VALID_DIRECTIONS)
                    if (dirs.contains(dir))
                    {
                        getChip().extractOrient = dir.ordinal();
                        break;
                    }
            }
        }

        @Override
        public void addWidgets()
        {
            add(sides);
        }

        private static final String[] names = new String[] { "bottom", "top", "North", "South", "West", "East" };

        @Override
        public void drawBackground()
        {
            super.drawBackground();

            int xOff = 90;
            fontRenderer.drawString("Extraction is", xOff, 20, PRColors.WHITE.rgb, true);
            if (getChip().extractOrient == -1)
                fontRenderer.drawString("not simulated", xOff, 30, PRColors.WHITE.rgb, true);
            else
            {
                fontRenderer.drawString("simulated from", xOff, 30, PRColors.WHITE.rgb, true);
                fontRenderer.drawString("the " + names[getChip().extractOrient], xOff, 40, PRColors.WHITE.rgb, true);
            }
        }
    }

    private static class GuiChipItemBroadcaster_Priority extends GuiChipContainerWidget<RoutingChipset_ItemBroadcaster>
    {
        public GuiChipItemBroadcaster_Priority(ChipGhostContainer inventorySlots, GuiScreen previous)
        {
            super(inventorySlots, previous);
        }

        @Override
        public void actionPerformed(String ident, Object... params)
        {
            if (ident.equals("prefUP"))
                getChip().prefUp();
            else if (ident.equals("prefDOWN"))
                getChip().prefDown();
        }

        @Override
        public void addWidgets()
        {
            add(new WidgetSimpleButton(82, 22, 12, 12).setText("+").setActionCommand("prefUP"));
            add(new WidgetSimpleButton(82, 50, 12, 12).setText("-").setActionCommand("prefDOWN"));
        }

        @Override
        public void drawBackground()
        {
            super.drawBackground();
            FontUtils.drawCenteredString(getChip().preference + "", 88, 38, PRColors.WHITE.rgb);
        }

    }

    private static class GuiChipItemStockKeeper extends GuiChipContainerWidget<RoutingChipset_ItemStockKeeper>
    {
        public GuiChipItemStockKeeper(ChipGhostContainer container)
        {
            super(container, null);
        }

        @Override
        public void actionPerformed(String ident, Object... params)
        {
            if (ident.equals("stock"))
            {
                ChipGhostContainer<RoutingChipset_ItemStockKeeper> g = getCleanContainer();
                g.addPlayerInventory(8, 86);
                int s = 0;
                for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(20, 15, 3, 3, 0, 0))
                    g.addCustomSlot(new SlotExtended(g.getChip().filter, s++, p.getValue1(), p.getValue2()).setGhosting(true));
                shiftScreen(new GuiChipItemStockKeeper_Stock(g, this), true);
            }
            else if (ident.equals("mode"))
            {
                ChipGhostContainer<RoutingChipset_ItemStockKeeper> g = getCleanContainer();
                g.addPlayerInventory(8, 86);
                shiftScreen(new GuiChipItemStockKeeper_FillMode(g, this), true);
            }
        }

        @Override
        public void addWidgets()
        {
            add(new WidgetDotSelector(75, 47) {
                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Stock");
                    getChip().addFilterInfo(list);
                    return list;
                }
            }.setActionCommand("stock"));
            add(new WidgetDotSelector(100, 37) {
                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Fill Mode");
                    getChip().addModeInfo(list);
                    return list;
                }
            }.setActionCommand("mode"));
        }
    }

    private static class GuiChipItemStockKeeper_Stock extends GuiChipContainerWidget<RoutingChipset_ItemStockKeeper>
    {
        public GuiChipItemStockKeeper_Stock(ChipGhostContainer inventorySlots, GuiScreen previous)
        {
            super(inventorySlots, previous);
        }

        @Override
        public void actionPerformed(String ident, Object... params)
        {
        }

        @Override
        public void addWidgets()
        {
        }

        @Override
        public void drawBackground()
        {
            super.drawBackground();
            for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(20, 15, 3, 3, 0, 0))
                BasicGuiUtils.drawSlotBackground(mc, p.getValue1() - 1, p.getValue2() - 1);
        }
    }

    private static class GuiChipItemStockKeeper_FillMode extends GuiChipContainerWidget<RoutingChipset_ItemStockKeeper>
    {
        public GuiChipItemStockKeeper_FillMode(ChipGhostContainer inventorySlots, GuiScreen previous)
        {
            super(inventorySlots, previous);
        }

        @Override
        public void actionPerformed(String ident, Object... params)
        {
            if (ident.equals("mode"))
                getChip().requestWhenEmpty = !getChip().requestWhenEmpty;
        }

        @Override
        public void addWidgets()
        {
            add(new WidgetButton(150, 16, 14, 14) {
                @Override
                public void drawButton(boolean mouseover)
                {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    drawTexturedModalRect(x, y, getChip().requestWhenEmpty ? 97 : 81, 102, 14, 14);
                }

                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Fill mode");
                    list.add(EnumChatFormatting.GRAY + (getChip().requestWhenEmpty ? "refill when items empty" : "refill when items missing"));
                    return list;
                }
            }.setActionCommand("mode"));
        }
    }

    private static class GuiChipDynamicItemResponder extends GuiChipContainerWidget<RoutingChipset_DynamicItemResponder>
    {
        public GuiChipDynamicItemResponder(ChipGhostContainer inv)
        {
            super(inv, null);
        }

        @Override
        public void actionPerformed(String ident, Object... params)
        {
            if (ident.equals("pri"))
            {
                ChipGhostContainer<RoutingChipset_DynamicItemResponder> g = getCleanContainer();
                g.addPlayerInventory(8, 86);
                shiftScreen(new GuiChipDynamicItemResponder_Priority(g, this), true);
            }
            else if (ident.equals("filt"))
            {
                ChipGhostContainer<RoutingChipset_DynamicItemResponder> g = getCleanContainer();
                g.addPlayerInventory(8, 86);
                shiftScreen(new GuiChipDynamicItemResponder_Filter(g, this), true);
            }
        }

        @Override
        public void addWidgets()
        {
            add(new WidgetDotSelector(80, 54) {
                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Priority");
                    getChip().addPriorityInfo(list);
                    return list;
                }
            }.setActionCommand("pri"));
            add(new WidgetDotSelector(100, 40) {
                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Filter");
                    getChip().addFilterInfo(list);
                    return list;
                }
            }.setActionCommand("filt"));
        }
    }

    private static class GuiChipDynamicItemResponder_Priority extends GuiChipContainerWidget<RoutingChipset_DynamicItemResponder>
    {
        public GuiChipDynamicItemResponder_Priority(ChipGhostContainer inv, GuiScreen previous)
        {
            super(inv, previous);
        }

        @Override
        public void drawBackground()
        {
            super.drawBackground();
            FontUtils.drawCenteredString(getChip().preference + "", 88, 38, PRColors.WHITE.rgb);
        }

        @Override
        public void actionPerformed(String ident, Object... params)
        {
            if (ident.equals("prefUP"))
                getChip().prefUp();
            else if (ident.equals("prefDOWN"))
                getChip().prefDown();
        }

        @Override
        public void addWidgets()
        {
            add(new WidgetSimpleButton(82, 22, 12, 12).setText("+").setActionCommand("prefUP"));
            add(new WidgetSimpleButton(82, 50, 12, 12).setText("-").setActionCommand("prefDOWN"));
        }
    }

    private static class GuiChipDynamicItemResponder_Filter extends GuiChipContainerWidget<RoutingChipset_DynamicItemResponder>
    {
        public GuiChipDynamicItemResponder_Filter(ChipGhostContainer inventorySlots, GuiScreen previous)
        {
            super(inventorySlots, previous);
        }

        @Override
        public void actionPerformed(String ident, Object... params)
        {
            if (ident.equals("fuz"))
                getChip().fuzzyMode = !getChip().fuzzyMode;
            else if (ident.equals("fuzD"))
                getChip().shiftFuzzy();
        }

        @Override
        public void addWidgets()
        {
            add(new WidgetButton(150, 16, 14, 14) {
                @Override
                public void drawButton(boolean mouseover)
                {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    drawTexturedModalRect(x, y, getChip().fuzzyMode ? 49 : 33, 102, 14, 14);
                }

                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Fuzzy mode");
                    list.add(EnumChatFormatting.GRAY + (getChip().fuzzyMode ? "NBT is ignored" : "NBT is checked"));
                    return list;
                }
            }.setActionCommand("fuz"));
            add(new WidgetButton(130, 35, 20, 20) {
                @Override
                public void drawButton(boolean mouseover)
                {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    int u = getChip().fuzzyDamageMode * 22 + 1;
                    drawTexturedModalRect(x, y, u, 80, 20, 20);
                }

                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Fuzzy tool damage");
                    list.add(EnumChatFormatting.GRAY + "Tools grouped at " + getChip().fuzzyPercent[getChip().fuzzyDamageMode] + "%");
                    return list;
                }
            }.setActionCommand("fuzD"));
        }
    }

    private static class GuiChipItemOverflowResponder extends GuiChipContainerWidget<RoutingChipset_ItemOverflowResponder>
    {
        public GuiChipItemOverflowResponder(ChipGhostContainer inventorySlots)
        {
            super(inventorySlots, null);
        }

        @Override
        public void actionPerformed(String ident, Object... params)
        {
            if (ident.equals("pri"))
            {
                ChipGhostContainer<RoutingChipset_ItemOverflowResponder> g = getCleanContainer();
                g.addPlayerInventory(8, 86);
                shiftScreen(new GuiChipItemOverflowResponder_Priority(g, this), true);
            }
        }

        @Override
        public void addWidgets()
        {
            add(new WidgetDotSelector(100, 37) {
                @Override
                public List<String> getOverlayText()
                {
                    List<String> list = new LinkedList<String>();
                    list.add("Priority");
                    getChip().addPriorityInfo(list);
                    return list;
                }
            }.setActionCommand("pri"));
        }
    }

    private static class GuiChipItemOverflowResponder_Priority extends GuiChipContainerWidget<RoutingChipset_ItemOverflowResponder>
    {
        public GuiChipItemOverflowResponder_Priority(ChipGhostContainer inventorySlots, GuiScreen previous)
        {
            super(inventorySlots, previous);
        }

        @Override
        public void drawBackground()
        {
            super.drawBackground();
            FontUtils.drawCenteredString(getChip().preference + "", 88, 38, PRColors.WHITE.rgb);
        }

        @Override
        public void actionPerformed(String ident, Object... params)
        {
            if (ident.equals("prefUP"))
                getChip().prefUp();
            else if (ident.equals("prefDOWN"))
                getChip().prefDown();
        }

        @Override
        public void addWidgets()
        {
            add(new WidgetSimpleButton(82, 22, 12, 12).setText("+").setActionCommand("prefUP"));
            add(new WidgetSimpleButton(82, 50, 12, 12).setText("-").setActionCommand("prefDOWN"));
        }
    }
}
