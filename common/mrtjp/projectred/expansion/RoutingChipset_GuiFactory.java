package mrtjp.projectred.expansion;

import java.util.LinkedList;
import java.util.List;

import mrtjp.projectred.core.BasicGuiUtils;
import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.core.inventory.GhostContainer2.SlotExtended;
import mrtjp.projectred.core.inventory.GhostGuiContainer;
import mrtjp.projectred.core.inventory.WidgetButton;
import mrtjp.projectred.core.inventory.WidgetButton.WidgetDotSelector;
import mrtjp.projectred.core.inventory.WidgetButton.WidgetSimpleButton;
import mrtjp.projectred.core.utils.Pair2;
import mrtjp.projectred.expansion.ItemRoutingChip.EnumRoutingChip;
import mrtjp.projectred.expansion.RoutingChipset_ContainerFactory.ChipGhostContainer;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.client.renderer.texture.TextureMap;
import net.minecraft.inventory.Container;
import net.minecraft.util.EnumChatFormatting;
import net.minecraft.util.Icon;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.FontUtils;

public class RoutingChipset_GuiFactory {

    public static GuiContainer getGui(Container c, int meta) {
        if (meta == 0)
            return new GuiChipItemResponder(c);
        else if (meta == 1)
            return new GuiChipItemExtractor(c);
        else if (meta == 2)
            return new GuiChipItemBroadcaster(c);
        else if (meta == 3)
            return new GuiChipItemStockKeeper(c);
        else if (meta == 4)
            return new GuiChipDynamicItemResponder(c);

        return null;
    }

    public static abstract class GuiChipContainerWidget<T extends RoutingChipset> extends GhostGuiContainer {

        public GuiChipContainerWidget(Container inventorySlots, GuiScreen previous) {
            super(inventorySlots, previous);
        }

        public T getChip() {
            return (T) ((ChipGhostContainer) inventorySlots).getChip();
        }

        public ChipGhostContainer<T> getCleanContainer() {
            return ((ChipGhostContainer<T>) inventorySlots).getNewInstance();
        }

        public GuiChipContainerWidget(Container inventorySlots, GuiScreen previous, int x, int y) {
            super(inventorySlots, previous, x, y);
        }

        @Override
        public void keyTyped(char par1, int id) {
            if (id >= 2 && id <= 10) {
                int actualKeyboardButton = id - 1;
                if (actualKeyboardButton == Minecraft.getMinecraft().thePlayer.inventory.currentItem + 1)
                    return;
            } else
                super.keyTyped(par1, id);
        }

        /** Utils **/

        public void drawChipIcon(Icon icon) {
            CCRenderState.changeTexture("projectred:textures/gui/chipcontainer.png");
            drawTexturedModalRect(0, 0, 0, 0, xSize, ySize);
            CCRenderState.changeTexture(TextureMap.locationItemsTexture);
            drawTexturedModelRectFromIcon(55, 14, icon, 64, 64);
        }

        public void drawChipOverlay() {
            drawGradientRect(5, 5, 171, 80, -1072689136, -804253680);
        }

        /** Abstracts **/
        @Override
        public abstract void actionPerformed(String ident, Object... params);
        @Override
        public abstract void addWidgets();
        @Override
        public abstract void drawBackground();

    }

    private static class GuiChipItemResponder extends GuiChipContainerWidget<RoutingChipset_ItemResponder> {
        public GuiChipItemResponder(Container inv) {
            super(inv, null);
        }

        @Override
        public void actionPerformed(String ident, Object... params) {
            if (ident.equals("pri")) {
                ChipGhostContainer<RoutingChipset_ItemResponder> g = getCleanContainer();
                g.addPlayerInventory(8, 86);
                mc.displayGuiScreen(new GuiChipItemResponder_Priority(g, this));
            } else if (ident.equals("filt")) {
                ChipGhostContainer<RoutingChipset_ItemResponder> g = getCleanContainer();
                int s = 0;
                for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(20, 15, 3, 3, 0, 0))
                    g.addCustomSlot(new SlotExtended(g.getChip().filter, s++, p.getValue1(), p.getValue2()).setLimit(1).setGhosting(true));
                g.addPlayerInventory(8, 86);
                mc.displayGuiScreen(new GuiChipItemResponder_Filter(g, this));
            }
        }

        @Override
        public void addWidgets() {
            add(new WidgetDotSelector(80, 54) {
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Priority");
                    getChip().addPriorityInfo(list);
                    return list;
                }
            }.setActionCommand("pri"));
            add(new WidgetDotSelector(100, 30) {
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Filter");
                    getChip().addFilterInfo(list);
                    return list;
                }
            }.setActionCommand("filt"));
        }

        @Override
        public void drawBackground() {
            drawChipIcon(EnumRoutingChip.ITEMRESPONDER.icon);
        }
    }

    private static class GuiChipItemResponder_Priority extends GuiChipContainerWidget<RoutingChipset_ItemResponder> {

        public GuiChipItemResponder_Priority(Container inv, GuiScreen previous) {
            super(inv, previous);
        }

        @Override
        public void drawBackground() {
            drawChipIcon(EnumRoutingChip.ITEMRESPONDER.icon);
            drawChipOverlay();

            FontUtils.drawCenteredString(getChip().priority.name, 31, 38, PRColors.WHITE.rgb);
            FontUtils.drawCenteredString(getChip().customPriority+"", 146, 38, PRColors.WHITE.rgb);
        }

        @Override
        public void actionPerformed(String ident, Object... params) {
            if (ident.equals("priorityUP"))
                getChip().priorityUp();
            else if (ident.equals("priorityDOWN"))
                getChip().priorityDown();
            else if (ident.equals("customUP"))
                getChip().customUp();
            else if (ident.equals("customDOWN"))
                getChip().customDown();
        }

        @Override
        public void addWidgets() {
            add(new WidgetSimpleButton(13, 22, 35, 12).setText("+").setActionCommand("priorityUP"));
            add(new WidgetSimpleButton(13, 50, 35, 12).setText("-").setActionCommand("priorityDOWN"));
            add(new WidgetSimpleButton(140, 22, 12, 12).setText("+").setActionCommand("customUP"));
            add(new WidgetSimpleButton(140, 50, 12, 12).setText("-").setActionCommand("customDOWN"));
        }

    }

    private static class GuiChipItemResponder_Filter extends GuiChipContainerWidget<RoutingChipset_ItemResponder> {

        public GuiChipItemResponder_Filter(Container inventorySlots, GuiScreen previous) {
            super(inventorySlots, previous);
        }

        @Override
        public void drawBackground() {
            drawChipIcon(EnumRoutingChip.ITEMRESPONDER.icon);
            drawChipOverlay();

            for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(20, 15, 3, 3, 0, 0))
                BasicGuiUtils.drawSlotBackground(mc, p.getValue1()-1, p.getValue2()-1);
        }

        @Override
        public void actionPerformed(String ident, Object... params) {
            if (ident.equals("ext"))
                getChip().filterExclude = !getChip().filterExclude;
            else if (ident.equals("fuz"))
                getChip().fuzzyMode = !getChip().fuzzyMode;
            else if (ident.equals("fuzD"))
                getChip().shiftFuzzy();
        }

        @Override
        public void addWidgets() {
            add(new WidgetButton(130, 16, 14, 14) {
                @Override
                public void drawButton(boolean mouseover) {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    drawTexturedModalRect(x, y, getChip().filterExclude ? 1 : 17, 102, 14, 14);
                }
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Filter mode");
                    list.add(EnumChatFormatting.GRAY + "Items are " + (getChip().filterExclude ? "blacklisted" : "whitelisted"));
                    return list;
                }
            }.setActionCommand("ext"));
            add(new WidgetButton(150, 16, 14, 14) {
                @Override
                public void drawButton(boolean mouseover) {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    drawTexturedModalRect(x, y, getChip().fuzzyMode?49:33, 102, 14, 14);
                }
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Fuzzy mode");
                    list.add(EnumChatFormatting.GRAY + (getChip().fuzzyMode ? "NBT is ignored" : "NBT is checked"));
                    return list;
                }
            }.setActionCommand("fuz"));
            add(new WidgetButton(130, 35, 20, 20) {
                @Override
                public void drawButton(boolean mouseover) {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    int u = getChip().fuzzyDamageMode*22+1;
                    drawTexturedModalRect(x, y, u, 80, 20, 20);
                }
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Fuzzy tool damage");
                    list.add(EnumChatFormatting.GRAY + "Tools grouped at " + getChip().fuzzyPercent[getChip().fuzzyDamageMode] + "%");
                    return list;
                }
            }.setActionCommand("fuzD"));
        }
    }


    private static class GuiChipItemExtractor extends GuiChipContainerWidget<RoutingChipset_ItemExtractor> {

        public GuiChipItemExtractor(Container inventorySlots) {
            super(inventorySlots, null);
        }

        @Override
        public void actionPerformed(String ident, Object... params) {
            if (ident.equals("filt")) {
                ChipGhostContainer<RoutingChipset_ItemExtractor> g = getCleanContainer();
                int s = 0;
                for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(20, 15, 3, 3, 0, 0))
                    g.addCustomSlot(new SlotExtended(g.getChip().filter, s++, p.getValue1(), p.getValue2()).setLimit(1).setGhosting(true));
                g.addPlayerInventory(8, 86);
                mc.displayGuiScreen(new GuiChipItemExtractor_Filter(g, this));
            } else if (ident.equals("sneak")) {
                ChipGhostContainer<RoutingChipset_ItemExtractor> g = getCleanContainer();
                g.addPlayerInventory(8, 86);
                mc.displayGuiScreen(new GuiChipItemExtractor_Orient(g, this));
            }
        }

        @Override
        public void addWidgets() {
            add(new WidgetDotSelector(100, 54) {
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Orientation");
                    getChip().addOrientInfo(list);
                    return list;
                }
            }.setActionCommand("sneak"));
            add(new WidgetDotSelector(80, 35) {
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Filter");
                    getChip().addFilterInfo(list);
                    return list;
                }
            }.setActionCommand("filt"));
        }

        @Override
        public void drawBackground() {
            drawChipIcon(EnumRoutingChip.ITEMEXTRACTOR.icon);
        }
    }

    private static class GuiChipItemExtractor_Filter extends GuiChipContainerWidget<RoutingChipset_ItemExtractor> {

        public GuiChipItemExtractor_Filter(Container inventorySlots, GuiScreen previous) {
            super(inventorySlots, previous);
        }

        @Override
        public void drawBackground() {
            drawChipIcon(EnumRoutingChip.ITEMEXTRACTOR.icon);
            drawChipOverlay();

            for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(20, 15, 3, 3, 0, 0))
                BasicGuiUtils.drawSlotBackground(mc, p.getValue1()-1, p.getValue2()-1);
        }

        @Override
        public void actionPerformed(String ident, Object... params) {
            if (ident.equals("ext"))
                getChip().filterExclude = !getChip().filterExclude;
            else if (ident.equals("fuz"))
                getChip().fuzzyMode = !getChip().fuzzyMode;
            else if (ident.equals("fuzD"))
                getChip().shiftFuzzy();
        }

        @Override
        public void addWidgets() {
            add(new WidgetButton(130, 16, 14, 14) {
                @Override
                public void drawButton(boolean mouseover) {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    drawTexturedModalRect(x, y, getChip().filterExclude ? 1 : 17, 102, 14, 14);
                }
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Filter mode");
                    list.add(EnumChatFormatting.GRAY + "Items are " + (getChip().filterExclude ? "blacklisted" : "whitelisted"));
                    return list;
                }
            }.setActionCommand("ext"));
            add(new WidgetButton(150, 16, 14, 14) {
                @Override
                public void drawButton(boolean mouseover) {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    drawTexturedModalRect(x, y, getChip().fuzzyMode?49:33, 102, 14, 14);
                }
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Fuzzy mode");
                    list.add(EnumChatFormatting.GRAY + (getChip().fuzzyMode ? "NBT is ignored" : "NBT is checked"));
                    return list;
                }
            }.setActionCommand("fuz"));
            add(new WidgetButton(130, 35, 20, 20) {
                @Override
                public void drawButton(boolean mouseover) {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    int u = getChip().fuzzyDamageMode*22+1;
                    drawTexturedModalRect(x, y, u, 80, 20, 20);
                }
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Fuzzy tool damage");
                    list.add(EnumChatFormatting.GRAY + "Tools grouped at " + getChip().fuzzyPercent[getChip().fuzzyDamageMode] + "%");
                    return list;
                }
            }.setActionCommand("fuzD"));
        }
    }

    private static class GuiChipItemExtractor_Orient extends GuiChipContainerWidget<RoutingChipset_ItemExtractor> {

        public GuiChipItemExtractor_Orient(Container inventorySlots, GuiScreen previous) {
            super(inventorySlots, previous);
        }

        @Override
        public void actionPerformed(String ident, Object... params) {
            if (ident.equals("orient"))
                getChip().shiftOrient();
        }

        private static final String[] names = new String[] {"bottom", "top", "North", "South", "West", "East"};
        @Override
        public void addWidgets() {
            add(new WidgetSimpleButton(75, 22, 14, 14) {
                @Override
                public void drawButton(boolean mouseover) {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    drawTexturedModalRect(x, y, 65, 102, 14, 14);
                }
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Extract Orientation");
                    list.add(EnumChatFormatting.GRAY +
                            "Simulated from "
                            + (getChip().extractOrient == -1 ? "Default" : names[getChip().extractOrient]));
                    return list;
                }
            }.setActionCommand("orient"));
        }

        @Override
        public void drawBackground() {
            drawChipIcon(EnumRoutingChip.ITEMEXTRACTOR.icon);
            drawChipOverlay();

            CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");

            //TODO icon for dir
        }
    }

    private static class GuiChipItemBroadcaster extends GuiChipContainerWidget<RoutingChipset_ItemBroadcaster> {

        public GuiChipItemBroadcaster(Container inventorySlots) {
            super(inventorySlots, null);
        }

        @Override
        public void actionPerformed(String ident, Object... params) {
            if (ident.equals("filt")) {
                ChipGhostContainer<RoutingChipset_ItemBroadcaster> g = getCleanContainer();
                int s = 0;
                for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(20, 15, 3, 3, 0, 0))
                    g.addCustomSlot(new SlotExtended(g.getChip().filter, s++, p.getValue1(), p.getValue2()).setLimit(1).setGhosting(true));
                g.addPlayerInventory(8, 86);
                mc.displayGuiScreen(new GuiChipItemBroadcaster_Filter(g, this));
            } else if (ident.equals("sneak")) {
                ChipGhostContainer<RoutingChipset_ItemBroadcaster> g = getCleanContainer();
                g.addPlayerInventory(8, 86);
                mc.displayGuiScreen(new GuiChipItemBroadcaster_Orient(g, this));
            }
        }

        @Override
        public void addWidgets() {
            add(new WidgetDotSelector(100, 47) {
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Orientation");
                    getChip().addOrientInfo(list);
                    return list;
                }
            }.setActionCommand("sneak"));
            add(new WidgetDotSelector(80, 37) {
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Filter");
                    getChip().addFilterInfo(list);
                    return list;
                }
            }.setActionCommand("filt"));
        }

        @Override
        public void drawBackground() {
            drawChipIcon(EnumRoutingChip.ITEMBROADCASTER.icon);
        }
    }

    private static class GuiChipItemBroadcaster_Filter extends GuiChipContainerWidget<RoutingChipset_ItemBroadcaster> {

        public GuiChipItemBroadcaster_Filter(Container inventorySlots, GuiScreen previous) {
            super(inventorySlots, previous);
        }

        @Override
        public void drawBackground() {
            drawChipIcon(EnumRoutingChip.ITEMBROADCASTER.icon);
            drawChipOverlay();

            for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(20, 15, 3, 3, 0, 0))
                BasicGuiUtils.drawSlotBackground(mc, p.getValue1()-1, p.getValue2()-1);
        }

        @Override
        public void actionPerformed(String ident, Object... params) {
            if (ident.equals("ext"))
                getChip().filterExclude = !getChip().filterExclude;
            else if (ident.equals("hide"))
                getChip().shiftHiding();
        }

        @Override
        public void addWidgets() {
            add(new WidgetButton(130, 16, 14, 14) {
                @Override
                public void drawButton(boolean mouseover) {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    drawTexturedModalRect(x, y, getChip().filterExclude ? 1 : 17, 102, 14, 14);
                }
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Filter mode");
                    list.add(EnumChatFormatting.GRAY + "Items are " + (getChip().filterExclude ? "blacklisted" : "whitelisted"));
                    return list;
                }
            }.setActionCommand("ext"));
            add(new WidgetButton(130, 32, 14, 14) {
                @Override
                public void drawButton(boolean mouseover) {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    int u = getChip().hideMode*16+1;
                    drawTexturedModalRect(x, y, u, 118, 14, 14);
                }
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Item hiding");
                    list.add(EnumChatFormatting.GRAY + "Hide " + (getChip().hideMode == 0 ? "nothing" : getChip().hide[getChip().hideMode]));
                    return list;
                }
            }.setActionCommand("hide"));
        }
    }

    private static class GuiChipItemBroadcaster_Orient extends GuiChipContainerWidget<RoutingChipset_ItemBroadcaster> {

        public GuiChipItemBroadcaster_Orient(Container inventorySlots, GuiScreen previous) {
            super(inventorySlots, previous);
        }

        @Override
        public void actionPerformed(String ident, Object... params) {
            if (ident.equals("orient"))
                getChip().shiftOrient();
        }

        private static final String[] names = new String[] {"bottom", "top", "North", "South", "West", "East"};
        @Override
        public void addWidgets() {
            add(new WidgetSimpleButton(75, 22, 14, 14) {
                @Override
                public void drawButton(boolean mouseover) {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    drawTexturedModalRect(x, y, 65, 102, 14, 14);
                }
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Extract Orientation");
                    list.add(EnumChatFormatting.GRAY +
                            "Simulated from "
                            + (getChip().extractOrient == -1 ? "Default" : names[getChip().extractOrient]));
                    return list;
                }
            }.setActionCommand("orient"));
        }

        @Override
        public void drawBackground() {
            drawChipIcon(EnumRoutingChip.ITEMBROADCASTER.icon);
            drawChipOverlay();

            CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");

            //TODO icon for dir
        }
    }

    private static class GuiChipItemStockKeeper extends GuiChipContainerWidget<RoutingChipset_ItemStockKeeper> {
        public GuiChipItemStockKeeper(Container container) {
            super(container, null);
        }

        @Override
        public void actionPerformed(String ident, Object... params) {
            if (ident.equals("stock")) {
                ChipGhostContainer<RoutingChipset_ItemStockKeeper> g = getCleanContainer();
                int s = 0;
                for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(20, 15, 3, 3, 0, 0))
                    g.addCustomSlot(new SlotExtended(g.getChip().filter, s++, p.getValue1(), p.getValue2()).setGhosting(true));
                g.addPlayerInventory(8, 86);
                mc.displayGuiScreen(new GuiChipItemStockKeeper_Stock(g, this));
            } else if (ident.equals("mode")) {
                ChipGhostContainer<RoutingChipset_ItemStockKeeper> g = getCleanContainer();
                g.addPlayerInventory(8, 86);
                mc.displayGuiScreen(new GuiChipItemStockKeeper_FillMode(g, this));
            }
        }

        @Override
        public void addWidgets() {
            add(new WidgetDotSelector(75, 47) {
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Stock");
                    getChip().addFilterInfo(list);
                    return list;
                }
            }.setActionCommand("stock"));
            add(new WidgetDotSelector(100, 37) {
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Fill Mode");
                    getChip().addModeInfo(list);
                    return list;
                }
            }.setActionCommand("mode"));
        }

        @Override
        public void drawBackground() {
            drawChipIcon(EnumRoutingChip.ITEMSTOCKKEEPER.icon);
        }
    }

    private static class GuiChipItemStockKeeper_Stock extends GuiChipContainerWidget<RoutingChipset_ItemStockKeeper> {

        public GuiChipItemStockKeeper_Stock(Container inventorySlots, GuiScreen previous) {
            super(inventorySlots, previous);
        }

        @Override
        public void actionPerformed(String ident, Object... params) {
        }

        @Override
        public void addWidgets() {
        }

        @Override
        public void drawBackground() {
            drawChipIcon(EnumRoutingChip.ITEMSTOCKKEEPER.icon);
            drawChipOverlay();

            for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(20, 15, 3, 3, 0, 0))
                BasicGuiUtils.drawSlotBackground(mc, p.getValue1()-1, p.getValue2()-1);
        }
    }

    private static class GuiChipItemStockKeeper_FillMode extends GuiChipContainerWidget<RoutingChipset_ItemStockKeeper> {

        public GuiChipItemStockKeeper_FillMode(Container inventorySlots, GuiScreen previous) {
            super(inventorySlots, previous);
        }

        @Override
        public void actionPerformed(String ident, Object... params) {
            if (ident.equals("mode"))
                getChip().requestWhenEmpty = !getChip().requestWhenEmpty;
        }

        @Override
        public void addWidgets() {
            add(new WidgetButton(150, 16, 14, 14) {
                @Override
                public void drawButton(boolean mouseover) {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    drawTexturedModalRect(x, y, getChip().requestWhenEmpty?97:81, 102, 14, 14);
                }
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Fill mode");
                    list.add(EnumChatFormatting.GRAY +
                            (getChip().requestWhenEmpty ? "refill when items empty" : "refill when items missing"));
                    return list;
                }
            }.setActionCommand("mode"));
        }

        @Override
        public void drawBackground() {
            drawChipIcon(EnumRoutingChip.ITEMSTOCKKEEPER.icon);
            drawChipOverlay();
        }
    }

    private static class GuiChipDynamicItemResponder extends GuiChipContainerWidget<RoutingChipset_DynamicItemResponder> {
        public GuiChipDynamicItemResponder(Container inv) {
            super(inv, null);
        }

        @Override
        public void actionPerformed(String ident, Object... params) {
            if (ident.equals("pri")) {
                ChipGhostContainer<RoutingChipset_DynamicItemResponder> g = getCleanContainer();
                g.addPlayerInventory(8, 86);
                mc.displayGuiScreen(new GuiChipDynamicItemResponder_Priority(g, this));
            }  else if (ident.equals("filt")) {
                ChipGhostContainer<RoutingChipset_DynamicItemResponder> g = getCleanContainer();
                g.addPlayerInventory(8, 86);
                mc.displayGuiScreen(new GuiChipDynamicItemResponder_Filter(g, this));
            }
        }

        @Override
        public void addWidgets() {
            add(new WidgetDotSelector(80, 54) {
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Priority");
                    getChip().addPriorityInfo(list);
                    return list;
                }
            }.setActionCommand("pri"));
            add(new WidgetDotSelector(100, 40) {
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Filter");
                    getChip().addFilterInfo(list);
                    return list;
                }
            }.setActionCommand("filt"));
        }

        @Override
        public void drawBackground() {
            drawChipIcon(EnumRoutingChip.DYNAMICITEMRESPONDER.icon);
        }
    }

    private static class GuiChipDynamicItemResponder_Priority extends GuiChipContainerWidget<RoutingChipset_DynamicItemResponder> {

        public GuiChipDynamicItemResponder_Priority(Container inv, GuiScreen previous) {
            super(inv, previous);
        }

        @Override
        public void drawBackground() {
            drawChipIcon(EnumRoutingChip.DYNAMICITEMRESPONDER.icon);
            drawChipOverlay();

            FontUtils.drawCenteredString(getChip().priority.name, 31, 38, PRColors.WHITE.rgb);
            FontUtils.drawCenteredString(getChip().customPriority+"", 146, 38, PRColors.WHITE.rgb);
        }

        @Override
        public void actionPerformed(String ident, Object... params) {
            if (ident.equals("priorityUP"))
                getChip().priorityUp();
            else if (ident.equals("priorityDOWN"))
                getChip().priorityDown();
            else if (ident.equals("customUP"))
                getChip().customUp();
            else if (ident.equals("customDOWN"))
                getChip().customDown();
        }

        @Override
        public void addWidgets() {
            add(new WidgetSimpleButton(13, 22, 35, 12).setText("+").setActionCommand("priorityUP"));
            add(new WidgetSimpleButton(13, 50, 35, 12).setText("-").setActionCommand("priorityDOWN"));
            add(new WidgetSimpleButton(140, 22, 12, 12).setText("+").setActionCommand("customUP"));
            add(new WidgetSimpleButton(140, 50, 12, 12).setText("-").setActionCommand("customDOWN"));
        }
    }

    private static class GuiChipDynamicItemResponder_Filter extends GuiChipContainerWidget<RoutingChipset_DynamicItemResponder> {

        public GuiChipDynamicItemResponder_Filter(Container inventorySlots, GuiScreen previous) {
            super(inventorySlots, previous);
        }

        @Override
        public void drawBackground() {
            drawChipIcon(EnumRoutingChip.DYNAMICITEMRESPONDER.icon);
            drawChipOverlay();
        }

        @Override
        public void actionPerformed(String ident, Object... params) {
            if (ident.equals("fuz"))
                getChip().fuzzyMode = !getChip().fuzzyMode;
            else if (ident.equals("fuzD"))
                getChip().shiftFuzzy();
        }

        @Override
        public void addWidgets() {
            add(new WidgetButton(150, 16, 14, 14) {
                @Override
                public void drawButton(boolean mouseover) {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    drawTexturedModalRect(x, y, getChip().fuzzyMode?49:33, 102, 14, 14);
                }
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Fuzzy mode");
                    list.add(EnumChatFormatting.GRAY + (getChip().fuzzyMode ? "NBT is ignored" : "NBT is checked"));
                    return list;
                }
            }.setActionCommand("fuz"));
            add(new WidgetButton(130, 35, 20, 20) {
                @Override
                public void drawButton(boolean mouseover) {
                    CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
                    int u = getChip().fuzzyDamageMode*22+1;
                    drawTexturedModalRect(x, y, u, 80, 20, 20);
                }
                @Override
                public List<String> getOverlayText() {
                    List<String> list = new LinkedList<String>();
                    list.add("Fuzzy tool damage");
                    list.add(EnumChatFormatting.GRAY + "Tools grouped at " + getChip().fuzzyPercent[getChip().fuzzyDamageMode] + "%");
                    return list;
                }
            }.setActionCommand("fuzD"));
        }
    }
}
