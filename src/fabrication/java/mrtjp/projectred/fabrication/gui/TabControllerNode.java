package mrtjp.projectred.fabrication.gui;

import mrtjp.projectred.redui.AbstractGuiNode;

import java.util.LinkedList;
import java.util.List;

import static mrtjp.projectred.fabrication.gui.TabButtonNode.TabState.*;

public class TabControllerNode extends AbstractGuiNode {

    private final List<TabButtonNode> tabButtonList = new LinkedList<>();
    private final List<TabEntry> tabs = new LinkedList<>();

    public void addButtonForTab(IToolbarTab tab) {

        int i = tabs.size();

        TabButtonNode button = tab.createButtonNode();
        button.setPosition(0, (button.getFrame().height() + 1) * tabButtonList.size());
        button.setZPosition(0.1);
        button.setTabState(TabButtonNode.TabState.ALL_CLOSED);
        addChild(button);

        tabs.add(new TabEntry(i, button, tab));
        tabButtonList.add(button);
    }

    public void selectInitialTab(int tabIndex) {
        for (int i = 0; i < tabs.size(); i++) {
            tabs.get(i).buttonNode.setTabState(i == tabIndex ? OPEN : CLOSED);
        }
    }

    public void openTab(IToolbarTab tab) {
        for (TabEntry entry : tabs) {
            if (entry.tab == tab) {
                entry.buttonNode.setTabState(OPEN);
            } else {
                entry.buttonNode.setTabState(CLOSED);
            }
        }
    }

    public void closeTab(IToolbarTab tab) {

        boolean tabsOpen = false;

        for (TabEntry entry : tabs) {
            if (entry.tab == tab) {
                entry.buttonNode.setTabState(CLOSED);
            }

            if (entry.buttonNode.getRenderState() == OPEN) {
                tabsOpen = true;
            }
        }

        if (!tabsOpen) {
            for (TabEntry entry : tabs) entry.buttonNode.setTabState(ALL_CLOSED);
        }
    }

    public void minimizeTab(IToolbarTab tab) {
        boolean tabsOpen = false;

        for (TabEntry entry : tabs) {
            if (entry.tab == tab) {
                entry.buttonNode.setTabState(MINIMIZED);
            }

            if (entry.buttonNode.getRenderState() == OPEN) {
                tabsOpen = true;
            }
        }

        if (!tabsOpen) {
            for (TabEntry entry : tabs) {
                if (entry.tab != tab) entry.buttonNode.setTabState(ALL_CLOSED);
            }
        }
    }

    public void closeAllTabs() {
        for (TabEntry entry : tabs) {
            entry.buttonNode.setTabState(ALL_CLOSED);
        }
    }

    private static class TabEntry {
        public final int i;
        public final TabButtonNode buttonNode;
        public final IToolbarTab tab;

        public TabEntry(int i, TabButtonNode buttonNode, IToolbarTab tab) {
            this.i = i;
            this.buttonNode = buttonNode;
            this.tab = tab;
        }
    }

    public interface IToolbarTab {

        void onTabClosed();

        void onTabOpened();

        void onTabMinimized();

        TabButtonNode createButtonNode();
    }
}
