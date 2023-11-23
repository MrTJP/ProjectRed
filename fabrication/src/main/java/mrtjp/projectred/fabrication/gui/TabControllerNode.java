package mrtjp.projectred.fabrication.gui;

import mrtjp.projectred.redui.AbstractGuiNode;
import net.covers1624.quack.collection.FastStream;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

import static mrtjp.projectred.fabrication.gui.TabButtonNode.TabState.*;

public class TabControllerNode extends AbstractGuiNode {

    private final List<TabButtonNode> tabButtonList = new LinkedList<>();
    private final List<TabEntry> tabs = new LinkedList<>();

    public void addButtonForTab(IToolbarTab tab) {

        int i = tabs.size();

        TabButtonNode button = tab.createButtonNode();
        button.setZPosition(0.1);
        button.setTabState(TabButtonNode.TabState.ALL_CLOSED);
        addChild(button);

        tabs.add(new TabEntry(i, button, tab));
        tabButtonList.add(button);
    }

    public Optional<IToolbarTab> getSelectedTab() {
        for (TabEntry entry : tabs) {
            TabButtonNode.TabState state = entry.buttonNode.getRenderState();
            if (state == OPEN || state == MINIMIZED) {
                return Optional.ofNullable(entry.tab);
            }
        }
        return Optional.empty();
    }

    public IToolbarTab getTab(int index) {
        return tabs.get(index).tab;
    }

    public void spreadButtonsVertically(int padding) {
        int y = 0;
        for (TabButtonNode button : tabButtonList) {
            button.setPosition(0, y);
            y += button.getFrame().height() + padding;
        }
    }

    public void spreadButtonsHorizontally(int padding) {
        int x = 0;
        for (TabButtonNode button : tabButtonList) {
            button.setPosition(x, 0);
            x += button.getFrame().width() + padding;
        }
    }

    public void selectInitialTab(int tabIndex) {
        openTab(tabs.get(tabIndex).tab);
    }

    public void openTab(IToolbarTab tab) {

        boolean tabsOpen = false;

        for (TabEntry entry : tabs) {
            if (entry.tab == tab) {
                entry.buttonNode.setTabState(OPEN);
                if (!tab.hasBody()) {
                    entry.buttonNode.setTabState(MINIMIZED);
                } else {
                    tabsOpen = true;
                }
            } else {
                entry.buttonNode.setTabState(CLOSED);
            }
        }

        if (!tabsOpen) {
            for (TabEntry entry : tabs) {
                if (entry.tab != tab) entry.buttonNode.setTabState(ALL_CLOSED);
            }
        }
    }

    public void openTab(Predicate<IToolbarTab> selector) {
        for (TabEntry entry : tabs) {
            if (selector.test(entry.tab)) {
                openTab(entry.tab);
                return;
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

        boolean hasBody(); // Tabs without bodies will be immediately minimized after open

        void onTabClosed();

        void onTabOpened();

        void onTabMinimized();

        TabButtonNode createButtonNode();
    }
}
