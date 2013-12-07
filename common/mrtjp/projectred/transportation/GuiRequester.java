package mrtjp.projectred.transportation;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import mrtjp.projectred.core.BasicGuiUtils;
import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.core.inventory.GhostGuiScreen;
import mrtjp.projectred.core.inventory.WidgetButton.WidgetCheckBox;
import mrtjp.projectred.core.inventory.WidgetButton.WidgetSimpleButton;
import mrtjp.projectred.core.inventory.WidgetItemSelection;
import mrtjp.projectred.core.inventory.WidgetTextBox;
import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.core.utils.ItemKeyStack;

import org.lwjgl.input.Keyboard;

import codechicken.lib.packet.PacketCustom;
import codechicken.lib.vec.BlockCoord;

public class GuiRequester extends GhostGuiScreen {

    IWorldRequester pipe;

    public GuiRequester(IWorldRequester pipe) {
        super(280, 230);
        this.pipe = pipe;
    }

    WidgetItemSelection itemList = new WidgetItemSelection(xSize/2-260/2, 10, 260, 140);
    WidgetTextBox textFilter = new WidgetTextBox(xSize/2-150/2, 185, 150, 16, "") {
        @Override
        public void onTextChanged(String oldText) {
            itemList.setNewFilter(getText());
        }
    }.setMaxStringLength(24);

    WidgetTextBox itemCount = new WidgetTextBox(xSize/2-50/2, 205, 50, 16, "1") {
        @Override
        public void mouseScrolled(int x, int y, int scroll) {
            if (pointInside(x, y)) {
                if (scroll > 0)
                    countUp();
                else if (scroll < 0)
                    countDown();
            }
        }
        
        @Override
        public void onFocusChanged() {
            if (getText() == null || getText().isEmpty()) {
                setText("1");
            }
        };
    }.setAllowedCharacters("0123456789").setMaxStringLength(7);

    WidgetCheckBox pull = new WidgetCheckBox(230, 170, true) {
        @Override
        public void onStateChanged(boolean oldState) {
            itemList.resetDownloadStats();
            askForListRefresh();
        }
    };
    WidgetCheckBox craft = new WidgetCheckBox(230, 190, true) {
        @Override
        public void onStateChanged(boolean oldState) {
            itemList.resetDownloadStats();
            askForListRefresh();
        }
    };
    WidgetCheckBox partials = new WidgetCheckBox(230, 210, false);
    
    @Override
    public void drawBackground() {
        BasicGuiUtils.drawGuiBackGround(mc, 0, 0, xSize, ySize, zLevel, true);
    }

    @Override
    public void drawForeground() {
        fontRenderer.drawStringWithShadow("Pull", 240, 166, PRColors.WHITE.rgb);
        fontRenderer.drawStringWithShadow("Craft", 240, 186, PRColors.WHITE.rgb);
        fontRenderer.drawStringWithShadow("Parials", 240, 206, PRColors.WHITE.rgb);
    }

    @Override
    public void addWidgets() {
        add(itemList);
        add(textFilter);
        add(itemCount);
        
        add(pull);
        add(craft);
        add(partials);

        // Submit and refresh
        add(new WidgetSimpleButton(10, 185, 50, 16).setActionCommand("refrsh").setText("Re-poll"));
        add(new WidgetSimpleButton(10, 205, 50, 16).setActionCommand("req").setText("Submit"));
        // Count + -
        add(new WidgetSimpleButton(95, 205, 16, 16).setActionCommand("-").setText("-"));
        add(new WidgetSimpleButton(170, 205, 16, 16).setActionCommand("+").setText("+"));
        // Page + -
        add(new WidgetSimpleButton(85, 152, 16, 16).setActionCommand("p-").setText("-"));
        add(new WidgetSimpleButton(180, 152, 16, 16).setActionCommand("p+").setText("+"));

        askForListRefresh();
    }

    private void sendItemRequest() {
        String count = itemCount.getText();

        if (count == null || count.isEmpty())
            return;

        int amount = Integer.parseInt(count);
        if (amount <= 0)
            return;

        ItemKeyStack request = itemList.getSelection();
        if (request != null) {
            PacketCustom packet = new PacketCustom(TransportationSPH.channel, NetConstants.gui_Request_submit);
            packet.writeCoord(new BlockCoord(pipe.getContainer().tile()));
            packet.writeBoolean(pull.isChecked());
            packet.writeBoolean(craft.isChecked());
            packet.writeBoolean(partials.isChecked());
            packet.writeItemStack(request.key().makeStack(amount), true);
            packet.sendToServer();
        }
    }

    private void askForListRefresh() {
        PacketCustom packet = new PacketCustom(TransportationSPH.channel, NetConstants.gui_Request_listRefresh);
        packet.writeCoord(new BlockCoord(pipe.getContainer().tile()));
        packet.writeBoolean(pull.isChecked());
        packet.writeBoolean(craft.isChecked());
        packet.sendToServer();
    }

    private void sendAction(String ident) {
        PacketCustom packet = new PacketCustom(TransportationSPH.channel, NetConstants.gui_Request_action);
        packet.writeCoord(new BlockCoord(pipe.getContainer().tile()));
        packet.writeString(ident);
        packet.sendToServer();
    }
    
    private void countUp() {
        int current = 0;
        String s = itemCount.getText();
        if (s != null && !s.isEmpty())
            current = Integer.parseInt(s);
        int newCount = 0; 
        
        if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT))
            newCount = current + 10;
        else
            newCount = current + 1;

        if (String.valueOf(newCount).length() <= itemCount.maxStringLength)
            itemCount.setText(String.valueOf(newCount));
    }
    
    private void countDown() {
        int current = 0;
        String s = itemCount.getText();
        if (s != null && !s.isEmpty())
            current = Integer.parseInt(s);
        int newCount = 0; 
        
        if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT))
            newCount = current - 10;
        else
            newCount = current - 1;
        newCount = Math.max(1, newCount);
        
        if (String.valueOf(newCount).length() <= itemCount.maxStringLength)
            itemCount.setText(String.valueOf(newCount));
    }

    @Override
    public void actionPerformed(String ident, Object... params) {
        if (ident.equals("req"))
            sendItemRequest();
        else if (ident.equals("refrsh")) {
            itemList.resetDownloadStats();
            askForListRefresh();
        } else if (ident.equals("+"))
            countUp();
        else if (ident.equals("-"))
            countDown();
        else if (ident.equals("p+"))
            itemList.pageUp();
        else if (ident.equals("p-"))
            itemList.pageDown();
        else
            sendAction(ident);
    }

    public void receiveContentList(Map<ItemKey, Integer> content) {
        List<ItemKeyStack> list = new ArrayList<ItemKeyStack>(content.size());
        for (Entry<ItemKey, Integer> entry : content.entrySet())
            list.add(ItemKeyStack.get(entry.getKey(), entry.getValue()));

        Collections.sort(list);
        itemList.setDisplayList(list);
    }
}
