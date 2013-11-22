package mrtjp.projectred.expansion;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import mrtjp.projectred.core.BasicGuiUtils;
import mrtjp.projectred.core.inventory.GhostGuiScreen;
import mrtjp.projectred.core.inventory.WidgetButton.WidgetSimpleButton;
import mrtjp.projectred.core.inventory.WidgetItemSelection;
import mrtjp.projectred.core.inventory.WidgetTextBox;
import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.core.utils.ItemKeyStack;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.vec.BlockCoord;

public class GuiRequester extends GhostGuiScreen {
    
    IWorldRoutedRequester pipe;
    
    public GuiRequester(IWorldRoutedRequester pipe) {
        super(280, 230);
        this.pipe = pipe;
    }
    
    WidgetItemSelection itemList = new WidgetItemSelection(xSize/2-(260/2), 10, 260, 140);
    WidgetTextBox textFilter = new WidgetTextBox(xSize/2-(150/2), 160, 150, 16, "") {
        @Override
        public void onTextChanged(String oldText) {
            itemList.setNewFilter(getText());
        }
    }.setMaxStringLength(24);
    
    WidgetTextBox itemCount = new WidgetTextBox(xSize/2-(50/2), 180, 50, 16, "1").setAllowedCharacters("0123456789").setMaxStringLength(7);
    
    public void drawBackground() {
        BasicGuiUtils.drawGuiBackGround(mc, 0, 0, xSize, ySize, zLevel, true);
    }

    public void drawForeground() {
    }

    public void addWidgets() {
        add(itemList);
        
        add(new WidgetSimpleButton(10, 165, 50, 16).setActionCommand("refrsh").setText("Re-poll"));
        add(new WidgetSimpleButton(10, 185, 50, 16).setActionCommand("req").setText("Submit"));
        
        add(textFilter);
        add(itemCount);

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
            PacketCustom packet = new PacketCustom(ExpansionSPH.channel, NetConstants.gui_Request_submit);
            packet.writeCoord(new BlockCoord(pipe.getContainer().tile()));
            //TODO options
            packet.writeBoolean(true);//pull
            packet.writeBoolean(true);//craft
            packet.writeBoolean(false);//partials
            packet.writeItemStack(request.getKey().makeStack(amount), true);
            packet.sendToServer();
        }
    }
    
    private void askForListRefresh() {
        PacketCustom packet = new PacketCustom(ExpansionSPH.channel, NetConstants.gui_Request_listRefresh);
        packet.writeCoord(new BlockCoord(pipe.getContainer().tile()));
        //TODO options
        packet.writeBoolean(true);//collect broadcasts
        packet.writeBoolean(true);//collect crafts
        packet.sendToServer();
    }
    
    private void sendAction(String ident) {
        PacketCustom packet = new PacketCustom(ExpansionSPH.channel, NetConstants.gui_Request_action);
        packet.writeCoord(new BlockCoord(pipe.getContainer().tile()));
        packet.writeString(ident);
        packet.sendToServer();
    }
    
    @Override
    public void actionPerformed(String ident, Object... params) {
        if (ident.equals("req"))
            sendItemRequest();
        else if (ident.equals("refrsh")) {
            askForListRefresh();
            itemList.resetDownloadStats();
        }
        else
            sendAction(ident);
    }
    
    public void receiveContentList(Map<ItemKey, Integer> content) {
        List<ItemKeyStack> list = new ArrayList<ItemKeyStack>(content.size());
        for (Entry<ItemKey, Integer> entry : content.entrySet())
            list.add(ItemKeyStack.get(entry.getKey(), entry.getValue()));
        
        itemList.setDisplayList(list);
    }
}
