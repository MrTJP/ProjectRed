package mrtjp.projectred.transportation;

import java.util.LinkedList;
import java.util.List;

import mrtjp.projectred.core.inventory.GhostGuiContainer;
import mrtjp.projectred.core.inventory.WidgetButton.WidgetDotSelector;
import mrtjp.projectred.core.inventory.WidgetButton.WidgetSimpleButton;
import mrtjp.projectred.transportation.ItemRouterUtility.RouterUtilContainer;
import mrtjp.projectred.transportation.RoutingChipset.UpgradeBus;
import net.minecraft.util.EnumChatFormatting;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.render.CCRenderState;

public class GuiRouterUtility extends GhostGuiContainer
{
    RouterUtilContainer c;
        
    public GuiRouterUtility(RouterUtilContainer container)
    {
        super(container, null, 176, 200);
        c = container;
    }
    
    @Override
    public void addWidgets()
    {
        add(new WidgetSimpleButton(108, 75, 40, 15).setText("Install").setActionCommand("inst"));
        
        add(new WidgetDotSelector(67, 45) {
            @Override
            public List<String> getOverlayText()
            {
                List<String> list = new LinkedList<String>();
                if (c.getChip() != null)
                {
                    UpgradeBus b = c.getChip().getUpgradeBus();
                    
                    list.add("L slot");
                    list.add(EnumChatFormatting.GRAY + "Latency: " + b.LLatency());
                    list.add("");
                    list.add(EnumChatFormatting.GRAY + b.Linfo);
                    list.add(EnumChatFormatting.YELLOW + b.Lformula);
                }
                return list;
            }
        });

        add(new WidgetDotSelector(110, 45) {
            @Override
            public List<String> getOverlayText()
            {
                List<String> list = new LinkedList<String>();
                if (c.getChip() != null)
                {
                    UpgradeBus b = c.getChip().getUpgradeBus();
                    
                    list.add("R slot");
                    list.add(EnumChatFormatting.GRAY + "Latency: " + b.RLatency());
                    list.add("");
                    list.add(EnumChatFormatting.GRAY + b.Rinfo);
                    list.add(EnumChatFormatting.YELLOW + b.Rformula);
                }
                return list;
            }
        });
    }
    
    @Override
    public void actionPerformed(String ident, Object... params)
    {
        new PacketCustom(TransportationSPH.channel, NetConstants.gui_RouterUtil_action).writeString(ident).sendToServer();
    }
    
    @Override
    public void drawBackground()
    {
        CCRenderState.changeTexture("projectred:textures/gui/chipupgradecontainer.png");
        drawTexturedModalRect(0, 0, 0, 0, xSize, ySize);
        
        if (c.getChip() != null)
        {
            UpgradeBus b = c.getChip().getUpgradeBus();
        }
    }
}
