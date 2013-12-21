package mrtjp.projectred.transportation;

import java.util.LinkedList;
import java.util.List;

import mrtjp.projectred.core.BasicGuiUtils;
import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.core.inventory.GhostGuiContainer;
import mrtjp.projectred.core.inventory.WidgetButton.WidgetDotSelector;
import mrtjp.projectred.core.inventory.WidgetButton.WidgetSimpleButton;
import mrtjp.projectred.core.utils.Pair2;
import mrtjp.projectred.transportation.ItemRouterUtility.ChipUpgradeContainer;
import mrtjp.projectred.transportation.RoutingChipset.UpgradeBus;
import net.minecraft.util.EnumChatFormatting;
import net.minecraft.util.ResourceLocation;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.FontUtils;

public class GuiChipUpgrade extends GhostGuiContainer
{
    private static final ResourceLocation utilCont = new ResourceLocation("projectred:textures/gui/chipupgradecontainer.png");
    ChipUpgradeContainer c;
        
    public GuiChipUpgrade(ChipUpgradeContainer container)
    {
        super(container, null, 176, 200);
        c = container;
    }

    @Override
    public void addWidgets()
    {
        add(new WidgetSimpleButton(xSize/2 - 20, 56, 40, 15).setText("Install").setActionCommand("inst"));
        
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
        CCRenderState.changeTexture(utilCont);
        drawTexturedModalRect(0, 0, 0, 0, xSize, ySize);
        
        if (c.getChip() != null)
        {
            UpgradeBus b = c.getChip().getUpgradeBus();
            fontRenderer.drawString(String.valueOf(b.LXLatency > 0 ? b.LXLatency : "-"), 29, 23, PRColors.GREY.rgb);
            fontRenderer.drawString(String.valueOf(b.LYLatency > 0 ? b.LYLatency : "-"), 29, 43, PRColors.GREY.rgb);
            fontRenderer.drawString(String.valueOf(b.LZLatency > 0 ? b.LZLatency : "-"), 29, 63, PRColors.GREY.rgb);
                        
            FontUtils.drawRightString(String.valueOf(b.RXLatency > 0 ? b.RXLatency : "-"), 148, 23, PRColors.GREY.rgb);
            FontUtils.drawRightString(String.valueOf(b.RYLatency > 0 ? b.RYLatency : "-"), 148, 43, PRColors.GREY.rgb);
            FontUtils.drawRightString(String.valueOf(b.RZLatency > 0 ? b.RZLatency : "-"), 148, 63, PRColors.GREY.rgb);
        }
        
        int s = 0;
        for (Pair2<Integer, Integer> coord : BasicGuiUtils.createSlotArray(8, 18, 1, 3, 2, 2))
        {
            int x = coord.getValue1();
            int y = coord.getValue2();
            int color = getColorForSlot(s++);
            drawGradientRect(x-2, y+4, x, y+12, color, color);
        }
        for (Pair2<Integer, Integer> coord : BasicGuiUtils.createSlotArray(152, 18, 1, 3, 2, 2))
        {
            int x = coord.getValue1();
            int y = coord.getValue2();
            int color = getColorForSlot(s++);
            drawGradientRect(x+16, y+4, x+18, y+12, color, color);
        }
    }

    private int getColorForSlot(int slot)
    {
        if (c.getChip() != null)
        {
            UpgradeBus b = c.getChip().getUpgradeBus();
            
            boolean hasChip = false;
            boolean canInstall = false;
            boolean canHandle = false;
            
            if (slot < 3)
            {
                hasChip = b.Lset[slot];
                canInstall = b.installL(slot, false);
                canHandle = b.maxL > slot;
            }
            else if (slot < 6)
            {
                hasChip = b.Rset[slot-3];
                canInstall = b.installR(slot-3, false);
                canHandle = b.maxR > slot-3;
            }
            int color = PRColors.BLACK.rgb;
            
            if (hasChip)
                color = 0xff6BF100;
            else if (canInstall)
                color = 0xffBFBF00;
            else if (canHandle)
                color = 0xffA20F06;
            else if (!canHandle)
                color = 0xff535353;
            
            return color;
        }
        return 0;
    }
}
