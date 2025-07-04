package mrtjp.projectred.integration.gui.screen;

import codechicken.lib.packet.PacketCustom;
import mrtjp.projectred.integration.IntegrationNetwork;
import mrtjp.projectred.integration.part.ComplexGatePart;
import mrtjp.projectred.integration.part.GatePart;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.ButtonNode;
import mrtjp.projectred.redui.RedUIScreen;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;

import java.util.Objects;

import static mrtjp.projectred.integration.ProjectRedIntegration.MOD_ID;

public class CounterScreen extends RedUIScreen {

    private static final ResourceLocation BACKGROUND = ResourceLocation.fromNamespaceAndPath(MOD_ID, "textures/gui/counter_gate.png");

    private final GatePart gate;
    private final ComplexGatePart.ICounterGuiLogic counterGate;

    public CounterScreen(GatePart gate) {
        super(256, 145, Component.translatable(gate.getType().getRegistryName().toString()));
        this.gate = gate;
        this.counterGate = (ComplexGatePart.ICounterGuiLogic) gate;

        for (int row = 0; row < 3; row++) {
            int y = 16 + 40 * row;
            createButton(5, y, 40, 20, row, -10);
            createButton(46, y, 40, 20, row, -5);
            createButton(87, y, 40, 20, row, -1);
            createButton(129, y, 40, 20, row, 1);
            createButton(170, y, 40, 20, row, 5);
            createButton(211, y, 40, 20, row, 10);
        }
    }

    private void createButton(int x, int y, int w, int h, int id, int delta) {
        ButtonNode b = new ButtonNode();
        b.setPosition(x, y);
        b.setSize(w, h);
        b.setButtonText((delta < 0 ? "" : "+") + delta);
        b.setClickFunction(() -> {
            PacketCustom packet = new PacketCustom(IntegrationNetwork.NET_CHANNEL, IntegrationNetwork.INCR_COUNTER_FROM_CLIENT, Minecraft.getInstance().level.registryAccess());
            IntegrationNetwork.writePartIndex(packet, gate);
            packet.writeByte(id);
            packet.writeShort(delta);
            packet.sendToServer();

        });
        addChild(b);
    }

    @Override
    public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {
        super.drawBack(graphics, mouse, partialFrame);

        int x = getFrame().x();
        int y = getFrame().y();
        int w = getFrame().width();

        graphics.blit(BACKGROUND, x, y, 0, 0, getFrame().width(), getFrame().height());
        String s = "Maximum: " + counterGate.getCounterMax();
        graphics.drawString(getFontRenderer(), s, x + (w - getFontRenderer().width(s)) / 2f, y + 5, 0x404040, false);
        s = "Increment: " + counterGate.getCounterIncr();
        graphics.drawString(getFontRenderer(), s, x + (w - getFontRenderer().width(s)) / 2f, y + 45, 0x404040, false);
        s = "Decrement: " + counterGate.getCounterDecr();
        graphics.drawString(getFontRenderer(), s, x + (w - getFontRenderer().width(s)) / 2f, y + 85, 0x404040, false);
        s = "State: " + counterGate.getCounterValue();
        graphics.drawString(getFontRenderer(), s, x + (w - getFontRenderer().width(s)) / 2f, y + 125, 0x404040, false);
    }

    @Override
    public void update() {
        if (!gate.hasTile()) {
            Objects.requireNonNull(getMinecraft().player).closeContainer();
        }
    }
}
