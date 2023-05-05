package mrtjp.projectred.integration.gui.screen;

import codechicken.lib.packet.PacketCustom;
import codechicken.lib.texture.TextureUtils;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.core.vec.Point;
import mrtjp.projectred.integration.IntegrationNetwork;
import mrtjp.projectred.integration.part.ComplexGatePart;
import mrtjp.projectred.integration.part.GatePart;
import mrtjp.projectred.redui.ButtonNode;
import mrtjp.projectred.redui.RedUIScreen;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.text.StringTextComponent;

import static mrtjp.projectred.ProjectRedIntegration.MOD_ID;

public class CounterScreen extends RedUIScreen {

    private static final ResourceLocation BACKGROUND = new ResourceLocation(MOD_ID, "textures/gui/counter_gate.png");

    private final GatePart gate;
    private final ComplexGatePart.ICounterGuiLogic counterGate;

    public CounterScreen(GatePart gate) {
        super(256, 145, new StringTextComponent(gate.getType().getRegistryName().toString()));
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
            PacketCustom packet = new PacketCustom(IntegrationNetwork.NET_CHANNEL, IntegrationNetwork.INCR_COUNTER_FROM_CLIENT);
            IntegrationNetwork.writePartIndex(packet, gate);
            packet.writeByte(id);
            packet.writeShort(delta);
            packet.sendToServer();

        });
        addChild(b);
    }

    @Override
    public void drawBack(MatrixStack stack, Point mouse, float partialFrame) {
        super.drawBack(stack, mouse, partialFrame);

        TextureUtils.changeTexture(BACKGROUND);
        int x = getFrame().x();
        int y = getFrame().y();
        int w = getFrame().width();

        blit(stack, x, y, 0, 0, getFrame().width(), getFrame().height());
        String s = "Maximum: " + counterGate.getCounterMax();
        getFontRenderer().draw(stack, s, x + (w - getFontRenderer().width(s)) / 2f, y + 5, 0x404040);
        s = "Increment: " + counterGate.getCounterIncr();
        getFontRenderer().draw(stack, s, x + (w - getFontRenderer().width(s)) / 2f, y + 45, 0x404040);
        s = "Decrement: " + counterGate.getCounterDecr();
        getFontRenderer().draw(stack, s, x + (w - getFontRenderer().width(s)) / 2f, y + 85, 0x404040);
        s = "State: " + counterGate.getCounterValue();
        getFontRenderer().draw(stack, s, x + (w - getFontRenderer().width(s)) / 2f, y + 125, 0x404040);
    }

    @Override
    public void update() {
        if (gate.tile() == null) {
            getMinecraft().player.closeContainer();
        }
    }

    public static void open(PlayerEntity player, GatePart part) {
        PacketCustom packet = new PacketCustom(IntegrationNetwork.NET_CHANNEL, IntegrationNetwork.OPEN_COUNTER_GUI_FROM_SERVER);
        IntegrationNetwork.writePartIndex(packet, part);
        packet.sendToPlayer((ServerPlayerEntity) player);
    }
}
