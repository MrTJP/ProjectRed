package mrtjp.projectred.integration.gui.screen;

import codechicken.lib.packet.PacketCustom;
import codechicken.lib.texture.TextureUtils;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.projectred.integration.IntegrationNetwork;
import mrtjp.projectred.integration.part.ComplexGatePart;
import mrtjp.projectred.integration.part.GatePart;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.ButtonNode;
import mrtjp.projectred.redui.RedUIScreen;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.text.StringTextComponent;

import static mrtjp.projectred.ProjectRedIntegration.MOD_ID;

public class TimerScreen extends RedUIScreen {

    private static final ResourceLocation BACKGROUND = new ResourceLocation(MOD_ID, "textures/gui/timer_gate.png");

    private final GatePart gate;
    private final ComplexGatePart.ITimerGuiLogic timerGate;

    public TimerScreen(GatePart gate) {
        super(256, 55, new StringTextComponent(gate.getType().getRegistryName().toString()));
        this.gate = gate;
        this.timerGate = (ComplexGatePart.ITimerGuiLogic) gate;

        createButton(5, 25, 40, 20, "-10s", -200);
        createButton(46, 25, 40, 20, "-1s", -20);
        createButton(87, 25, 40, 20, "-50ms", -1);
        createButton(129, 25, 40, 20, "+50ms", 1);
        createButton(170, 25, 40, 20, "+1s", 20);
        createButton(211, 25, 40, 20, "+10s", 200);
    }

    private void createButton(int x, int y, int w, int h, String text, int delta) {
        ButtonNode b = new ButtonNode();
        b.setPosition(x, y);
        b.setSize(w, h);
        b.setButtonText(text);
        b.setClickFunction(() -> {
            PacketCustom packet = new PacketCustom(IntegrationNetwork.NET_CHANNEL, IntegrationNetwork.INCR_TIMER_FROM_CLIENT);
            IntegrationNetwork.writePartIndex(packet, gate);
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

        blit(stack, x, y, 0, 0, getFrame().width(), getFrame().height());

        String s = String.format("Timer interval: %.2fs", timerGate.getTimerMax() * 0.05);
        int sw = getFontRenderer().width(s);
        getFontRenderer().draw(stack, s, x + (getFrame().width() - sw)/2f, y + 8, 0x404040);
    }

    @Override
    public void update() {
        if (gate.tile() == null) {
            getMinecraft().player.closeContainer();
        }
    }

    public static void open(PlayerEntity player, GatePart part) {
        PacketCustom packet = new PacketCustom(IntegrationNetwork.NET_CHANNEL, IntegrationNetwork.OPEN_TIMER_GUI_FROM_SERVER);
        IntegrationNetwork.writePartIndex(packet, part);
        packet.sendToPlayer((ServerPlayerEntity) player);
    }
}
