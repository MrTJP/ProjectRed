package mrtjp.projectred.transmission.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.Cuboid6;
import codechicken.multipart.api.MultiPartType;
import codechicken.multipart.api.part.IconHitEffects;
import codechicken.multipart.api.part.TIconHitEffectsPart;
import codechicken.multipart.api.part.TMultiPart;
import codechicken.multipart.util.PartRayTraceResult;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.client.particle.ParticleManager;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Direction;
import net.minecraft.util.math.shapes.ISelectionContext;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import java.util.Collections;
import java.util.function.Consumer;

public abstract class BaseWirePart extends TMultiPart implements TIconHitEffectsPart {

    protected static final int KEY_UPDATE = 0;

    private final WireType wireType;

    public BaseWirePart(WireType wireType) {
        this.wireType = wireType;
    }

    public abstract void preparePlacement(Direction side);

    public WireType getWireType() {
        return wireType;
    }

    @Override
    public MultiPartType<?> getType() {
        return getWireType().getPartType();
    }

    protected ItemStack getItem() {
        return getWireType().makeStack();
    }

    @Override
    public Iterable<ItemStack> getDrops() {
        return Collections.singleton(getItem());
    }

    @Override
    public ItemStack pickItem(PartRayTraceResult hit) {
        return getItem();
    }

    //region Packets
    @Override
    public final void sendUpdate(Consumer<MCDataOutput> func) {
        sendUpdate(KEY_UPDATE, func);
    }

    @Override
    public final void readUpdate(MCDataInput packet) {
        read(packet, packet.readUByte());
    }

    protected final void sendUpdate(int key, Consumer<MCDataOutput> func) {
        super.sendUpdate(p -> {
            p.writeByte(key);
            func.accept(p);
        });
    }

    // Override to handle other keys
    protected void read(MCDataInput packet, int key) {
        switch (key) {
            case KEY_UPDATE:
                readDesc(packet);
                break;
            default:
                // Unkown
        }
    }
    //endregion

    //region Render stuff
    @OnlyIn(Dist.CLIENT)
    public int getRenderHue() {
        return -1;
    }

    @OnlyIn(Dist.CLIENT)
    public TextureAtlasSprite getIcon() {
        return getWireType().getTextures().get(0);
    }

    public boolean useStaticRenderer() {
        return Configurator.staticWires();
    }

    @Override
    public Cuboid6 getBounds() {
        return new Cuboid6(getShape(ISelectionContext.empty()).bounds());
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public TextureAtlasSprite getBreakingIcon(PartRayTraceResult hit) {
        return getIcon();
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public TextureAtlasSprite getBrokenIcon(int side) {
        return getIcon();
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void addHitEffects(PartRayTraceResult hit, ParticleManager manager) {
        IconHitEffects.addHitEffects(this, hit, manager);
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void addDestroyEffects(PartRayTraceResult hit, ParticleManager manager) {
        IconHitEffects.addDestroyEffects(this, manager);
    }
    //endregion
}
