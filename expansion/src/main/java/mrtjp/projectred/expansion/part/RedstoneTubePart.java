package mrtjp.projectred.expansion.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.multipart.api.RedstoneInteractions;
import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.api.part.redstone.MaskedRedstonePart;
import codechicken.multipart.api.part.redstone.RedstonePart;
import codechicken.multipart.util.PartRayTraceResult;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.*;
import mrtjp.projectred.core.init.CoreItems;
import mrtjp.projectred.core.init.CoreTags;
import mrtjp.projectred.core.part.IPropagationCenterPart;
import mrtjp.projectred.core.part.IRedstonePropagationPart;
import mrtjp.projectred.core.part.IRedwirePart;
import mrtjp.projectred.expansion.TubeType;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.sounds.SoundSource;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.SoundType;

import java.util.LinkedList;

import static mrtjp.projectred.core.RedstonePropagator.FORCE;
import static mrtjp.projectred.core.RedstonePropagator.RISING;

//TODO extract out common code between this and FramedRedwirePart
public class RedstoneTubePart extends BaseTubePart implements IRedstonePropagationPart, IPropagationCenterPart, MaskedRedstonePart, IRedwirePart {

    private static final int KEY_SIGNAL = 10;

    private boolean hasRedstone = false;
    private byte signal = 0;

    public RedstoneTubePart(TubeType pipeType) {
        super(pipeType);
    }

    public boolean hasRedstone() {
        return hasRedstone;
    }

    //region save/load
    @Override
    public void save(CompoundTag tag) {
        super.save(tag);
        tag.putBoolean("has_redstone", hasRedstone);
        tag.putByte("signal", signal);
    }

    @Override
    public void load(CompoundTag tag) {
        super.load(tag);
        hasRedstone = tag.getBoolean("has_redstone");
        signal = tag.getByte("signal");
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        packet.writeBoolean(hasRedstone);
        packet.writeByte(signal);
    }

    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        hasRedstone = packet.readBoolean();
        signal = packet.readByte();
    }
    //endregion

    //region Network
    @Override
    protected void read(MCDataInput packet, int key) {
        //noinspection SwitchStatementWithTooFewBranches
        switch (key) {
            case KEY_SIGNAL -> {
                hasRedstone = packet.readBoolean();
                signal = packet.readByte();
                if (Configurator.staticWires) tile().markRender();
            }
            default -> super.read(packet, key);
        }
    }

    protected void sendSignalUpdate() {
        sendUpdate(KEY_SIGNAL, p -> p.writeBoolean(hasRedstone).writeByte(signal));
    }
    //endregion

    //region Multipart events
    @Override
    public InteractionResult activate(Player player, PartRayTraceResult hit, ItemStack held, InteractionHand hand) {
        var result = super.activate(player, hit, held, hand);
        if (result.consumesAction()) return result;

        // Couch + right click with empty hand removes redstone
        if (held.isEmpty() && player.isCrouching() && hasRedstone) {
            if (!level().isClientSide) {
                if (!player.isCreative()) {
                    PlacementLib.dropTowardsPlayer(level(), pos(), new ItemStack(CoreItems.RED_ALLOY_INGOT_ITEM.get()), player);
                }
                hasRedstone = false;
                tile().notifyPartChange(null);
                sendSignalUpdate();
            }
            return InteractionResult.sidedSuccess(level().isClientSide);
        }

        // Right click with red alloy adds redstone wiring to pipe
        if (!held.isEmpty() && held.is(CoreTags.RED_ALLOY_INGOT_TAG) && !hasRedstone) {
            if (!level().isClientSide) {
                // Swap the material
                hasRedstone = true;
                tile().notifyPartChange(null);
                sendSignalUpdate();

                // Play material sound
                SoundType sound = SoundType.GLASS;
                level().playSound(null, pos(), sound.getPlaceSound(), SoundSource.BLOCKS, sound.getVolume() + 1.0F/2.0F, sound.getPitch() * 0.8F);

                // Consume item from player if not in creative
                if (!player.isCreative()) {
                    held.shrink(1);
                }
            }
            return InteractionResult.sidedSuccess(level().isClientSide);
        }

        return InteractionResult.PASS;
    }

    @Override
    public Iterable<ItemStack> getDrops() {
        if (!hasRedstone) {
            return super.getDrops();
        }

        LinkedList<ItemStack> drops = new LinkedList<>();
        for (var drop : super.getDrops()) {
            drops.add(drop);
        }
        drops.add(new ItemStack(CoreItems.RED_ALLOY_INGOT_ITEM.get()));
        return drops;
    }
    //endregion

    protected int redstoneSignalLevel() {
        return RedstonePropagator.canRedwiresProvidePower() ? ((signal & 0xFF) + 16) / 17 : 0;
    }

    //region IMaskedRedstonePart overrides
    @Override
    public int strongPowerLevel(int side) {
        return 0;
    }

    @Override
    public int weakPowerLevel(int side) {
        if (!hasRedstone || !maskConnects(side)) return 0;
        return redstoneSignalLevel();
    }

    @Override
    public boolean canConnectRedstone(int side) {
        return hasRedstone;
    }

    @Override
    public int getConnectionMask(int side) {
        return 0x10;
    }
    //endregion

    //region IConnectableCenterPart overrides
    @Override
    public void maskChangeEvent(boolean internalChange, boolean externalChange) {
        super.maskChangeEvent(internalChange, externalChange);
        RedstonePropagator.logCalculation();
        RedstonePropagator.propagateTo(this, internalChange || externalChange ? FORCE : RISING);
    }
    //endregion

    //region IConnectable overrides
    @Override
    public boolean canConnectPart(IConnectable part, int dir) {
        if (!hasRedstone) return false;

        if (part instanceof MaskedRedstonePart mrp) {
            return (mrp.getConnectionMask(dir ^ 1) & 0x10) != 0;
        }

        if (part instanceof RedstonePart rp) {
            return (rp.canConnectRedstone(dir ^ 1));
        }

        return part instanceof IRedwirePart;
    }

    @Override
    public boolean discoverStraightOverride(int s) {
        if (!hasRedstone) return false;

        boolean prevCanConnectRW = RedstonePropagator.canConnectRedwires();
        RedstonePropagator.setCanConnectRedwires(false);
        boolean discovered = (RedstoneInteractions.otherConnectionMask(level(), pos(), s, false) &
                RedstoneInteractions.connectionMask(this, s)) != 0;
        RedstonePropagator.setCanConnectRedwires(prevCanConnectRW);
        return discovered;
    }

    @Override
    public boolean discoverInternalOverride(int s) {
        if (!hasRedstone) return false;

        MultiPart part = tile().getSlottedPart(s);
        if (part instanceof RedstonePart) {
            return ((RedstonePart) part).canConnectRedstone(s ^ 1);
        }
        return false;
    }
    //endregion

    //region IRedstonePropagationPart overrides
    @Override
    public int getSignal() {
        return signal & 0xFF;
    }

    @Override
    public void setSignal(int signal) {
        this.signal = (byte) signal;
    }

    @Override
    public void onSignalUpdate() {
        sendSignalUpdate();
    }

    @Override
    public int calculateSignal() {
        if (!hasRedstone) return 0;

        RedstonePropagator.setDustProvidesPower(false);
        RedstonePropagator.setRedwiresProvidePower(false);

        int signal = 0;
        for (int s = 0; s < 6; s++) {
            int sig = 0;
            if (maskConnectsIn(s)) {
                CenterLookup lookup = CenterLookup.lookupInsideFace(level(), pos(), s);
                sig = resolveSignal(lookup);

            } else if (maskConnectsOut(s)) {
                CenterLookup lookup = CenterLookup.lookupStraightCenter(level(), pos(), s);
                sig = resolveSignal(lookup);
                if (sig == 0) {
                    sig = RedstoneCenterLookup.resolveVanillaSignal(lookup, this);
                }
            }

            signal = Math.max(sig, signal);
        }

        RedstonePropagator.setDustProvidesPower(true);
        RedstonePropagator.setRedwiresProvidePower(true);
        return signal;
    }

    @Override
    public void propagateOther(int mode) {
        for (int s = 0; s < 6; s++) {
            if (!maskConnects(s)) {
                RedstonePropagator.addNeighborChange(level(), pos(), posOfStraight(s));
            }
        }
    }
    //endregion

    //region IRedwirePart overrides
    @Override
    public int getRedwireSignal(int dir) {
        return getSignal();
    }

    @Override
    public boolean diminishOnSide(int side) {
        return true;
    }
    //endregion

    protected int resolveSignal(CenterLookup lookup) {
        return RedstoneCenterLookup.resolveSignal(lookup, true);
    }
}
