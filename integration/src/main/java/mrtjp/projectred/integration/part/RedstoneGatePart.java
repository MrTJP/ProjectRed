package mrtjp.projectred.integration.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.multipart.api.RedstoneInteractions;
import codechicken.multipart.api.part.AnimateTickPart;
import codechicken.multipart.api.part.redstone.IFaceRedstonePart;
import codechicken.multipart.init.CBMultipartModContent;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.FaceLookup;
import mrtjp.projectred.core.part.IRedwireEmitter;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.client.GateModelRenderer;
import net.minecraft.block.Blocks;
import net.minecraft.block.RedstoneWireBlock;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.Direction;
import net.minecraft.util.SoundCategory;
import net.minecraft.util.SoundEvents;
import net.minecraft.util.math.BlockPos;

import java.util.Random;

public abstract class RedstoneGatePart extends GatePart implements IFaceRedstonePart, AnimateTickPart {

    public static final int KEY_STATE = 10;

    /**
     * Mapped inputs and outputs of the gate.
     * OOOO IIII
     * High nybble is output.
     * Low nybble is input
     */
    private byte gateState = 0;

    public RedstoneGatePart(GateType type) {
        super(type);
    }

    public int getState() {
        return gateState & 0xFF;
    }

    public void setState(int gateState) {
        this.gateState = (byte) gateState;
    }

    @Override
    public int state() {
        return gateState & 0xFF;
    }

    //region Save/load and description
    @Override
    public void save(CompoundNBT tag) {
        super.save(tag);
        tag.putByte("state", gateState);
    }

    @Override
    public void load(CompoundNBT tag) {
        super.load(tag);
        gateState = tag.getByte("state");
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        packet.writeByte(gateState);
    }

    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        gateState = packet.readByte();
    }
    //endregion

    //region Packets
    @Override
    protected void read(MCDataInput packet, int key) {
        switch (key) {
            case KEY_STATE:
                gateState = packet.readByte();
                if (Configurator.staticGates) {
                    tile().markRender();
                }
                break;
            default:
                super.read(packet, key);
        }
    }

    protected void sendStateUpdate() {
        sendUpdate(KEY_STATE, p -> p.writeByte(gateState));
    }
    //endregion

    @Override
    public void notifyExternals(int mask) {
        int smask = 0;

        for (int r = 0; r < 4; r++) {
            if ((mask & 1 << r) == 0) continue;
            int absSide = absoluteDir(r);
            BlockPos pos2 = pos().relative(Direction.values()[absSide]);

            world().neighborChanged(pos2, CBMultipartModContent.blockMultipart, pos());
            for (int s = 0; s < 6; s++) {
                if (s != (absSide ^ 1) && (smask & 1 << s) == 0) {
                    world().neighborChanged(
                            pos2.relative(Direction.values()[s]), CBMultipartModContent.blockMultipart, pos2);
                }
            }
            smask |= 1 << absSide;
        }
    }

    //region Multipart properties

    @Override
    public int getFace() {
        return getSide();
    }
    //endregion

    //region Redstone signaling
    @Override
    public int strongPowerLevel(int side) {
        if ((side & 6) == (getSide() & 6)) return 0;
        int ir = toInternal(absoluteRot(side));

        return (outputMask(shape()) & 1 << ir) != 0 ? getOutput(ir) : 0;
    }

    @Override
    public int weakPowerLevel(int side) {
        return strongPowerLevel(side);
    }

    @Override
    public boolean canConnectRedstone(int side) {
        if ((side & 6) == (getSide() & 6)) return false;
        return gateLogicCanConnect(toInternal(absoluteRot(side)));
    }

    protected int getRedstoneInput(int r) {
        int ar = toAbsolute(r);

        FaceLookup lookup = null;
        if (maskConnectsCorner(ar)) {
            lookup = FaceLookup.lookupCorner(world(), pos(), getSide(), ar);
        } else if (maskConnectsStraight(ar)) {
            lookup = FaceLookup.lookupStraight(world(), pos(), getSide(), ar);
        } else if (maskConnectsInside(ar)) {
            lookup = FaceLookup.lookupInsideFace(world(), pos(), getSide(), ar);
        }

        return lookup == null ? getVanillaSignal(ar, true, false) : resolveSignal(lookup);
    }

    protected int getAnalogRedstoneInput(int r) {
        return (getRedstoneInput(r) + 16) / 17;
    }

    protected int resolveSignal(FaceLookup lookup) {
        if (lookup.part instanceof IRedwireEmitter) {
            return ((IRedwireEmitter) lookup.part).getRedwireSignal(lookup.otherRotation);
        }
        return 0;
    }

    protected int getVanillaSignal(int r, boolean strong, boolean limitDust) {
        FaceLookup lookup = FaceLookup.lookupStraight(world(), pos(), getSide(), r);
        int signal = 0;

        // Dust signal
        if (lookup.block == Blocks.REDSTONE_WIRE) {
            signal = Math.max(lookup.state.getValue(RedstoneWireBlock.POWER) - 1, 0);
            if (limitDust) {
                return signal;
            }
        }

        // Strong signal
        int dir = absoluteDir(r);
        signal = RedstoneInteractions.getPowerTo(this, dir) * 17;
        if (signal > 0 && strong) {
            return signal;
        }

        // Weak signal
        if (lookup.state.isRedstoneConductor(world(), lookup.otherPos)) {
            signal = world().getBestNeighborSignal(lookup.otherPos) * 17;
        }

        return signal;
    }
    //endregion

    //region Gate logic
    protected void onInputChange() {
        tile().setChanged();
        sendStateUpdate();
    }

    protected void onOutputChange(int mask) {
        tile().setChanged();
        sendStateUpdate();
        tile().internalPartChange(this);
        notifyExternals(toAbsoluteMask(mask));
    }

    protected void tickSound() {
        if (Configurator.logicGateSounds) {
            world().playSound(null, pos(), SoundEvents.LEVER_CLICK, SoundCategory.BLOCKS, 0.15F, 0.5f);
        }
    }

    @Override
    protected boolean gateLogicCanConnectTo(IConnectable part, int r) {
        if (part instanceof IRedwireEmitter) {
            return gateLogicCanConnect(r);
        }
        return false;
    }

    protected boolean gateLogicCanConnect(int r) {
        int ioMask = outputMask(shape()) | inputMask(shape());
        return ((ioMask) & 1 << r) != 0;
    }

    protected int outputMask(int shape) {
        return 0;
    }

    protected int inputMask(int shape) {
        return 0;
    }

    int getOutput(int r) {
        return (getState() & 0x10 << r) != 0 ? 15 : 0;
    }

    int getInput(int mask) {
        int input = 0;
        for (int r = 0; r < 4; r++) {
            if ((mask & 1 << r) != 0 && getRedstoneInput(r) > 0) {
                input |= 1 << r;
            }
        }
        return input;
    }
    //endregion

    //region Rendering
    @Override
    public void animateTick(Random random) {
        // client side only
        GateModelRenderer.instance().spawnParticles(this, random);
    }
    //endregion
}
