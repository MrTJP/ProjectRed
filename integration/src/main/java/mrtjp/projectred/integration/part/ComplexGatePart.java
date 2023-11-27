package mrtjp.projectred.integration.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.vec.Rotation;
import codechicken.multipart.api.part.NeighborTileChangePart;
import codechicken.multipart.util.PartRayTraceResult;
import mrtjp.projectred.api.IScrewdriver;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.IntegrationNetwork;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.decoration.ItemFrame;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.RedStoneWireBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.AABB;

import java.util.List;

import static mrtjp.projectred.core.part.IOrientableFacePart.flipMaskZ;
import static mrtjp.projectred.core.part.IOrientableFacePart.shiftMask;

public abstract class ComplexGatePart extends RedstoneGatePart {

    public static final int KEY_STATE2 = 20;

    private byte state2 = 0;

    public ComplexGatePart(GateType type) {
        super(type);
    }

    public int state2() {
        return state2 & 0xFF;
    }

    public void setState2(int state2) {
        this.state2 = (byte) state2;
    }

    protected boolean clientNeedsState2() {
        return false;
    }

    //region save/load
    @Override
    public void save(CompoundTag tag) {
        super.save(tag);
        tag.putByte("state2", state2);
    }

    @Override
    public void load(CompoundTag tag) {
        super.load(tag);
        state2 = tag.getByte("state2");
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        if (clientNeedsState2()) packet.writeByte(state2);
    }

    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        if (clientNeedsState2()) state2 = packet.readByte();
    }
    //endregion

    //region Packets
    @Override
    protected void read(MCDataInput packet, int key) {
        switch (key) {
            case KEY_STATE2:
                state2 = packet.readByte();
                break;
            default:
                super.read(packet, key);
        }
    }

    protected void sendState2Update() {
        sendUpdate(KEY_STATE2, w -> w.writeByte(state2));
    }
    //endregion

    public static class SRLatch extends ComplexGatePart {

        public SRLatch(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return (shape >> 1) == 0 ? 0xF : 5;
        }

        @Override
        protected int inputMask(int shape) {
            return 0xA;
        }

        @Override
        protected boolean gateLogicCycleShape() {
            setShape((shape() + 1) % 4);
            setState2(flipMaskZ(state2()));
            setState(flipMaskZ(state()));
            onOutputChange(0xF);
            scheduleTick(2);
            return true;
        }

        @Override
        protected void gateLogicSetup() {
            setState2(2);
            setState(0x30);
        }

        @Override
        protected void gateLogicOnChange() {
            int stateInput = state2();

            int oldInput = state() & 0xF;
            int newInput = getInput(0xA);
            int oldOutput = state() >> 4;

            if (newInput != oldInput) {
                if (stateInput != 0xA && newInput != 0 && newInput != stateInput) { //state needs changing
                    setState(newInput);
                    setState2(newInput);
                    onOutputChange(oldOutput); //always going low
                    scheduleTick(2);
                } else {
                    setState(oldOutput << 4 | newInput);
                    onInputChange();
                }
            }
        }

        @Override
        protected void gateLogicOnScheduledTick() {
            int oldOutput = state() >> 4;
            int newOutput = calcOutput();

            if (oldOutput != newOutput) {
                setState(state() & 0xF | newOutput << 4);
                onOutputChange(outputMask(shape()));
            }
            gateLogicOnChange();
        }

        private int calcOutput() {
            int input = state() & 0xF;
            int stateInput = state2();

            if ((shape() & 1) != 0) { //reverse
                input = flipMaskZ(input);
                stateInput = flipMaskZ(stateInput);
            }

            if (stateInput == 0xA) { //disabled
                if (input == 0xA) {
                    scheduleTick(2);
                    return 0;
                }
                stateInput = input == 0 ? level().random.nextBoolean() ? 2 : 8 : input;
                setState2((shape() & 1) != 0 ? flipMaskZ(stateInput) : stateInput);
            }

            int output = shiftMask(stateInput, 1);
            if ((shape() & 2) == 0) output |= stateInput;
            if ((shape() & 1) != 0) output = flipMaskZ(output); //reverse
            return output;
        }
    }

    public static class ToggleLatch extends ComplexGatePart {

        public ToggleLatch(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return 5;
        }

        @Override
        protected int inputMask(int shape) {
            return 0xA;
        }

        @Override
        protected boolean clientNeedsState2() {
            return true;
        }

        @Override
        protected void gateLogicSetup() {
            setState(0x10);
            sendStateUpdate();
        }

        @Override
        protected void gateLogicOnChange() {
            int oldInput = state() & 0xF;
            int newInput = getInput(0xA);
            int high = newInput & ~oldInput;

            if (high == 2 || high == 8) toggle();

            if (oldInput != newInput) {
                setState(state() & 0xF0 | newInput);
                onInputChange();
            }
        }

        @Override
        protected void gateLogicOnScheduledTick() {
            int oldOutput = state() >> 4;
            int newOutput = state2() == 0 ? 1 : 4;
            if (oldOutput != newOutput) {
                setState(newOutput << 4 | state() & 0xF);
                onOutputChange(5);
            }
            gateLogicOnChange();
        }

        @Override
        protected boolean gateLogicActivate(Player player, ItemStack held, PartRayTraceResult hit) {
            if (held.isEmpty() || !(held.getItem() instanceof IScrewdriver)) {
                if (!level().isClientSide) toggle();
                return true;
            }
            return false;
        }

        private void toggle() {
            setState2(state2() == 0 ? 1 : 0);
            scheduleTick(2);
            tickSound();
        }
    }

    public interface ITimerGuiLogic {

        int getTimerMax();

        void setTimerMax(int t);

        static void openFromServer(Player player, GatePart part) {
            PacketCustom packet = new PacketCustom(IntegrationNetwork.NET_CHANNEL, IntegrationNetwork.OPEN_TIMER_GUI_FROM_SERVER);
            IntegrationNetwork.writePartIndex(packet, part);
            packet.sendToPlayer((ServerPlayer) player);
        }
    }

    public interface ICounterGuiLogic {

        int getCounterMax();

        void setCounterMax(int i);

        int getCounterIncr();

        void setCounterIncr(int i);

        int getCounterDecr();

        void setCounterDecr(int i);

        int getCounterValue();

        void setCounterValue(int i);

        static void openFromServer(Player player, GatePart part) {
            PacketCustom packet = new PacketCustom(IntegrationNetwork.NET_CHANNEL, IntegrationNetwork.OPEN_COUNTER_GUI_FROM_SERVER);
            IntegrationNetwork.writePartIndex(packet, part);
            packet.sendToPlayer((ServerPlayer) player);
        }
    }

    public static abstract class RedstoneTimerGatePart extends ComplexGatePart implements ITimerGuiLogic {

        private static final int KEY_POINTER_MAX = 30;
        private static final int KEY_POINTER_START = 31;

        private int pointer_max = 38;
        private long pointer_start = -1L;

        public RedstoneTimerGatePart(GateType type) {
            super(type);
        }

        //region save/load
        @Override
        public void save(CompoundTag tag) {
            super.save(tag);
            tag.putInt("pmax", pointer_max);
            tag.putLong("pelapsed", pointer_start < 0 ? -1L : level().getGameTime() - pointer_start);
        }

        @Override
        public void load(CompoundTag tag) {
            super.load(tag);
            pointer_max = tag.getInt("pmax");
            pointer_start = tag.getLong("pelapsed");
        }

        @Override
        public void writeDesc(MCDataOutput packet) {
            super.writeDesc(packet);
            packet.writeInt(pointer_max);
            packet.writeLong(pointer_start);
        }

        @Override
        public void readDesc(MCDataInput packet) {
            super.readDesc(packet);
            pointer_max = packet.readInt();
            pointer_start = packet.readLong();
        }

        @Override
        protected void gateLogicOnWorldLoad() {
            if (pointer_start >= 0) pointer_start = level().getGameTime() - pointer_start;
        }
        //endregion

        //region Packets
        @Override
        protected void read(MCDataInput packet, int key) {
            switch (key) {
                case KEY_POINTER_MAX:
                    pointer_max = packet.readInt();
                    break;
                case KEY_POINTER_START:
                    pointer_start = packet.readInt();
                    if (pointer_start >= 0) pointer_start = level().getGameTime() - pointer_start;
                    break;
                default:
                    super.read(packet, key);
            }
        }

        protected void sendPointerMaxUpdate() {
            sendUpdate(KEY_POINTER_MAX, p -> p.writeInt(pointer_max));
        }

        protected void sendPointerUpdate() {
            sendUpdate(KEY_POINTER_START, p -> p.writeInt(pointer_start < 0 ? -1 : pointerValue()));
        }
        //endregion

        //region ITimerGuiLogic
        @Override
        public int getTimerMax() {
            return pointer_max + 2;
        }

        @Override
        public void setTimerMax(int t) {
            int minTime = Math.max(4, Configurator.minTimerTicks);
            if (t < minTime) t = minTime;
            if (t != pointer_max) { //TODO check this, should be - 2 here i think
                pointer_max = t - 2;
                sendPointerMaxUpdate();
            }
        }
        //endregion

        //region Gate logic
        @Override
        protected void gateLogicOnTick() {
            if (pointer_start >= 0) {
                if (level().getGameTime() >= pointer_start + pointer_max) {
                    pointerTick();
                } else if (pointer_start > level().getGameTime()) {
                    pointer_start = level().getGameTime();
                }
            }
        }

        protected void startPointer() {
            if (pointer_start < 0) {
                pointer_start = level().getGameTime();
                tile().setChanged();
                if (!level().isClientSide) sendPointerUpdate();
            }
        }

        protected void resetPointer() {
            if (pointer_start >= 0) {
                pointer_start = -1;
                tile().setChanged();
                if (!level().isClientSide) sendPointerUpdate();
            }
        }

        protected abstract void pointerTick();
        //endregion

        //region IGateRenderData
        @Override
        public boolean isPointerStarted() {
            return pointer_start >= 0;
        }

        @Override
        public int pointerMax() {
            return pointer_max;
        }

        @Override
        public int pointerValue() {
            return pointer_start < 0 ? 0 : (int) (level().getGameTime() - pointer_start);
        }
        //endregion

        @Override
        protected boolean gateLogicActivate(Player player, ItemStack held, PartRayTraceResult hit) {
            if (held.isEmpty() || !(held.getItem() instanceof IScrewdriver)) {
                if (!level().isClientSide) {
                    ITimerGuiLogic.openFromServer(player, this);
                }
                return true;
            }
            return false;
        }
    }

    public static class Timer extends RedstoneTimerGatePart {

        public Timer(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return 0xB;
        }

        @Override
        protected int inputMask(int shape) {
            return 0xE;
        }

        @Override
        protected void gateLogicSetup() {
            startPointer();
        }

        @Override
        protected void gateLogicOnScheduledTick() {
            setState(state() & 0xF);
            onOutputChange(0xB);
            gateLogicOnChange();
        }

        @Override
        protected void gateLogicOnChange() {
            int oldInput = state() & 0xF;
            int newInput = getInput(0xE);

            if (newInput != oldInput) {
                setState(state() & 0xF0 | newInput);
                onInputChange();
            }

            if (!isTickScheduled()) {
                if (newInput > 0) { resetPointer(); } else startPointer();
            }
        }

        @Override
        protected void pointerTick() {
            resetPointer();
            if (!level().isClientSide) {
                scheduleTick(2);
                setState(0xB0 | state() & 0xF);
                onOutputChange(0xB);
                tickSound();
            }
        }
    }

    public static class Sequencer extends ComplexGatePart implements ITimerGuiLogic {

        private static final int KEY_POINTER_MAX = 30;

        private int pointer_max = 40;

        public Sequencer(GateType type) {
            super(type);
        }

        //region save/load
        @Override
        public void save(CompoundTag tag) {
            super.save(tag);
            tag.putInt("pmax", pointer_max);
        }

        @Override
        public void load(CompoundTag tag) {
            super.load(tag);
            pointer_max = tag.getInt("pmax");
        }

        @Override
        public void writeDesc(MCDataOutput packet) {
            super.writeDesc(packet);
            packet.writeInt(pointer_max);
        }

        @Override
        public void readDesc(MCDataInput packet) {
            super.readDesc(packet);
            pointer_max = packet.readInt();
        }
        //endregion

        //region Packets
        @Override
        protected void read(MCDataInput packet, int key) {
            switch (key) {
                case KEY_POINTER_MAX:
                    pointer_max = packet.readInt();
                    break;
                default:
                    super.read(packet, key);
            }
        }

        protected void sendPointerMaxUpdate() {
            sendUpdate(KEY_POINTER_MAX, p -> p.writeInt(pointer_max));
        }
        //endregion

        //region IGuiTimerLogic
        @Override
        public int getTimerMax() {
            return pointer_max;
        }

        @Override
        public void setTimerMax(int t) {
            int minTime = Math.max(4, Configurator.minTimerTicks);
            if (t < minTime) t = minTime;
            if (t != pointer_max) {
                pointer_max = t;
                sendPointerMaxUpdate();
            }
        }
        //endregion

        //region Gate Render data
        @Override
        public boolean isPointerStarted() {
            return true;
        }

        @Override
        public int pointerValue() {
            return (int) (level().getDayTime() % (pointer_max * 4L));
        }

        @Override
        public int pointerMax() {
            return pointer_max;
        }
        //endregion

        //region Gate logic
        @Override
        protected int outputMask(int shape) {
            return 0xF;
        }

        @Override
        protected void gateLogicOnChange() {
        }

        @Override
        protected void gateLogicOnScheduledTick() {
        }

        @Override
        protected void gateLogicOnTick() {
            if (level().isClientSide) return;

            int oldOut = state() >> 4;
            int out = 1 << level().getDayTime() % (pointer_max * 4L) / pointer_max;
            if (shape() == 1) out = flipMaskZ(out);
            if (oldOut != out) {
                setState(out << 4);
                onOutputChange(0xF);
                tickSound();
            }
        }

        @Override
        protected boolean gateLogicCycleShape() {
            setShape(shape() == 0 ? 1 : 0);
            return true;
        }

        @Override
        protected boolean gateLogicActivate(Player player, ItemStack held, PartRayTraceResult hit) {
            if (held.isEmpty() || !(held.getItem() instanceof IScrewdriver)) {
                if (!level().isClientSide) {
                    ITimerGuiLogic.openFromServer(player, this);
                }
                return true;
            }
            return false;
        }
    }

    public static class Counter extends RedstoneGatePart implements ICounterGuiLogic {

        private static final int KEY_VALUE = 30;
        private static final int KEY_MAX = 31;
        private static final int KEY_INCR = 32;
        private static final int KEY_DECR = 33;

        private int value = 0;
        private int max = 10;
        private int incr = 1;
        private int decr = 1;

        public Counter(GateType type) {
            super(type);
        }

        //region save/load
        @Override
        public void save(CompoundTag tag) {
            super.save(tag);
            tag.putInt("val", value);
            tag.putInt("max", max);
            tag.putInt("inc", incr);
            tag.putInt("dec", decr);
        }

        @Override
        public void load(CompoundTag tag) {
            super.load(tag);
            value = tag.getInt("val");
            max = tag.getInt("max");
            incr = tag.getInt("inc");
            decr = tag.getInt("dec");
        }

        @Override
        public void writeDesc(MCDataOutput packet) {
            super.writeDesc(packet);
            packet.writeInt(value);
            packet.writeInt(max);
            packet.writeInt(incr);
            packet.writeInt(decr);
        }

        @Override
        public void readDesc(MCDataInput packet) {
            super.readDesc(packet);
            value = packet.readInt();
            max = packet.readInt();
            incr = packet.readInt();
            decr = packet.readInt();
        }
        //endregion

        //region Packets

        @Override
        protected void read(MCDataInput packet, int key) {
            switch (key) {
                case KEY_VALUE:
                    value = packet.readInt();
                    break;
                case KEY_MAX:
                    max = packet.readInt();
                    break;
                case KEY_INCR:
                    incr = packet.readInt();
                    break;
                case KEY_DECR:
                    decr = packet.readInt();
                    break;
                default:
                    super.read(packet, key);
            }
        }

        protected void sendValueUpdate() {
            sendUpdate(KEY_VALUE, p -> p.writeInt(value));
        }

        protected void sendMaxUpdate() {
            sendUpdate(KEY_MAX, p -> p.writeInt(max));
        }

        protected void sendIncrUpdate() {
            sendUpdate(KEY_INCR, p -> p.writeInt(incr));
        }

        protected void sendDecrUpdate() {
            sendUpdate(KEY_DECR, p -> p.writeInt(decr));
        }
        //endregion

        //region ICounterGuiLogic
        @Override
        public int getCounterValue() {
            return value;
        }

        @Override
        public int getCounterMax() {
            return max;
        }

        @Override
        public int getCounterIncr() {
            return incr;
        }

        @Override
        public int getCounterDecr() {
            return decr;
        }

        @Override
        public void setCounterValue(int i) {
            int oldVal = value;
            value = Math.min(max, Math.max(0, i));
            if (value != oldVal) {
                tickSound();
                sendValueUpdate();
            }
        }

        @Override
        public void setCounterMax(int i) {
            int oldMax = max;
            max = Math.min(32767, Math.max(1, i));
            if (max != oldMax) {
                tickSound();
                sendMaxUpdate();
                int oldVal = value;
                value = Math.min(value, Math.max(0, i));
                if (value != oldVal) {
                    sendValueUpdate();
                    scheduleTick(2);
                }
            }
        }

        @Override
        public void setCounterIncr(int i) {
            int oldIncr = incr;
            incr = Math.min(max, Math.max(1, i));
            if (incr != oldIncr) {
                tickSound();
                sendIncrUpdate();
            }
        }

        @Override
        public void setCounterDecr(int i) {
            int oldDecr = decr;
            decr = Math.min(max, Math.max(1, i));
            if (decr != oldDecr) {
                tickSound();
                sendDecrUpdate();
            }
        }
        //endregion

        //region GateLogic

        @Override
        protected int outputMask(int shape) {
            return 5;
        }

        @Override
        protected int inputMask(int shape) {
            return 0xA;
        }

        @Override
        protected void gateLogicOnChange() {
            int oldInput = state() & 0xF;
            int newInput = getInput(0xA);
            if (shape() == 1) newInput = flipMaskZ(newInput);
            int high = newInput & ~oldInput;

            if ((high & 2) != 0) setCounterValue(value + incr);
            if ((high & 8) != 0) setCounterValue(value - decr);
            if (oldInput != newInput) {
                setState(state() & 0xF0 | newInput);
                onInputChange();
                scheduleTick(2);
            }
        }

        @Override
        protected boolean gateLogicCycleShape() {
            setShape(shape() == 0 ? 1 : 0);
            return true;
        }

        @Override
        protected void gateLogicOnScheduledTick() {
            int oldOutput = state();
            int newOutput = value == max ? 1 : value == 0 ? 4 : 0;
            if (newOutput != oldOutput) setState(state() & 0xF | newOutput << 4);
            if (newOutput != oldOutput) onOutputChange(5);
        }
        //endregion

        //region Gate render data
        @Override
        public boolean isPointerStarted() {
            return true;
        }

        @Override
        public int pointerValue() {
            return value;
        }

        @Override
        public int pointerMax() {
            return max;
        }
        //endregion

        @Override
        protected boolean gateLogicActivate(Player player, ItemStack held, PartRayTraceResult hit) {
            if (held.isEmpty() || !(held.getItem() instanceof IScrewdriver)) {
                if (!level().isClientSide) {
                    ICounterGuiLogic.openFromServer(player, this);
                }
                return true;
            }
            return false;
        }
    }

    public static class StateCell extends RedstoneTimerGatePart {

        public StateCell(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            int mask = 9;
            if (shape == 1) mask = flipMaskZ(mask);
            return mask;
        }

        @Override
        protected int inputMask(int shape) {
            int mask = 6;
            if (shape == 1) mask = flipMaskZ(mask);
            return mask;
        }

        @Override
        protected boolean gateLogicCycleShape() {
            setShape(shape() == 0 ? 1 : 0);
            return true;
        }

        @Override
        protected void gateLogicOnChange() {
            int oldInput = state() & 0xF;
            int newInput = getInput(0xE);
            if (oldInput != newInput) {
                setState(state() & 0xF0 | newInput);
                onInputChange();

                if (shape() == 1) newInput = flipMaskZ(newInput);
                if ((newInput & 4) != 0 && state2() == 0) {
                    setState2(1);
                    sendState2Update();
                    scheduleTick(2);
                }

                if (state2() != 0) {
                    if ((newInput & 6) != 0) {
                        resetPointer();
                    } else {
                        startPointer();
                    }
                }
            }
        }

        @Override
        protected void pointerTick() {
            resetPointer();
            if (!level().isClientSide) {
                setState2(0);
                sendState2Update();
                setState(0x10 | state() & 0xF);
                onOutputChange(outputMask(shape()));
                scheduleTick(2);
                tickSound();
            }
        }

        @Override
        protected void gateLogicOnScheduledTick() {
            int output = 0;
            if (state2() != 0) output = 8;
            if (shape() == 1) output = flipMaskZ(output);

            setState(output << 4 | state() & 0xF);
            onOutputChange(outputMask(shape()));
        }
    }

    public static class Synchronizer extends ComplexGatePart {

        public Synchronizer(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return 1;
        }

        @Override
        protected int inputMask(int shape) {
            return 0xE;
        }

        @Override
        protected void gateLogicOnChange() {
            int oldInput = state() & 0xF;
            int newInput = getInput(14);
            int high = newInput & ~oldInput;
            if (oldInput != newInput) {
                int oldValue = state2();

                setState(state() & 0xF0 | newInput);
                onInputChange();
                if ((newInput & 4) != 0) { setState2(0); } else {
                    if ((high & 2) != 0) setState2(state2() | 1); //right
                    if ((high & 8) != 0) setState2(state2() | 2); //left
                }
                if (right() && left()) scheduleTick(2);

                if (state2() != oldValue) sendState2Update();
            }
        }

        @Override
        protected void gateLogicOnScheduledTick() {
            int oldValue = state2();
            if (!pulsing() && right() && left()) {
                setState(state() | 1 << 4);
                onOutputChange(1);
                setState2(state2() | 4); //pulsing
                scheduleTick(2);
            } else if (pulsing()) {
                setState(state() & ~0x10);
                onOutputChange(1);
                setState2(0); //off
            }
            if (state2() != oldValue) sendState2Update();
        }

        protected boolean right() {
            return (state2() & 1) != 0;
        }

        protected boolean left() {
            return (state2() & 2) != 0;
        }

        protected boolean pulsing() {
            return (state2() & 4) != 0;
        }
    }

    public static class Comparator extends RedstoneGatePart implements NeighborTileChangePart {

        public static final int KEY_STATE2 = 20;

        private short lState2 = 0;

        public Comparator(GateType type) {
            super(type);
        }

        public int state2() {
            return lState2;
        }

        public void setState2(int state2) {
            this.lState2 = (short) state2;
        }

        //region save/load
        @Override
        public void save(CompoundTag tag) {
            super.save(tag);
            tag.putShort("state2", lState2);
        }

        @Override
        public void load(CompoundTag tag) {
            super.load(tag);
            lState2 = tag.getShort("state2");
        }
        //endregion

        //region Gate logic
        @Override
        protected int outputMask(int shape) {
            return 1;
        }

        @Override
        protected int inputMask(int shape) {
            return 0xE;
        }

        @Override
        protected boolean gateLogicCycleShape() {
            setShape(shape() == 0 ? 1 : 0);
            return true;
        }

        @Override
        protected boolean gateLogicCanConnect(int r) {
            return true; //TODO why?
        }

        @Override
        protected int getOutput(int r) {
            return r == 0 ? state2() & 0xF : 0;
        }
        //endregion

        private int calcInputA() {

            Direction absDir = Direction.values()[Rotation.rotateSide(getSide(), toAbsolute(2))];
            BlockPos pos1 = tile().getBlockPos().relative(absDir);
            BlockState state1 = level().getBlockState(pos1);

            int i = getDiodeSignal(2);

            if (state1.hasAnalogOutputSignal()) {
                i = state1.getAnalogOutputSignal(level(), pos1);
            } else if (i < 15 && state1.isRedstoneConductor(level(), pos1)) {
                BlockPos pos2 = pos1.relative(absDir);
                BlockState state2 = level().getBlockState(pos2);

                if (state2.hasAnalogOutputSignal()) {
                    i = Math.max(state2.getAnalogOutputSignal(level(), pos2), i);
                }

                ItemFrame entityitemframe = getItemFrame(level(), absDir, pos2);
                if (entityitemframe != null) {
                    i = Math.max(entityitemframe.getAnalogOutput(), i);
                }
            }
            return i;
        }

        /*
         * Emulates RedstoneDiodeBlock#getInputSignal(World, BlockPos, BlockState)
         */
        private int getDiodeSignal(int r) {
            Direction absDir = Direction.values()[Rotation.rotateSide(getSide(), toAbsolute(r))];
            BlockPos pos = tile().getBlockPos().relative(absDir);
            BlockState state = level().getBlockState(pos);

            // Check signal
            int i = level().getSignal(pos, absDir);
            if (i >= 15) return i;

            if (state.is(Blocks.REDSTONE_WIRE)) {
                i = Math.max(i, state.getValue(RedStoneWireBlock.POWER));
            }

            return i;
        }

        /*
         * Copied from ComparatorBlock#getItemFrame(World, Direction, BlockPos)
         */
        private ItemFrame getItemFrame(Level world, Direction facing, BlockPos pos) {
            List<ItemFrame> list = world.getEntitiesOfClass(ItemFrame.class, new AABB((double) pos.getX(), (double) pos.getY(), (double) pos.getZ(), (double) (pos.getX() + 1), (double) (pos.getY() + 1), (double) (pos.getZ() + 1)), (p_210304_1_) -> {
                return p_210304_1_ != null && p_210304_1_.getDirection() == facing;
            });
            return list.size() == 1 ? list.get(0) : null;
        }

        private int calcInput() {
            return getRedstoneInput(1) << 4 | calcInputA() << 8 | getAnalogRedstoneInput(3) << 12;
        }

        private int digitize(int analog) {
            int digital = 0;
            for (int i = 0; i < 4; i++) {
                if ((analog >> i * 4 & 0xF) > 0) digital |= 1 << i;
            }
            return digital;
        }

        @Override
        protected void gateLogicOnChange() {
            int oldInput = state2() & 0xFFF0;
            int newInput = calcInput();
            if (oldInput != newInput) {
                setState2(state2() & 0xF | newInput);
                setState(digitize(newInput | calcOutput()) | state() & 0xF0);
                onInputChange();
            }
            if ((state2() & 0xF) != calcOutput()) scheduleTick(2);
        }

        private int calcOutput() {
            int inputA = state2() >> 8 & 0xF;
            int inputB = Math.max(state2() >> 4 & 0xF, state2() >> 12 & 0xF);
            return shape() == 0 ? inputA > inputB ? inputA : 0 : Math.max(inputA - inputB, 0);
        }

        @Override
        protected void gateLogicOnScheduledTick() {
            int oldOutput = state2() & 0xF;
            int newOutput = calcOutput();
            if (oldOutput != newOutput) {
                setState2(state2() & 0xFFF0 | newOutput);
                setState(state() & 0xF | digitize(newOutput) << 4);
                onOutputChange(1);
            }
        }

        @Override
        public void onNeighborTileChanged(Direction side, boolean weak) {
            if (side.ordinal() == Rotation.rotateSide(getSide(), toAbsolute(2))) {
                onChange();
            }
        }

        @Override
        public boolean weakTileChanges() {
            return true;
        }
    }
}
