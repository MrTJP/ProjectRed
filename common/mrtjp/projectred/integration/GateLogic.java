package mrtjp.projectred.integration;

import static mrtjp.projectred.transmission.BasicWireUtils.oldBACK;
import static mrtjp.projectred.transmission.BasicWireUtils.oldFRONT;
import static mrtjp.projectred.transmission.BasicWireUtils.oldLEFT;
import static mrtjp.projectred.transmission.BasicWireUtils.oldRIGHT;

import java.util.Random;

import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.Configurator;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import codechicken.lib.packet.PacketCustom;
import dan200.computer.api.IComputerAccess;
import dan200.computer.api.ILuaContext;
import dan200.computer.api.IPeripheral;

public abstract class GateLogic {

    public World world; // undefined for Stateless logics

    // For redstone connections, inputs and outputs are signal strength 0-255.
    // For bundled connections, inputs and outputs are a bitmask, with bit 0 =
    // white.
    public abstract void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings);

    public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
        return 0;
    }

    public int configure(int gateSettings) {
        return gateSettings;
    }

    public void write(NBTTagCompound tag) {
    }

    public void read(NBTTagCompound tag) {
    }

    public boolean connectsToWire(int side) {
        return true;
    }

    /**
     * Used to check if side specified should connect to a bundled cable.
     */
    public static interface WithBundledConnections {
        public boolean connectsToBundled(int side);
    }

    /**
     * Gates that are considered stateless will not update every tick. Instead,
     * they will ONLY update when a neighbor changes. Examples are OR gate and
     * Timer. The OR gate only needs to update when its inputs changed to
     * reflect on the outputs. However, a Timer needs to update every tick to
     * spin the pointer, and if the time is up, pulse the output.
     */
    public static interface Stateless {
    }

    /**
     * Used to do something if gate has a rightclick action.
     */
    public static interface WithRightClickAction {
        public void onRightClick(EntityPlayer player, GatePart tile);
    }

    /**
     * Used for gates with Guis.
     */
    public static interface WithGui extends WithRightClickAction {
        public void openGui(EntityPlayer player, GatePart tile);

        public void handleButtonPressed(String action, GatePart tile);

        public void updateWatchers(GatePart tile);
    }

    /**
     * Implemented by GateLogics which can be used with the timer GUI. Allows
     * access to the timer interval (which is measured in ticks)
     */
    public interface GateLogicTimed {
        public int getInterval();

        public void setInterval(int i);
    }

    /**
     * Provides getter methods to a pointer's degree rotation and degrees per
     * tick.
     */
    public static interface WithPointer {
        public int getPointerPosition();

        public float getPointerSpeed();
    }

    /**
     * Used for gates with logic that needs to be aware of the world, such as
     * Light Sensors.
     */
    public static interface WorldStateBound {
        public void setWorldInfo(World world, int x, int y, int z);

        public boolean needsWorldInfo();
    }

    public static class AND extends GateLogic implements Stateless {
        // gateSettings 1: left input ignored
        // gateSettings 2: back input ignored
        // gateSettings 4: right input ignored
        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            boolean left = inputs[oldLEFT] != 0 || (gateSettings & 1) != 0;
            boolean back = inputs[oldBACK] != 0 || (gateSettings & 2) != 0;
            boolean right = inputs[oldRIGHT] != 0 || (gateSettings & 4) != 0;
            outputs[oldFRONT] = (short) (left && back && right ? 255 : 0);
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (outputs[oldFRONT] != 0 ? 1 : 0) | (inputs[oldBACK] != 0 ? 2 : 0) | (inputs[oldLEFT] != 0 || outputs[oldLEFT] != 0 ? 4 : 0) | (inputs[oldRIGHT] != 0 || outputs[oldRIGHT] != 0 ? 8 : 0) | (inputs[oldFRONT] != 0 || outputs[oldFRONT] != 0 ? 128 : 0) | (gateSettings << 4);
        }

        @Override
        public int configure(int gateSettings) {
            return (gateSettings + 1) & 7;
        }
    }

    public static class OR extends GateLogic implements Stateless {
        // gateSettings 1: left input ignored
        // gateSettings 2: back input ignored
        // gateSettings 4: right input ignored
        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            boolean left = inputs[oldLEFT] != 0 && (gateSettings & 1) == 0;
            boolean back = inputs[oldBACK] != 0 && (gateSettings & 2) == 0;
            boolean right = inputs[oldRIGHT] != 0 && (gateSettings & 4) == 0;
            outputs[oldFRONT] = (short) (left || back || right ? 255 : 0);
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (inputs[oldFRONT] != 0 || outputs[oldFRONT] != 0 ? 1 : 0) | (inputs[oldBACK] != 0 ? 2 : 0) | (inputs[oldLEFT] != 0 || outputs[oldLEFT] != 0 ? 4 : 0) | (inputs[oldRIGHT] != 0 || outputs[oldRIGHT] != 0 ? 8 : 0) | (outputs[oldFRONT] != 0 ? 16 : 0) | (gateSettings << 5);
        }

        @Override
        public int configure(int gateSettings) {
            return (gateSettings + 1) & 7;
        }
    }

    public static class NOT extends GateLogic implements Stateless {
        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            outputs[oldFRONT] = outputs[oldLEFT] = outputs[oldRIGHT] = (short) (inputs[oldBACK] != 0 ? 0 : 255);
            if ((gateSettings & 1) != 0)
                outputs[oldLEFT] = 0;
            if ((gateSettings & 2) != 0)
                outputs[oldFRONT] = 0;
            if ((gateSettings & 4) != 0)
                outputs[oldRIGHT] = 0;
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (inputs[oldFRONT] != 0 || outputs[oldFRONT] != 0 ? 1 : 0) | (inputs[oldBACK] != 0 ? 2 : 0) | (inputs[oldLEFT] != 0 || outputs[oldLEFT] != 0 ? 4 : 0) | (inputs[oldRIGHT] != 0 || outputs[oldRIGHT] != 0 ? 8 : 0) | (outputs[oldFRONT] != 0 || outputs[oldLEFT] != 0 || outputs[oldRIGHT] != 0 ? 16 : 0) | (gateSettings << 5);
        }

        @Override
        public int configure(int gateSettings) {
            return (gateSettings + 1) & 7;
        }
    }

    public static class RSLatch extends GateLogic implements Stateless {
        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            if (inputs[oldLEFT] != 0 && inputs[oldRIGHT] != 0)
                outputs[oldLEFT] = outputs[oldRIGHT] = 0;
            else if (inputs[oldLEFT] != 0) {
                outputs[oldLEFT] = (short) 255;
                outputs[oldRIGHT] = 0;
            } else if (inputs[oldRIGHT] != 0) {
                outputs[oldRIGHT] = (short) 255;
                outputs[oldLEFT] = 0;
            }

            outputs[oldFRONT] = !(inputs[oldRIGHT] != 0 || outputs[oldRIGHT] != 0) ? (short) 255 : 0;
            outputs[oldBACK] = !(inputs[oldLEFT] != 0 || outputs[oldLEFT] != 0) ? (short) 255 : 0;
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (inputs[oldLEFT] != 0 || outputs[oldLEFT] != 0 ? 1 : 0) | (inputs[oldRIGHT] != 0 || outputs[oldRIGHT] != 0 ? 2 : 0) | (outputs[oldFRONT] != 0 ? 4 : 0) | (outputs[oldBACK] != 0 ? 8 : 0);
        }
    }

    public static class ToggleLatch extends GateLogic implements WithRightClickAction {
        private boolean wasLeft, wasRight;
        private boolean state;

        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            if (inputs[oldLEFT] != 0 && !wasLeft)
                state = !state;
            if (inputs[oldRIGHT] != 0 && !wasRight)
                state = !state;
            wasLeft = inputs[oldLEFT] != 0;
            wasRight = inputs[oldRIGHT] != 0;
            outputs[oldFRONT] = !state ? (short) 255 : 0;
            outputs[oldBACK] = state ? (short) 255 : 0;
        }

        @Override
        public void write(NBTTagCompound tag) {
            tag.setBoolean("wasLeft", wasLeft);
            tag.setBoolean("wasRight", wasRight);
            tag.setBoolean("state", state);
        }

        @Override
        public void read(NBTTagCompound tag) {
            wasLeft = tag.getBoolean("wasLeft");
            wasRight = tag.getBoolean("wasRight");
            state = tag.getBoolean("state");
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (inputs[oldLEFT] != 0 ? 1 : 0) | (inputs[oldRIGHT] != 0 ? 2 : 0) | (outputs[oldFRONT] != 0 ? 4 : 0) | (outputs[oldBACK] != 0 ? 8 : 0);
        }

        @Override
        public void onRightClick(EntityPlayer player, GatePart tile) {
            state = !state;
            player.worldObj.playSoundEffect(player.posX + 0.5D, player.posY + 0.5D, player.posZ + 0.5D, "random.click", 0.3F, state ? 0.6F : 0.5F);
        }
    }

    public static class NOR extends GateLogic implements Stateless {
        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            boolean left = inputs[oldLEFT] != 0 && (gateSettings & 1) == 0;
            boolean back = inputs[oldBACK] != 0 && (gateSettings & 2) == 0;
            boolean right = inputs[oldRIGHT] != 0 && (gateSettings & 4) == 0;
            outputs[oldFRONT] = !left && !back && !right ? (short) 255 : 0;
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (inputs[oldFRONT] != 0 || outputs[oldFRONT] != 0 ? 1 : 0) | (inputs[oldBACK] != 0 ? 2 : 0) | (inputs[oldLEFT] != 0 ? 4 : 0) | (inputs[oldRIGHT] != 0 ? 8 : 0) | (outputs[oldFRONT] != 0 ? 16 : 0) | (gateSettings << 5);
        }

        @Override
        public int configure(int gateSettings) {
            return (gateSettings + 1) & 7;
        }
    }

    public static class NAND extends GateLogic implements Stateless {
        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            boolean left = inputs[oldLEFT] != 0 || (gateSettings & 1) != 0;
            boolean back = inputs[oldBACK] != 0 || (gateSettings & 2) != 0;
            boolean right = inputs[oldRIGHT] != 0 || (gateSettings & 4) != 0;
            outputs[oldFRONT] = !(back && left && right) ? (short) 255 : 0;
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (inputs[oldFRONT] != 0 || outputs[oldFRONT] != 0 ? 1 : 0) | (inputs[oldBACK] != 0 ? 2 : 0) | (inputs[oldLEFT] != 0 ? 4 : 0) | (inputs[oldRIGHT] != 0 ? 8 : 0) | (gateSettings << 4);
        }

        @Override
        public int configure(int gateSettings) {
            return (gateSettings + 1) & 7;
        }
    }

    public static class XOR extends GateLogic implements Stateless {
        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            outputs[oldFRONT] = (inputs[oldLEFT] != 0) ^ (inputs[oldRIGHT] != 0) ? (short) 255 : 0;
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (inputs[oldLEFT] != 0 ? 1 : 0) | (inputs[oldRIGHT] != 0 ? 2 : 0) | (outputs[oldFRONT] != 0 ? 4 : 0);
        }

        @Override
        public boolean connectsToWire(int side) {
            return super.connectsToWire(side) && side != oldBACK;
        }
    }

    public static class XNOR extends GateLogic implements Stateless {
        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            outputs[oldFRONT] = !((inputs[oldLEFT] != 0) ^ (inputs[oldRIGHT] != 0)) ? (short) 255 : 0;
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (inputs[oldLEFT] != 0 ? 1 : 0) | (inputs[oldRIGHT] != 0 ? 2 : 0) | (outputs[oldFRONT] != 0 ? 4 : 0) | (outputs[oldFRONT] != 0 || inputs[oldFRONT] != 0 ? 8 : 0);
        }

        @Override
        public boolean connectsToWire(int side) {
            return super.connectsToWire(side) && side != oldBACK;
        }
    }

    public static class Buffer extends GateLogic implements Stateless {
        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            outputs[oldFRONT] = outputs[oldLEFT] = outputs[oldRIGHT] = inputs[oldBACK];
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (outputs[oldFRONT] != 0 ? 1 : 0) | (inputs[oldBACK] != 0 ? 2 : 0) | (inputs[oldLEFT] != 0 || outputs[oldLEFT] != 0 ? 4 : 0) | (inputs[oldRIGHT] != 0 || outputs[oldRIGHT] != 0 ? 8 : 0) | (outputs[oldFRONT] != 0 || inputs[oldFRONT] != 0 ? 16 : 0);
        }
    }

    public static class Multiplexer extends GateLogic implements Stateless {
        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            outputs[oldFRONT] = inputs[oldBACK] != 0 ? inputs[oldLEFT] : inputs[oldRIGHT];
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (inputs[oldBACK] != 0 ? 1 : 0) | (inputs[oldLEFT] != 0 ? 2 : 0) | (inputs[oldRIGHT] != 0 ? 4 : 0) | (outputs[oldFRONT] != 0 ? 8 : 0) | (inputs[oldFRONT] != 0 || outputs[oldFRONT] != 0 ? 16 : 0);
        }
    }

    public static class Repeater extends GateLogic {
        private static int[] DELAYS = { 1, 2, 4, 8, 16, 32, 64, 128 };

        /*
         * When the input turns on, the output turns on after N ticks.
         * (state=false, timer=N) When the input turns off, the output turns off
         * after N ticks. (state=true, timer=N) If the repeater is turning on
         * and the input turns off, nothing happens. If the repeater is turning
         * off and the input turns on, the timer resets.
         */

        private boolean state;
        private int timer;

        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            if (inputs[oldBACK] != 0 && state) {
                timer = 0;
                return;
            }

            if ((inputs[oldBACK] != 0) != state && timer == 0) {
                // Account for 2 game ticks = 1 RS tick
                timer = DELAYS[gateSettings] * 2 - 2;

                if (timer == 0) {
                    state = !state;
                    outputs[oldFRONT] = state ? (short) 255 : 0;
                }
            }

            if (timer > 0) {
                timer--;
                if (timer == 0) {
                    state = !state;
                    outputs[oldFRONT] = state ? (short) 255 : 0;
                }
            }
        }

        @Override
        public void read(NBTTagCompound tag) {
            super.read(tag);
            timer = tag.getInteger("timer");
            state = tag.getBoolean("state");
        }

        @Override
        public void write(NBTTagCompound tag) {
            super.write(tag);
            tag.setInteger("timer", timer);
            tag.setBoolean("state", state);
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return gateSettings | (outputs[oldFRONT] != 0 ? 32768 : 0) | (inputs[oldBACK] != 0 ? 64 : 0);
        }

        @Override
        public int configure(int gateSettings) {
            return (gateSettings + 1) % DELAYS.length;
        }

        @Override
        public boolean connectsToWire(int side) {
            return side == oldFRONT || side == oldBACK;
        }
    }

    public static class Timer extends GateLogic implements WithGui, WithPointer, GateLogicTimed {

        public int intervalTicks = 20;
        public int ticksLeft;
        public boolean state, stopped;

        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            stopped = inputs[oldBACK] != 0;
            if (inputs[oldBACK] != 0) {
                state = true;
                outputs[oldFRONT] = outputs[oldLEFT] = outputs[oldRIGHT] = 0;
                ticksLeft = 0;
                return;
            }

            ticksLeft--;
            if (ticksLeft <= 0) {
                state = !state;
                outputs[oldFRONT] = outputs[oldLEFT] = outputs[oldRIGHT] = state ? (short) 255 : 0;
                ticksLeft = state ? 2 : intervalTicks - 2;
            }
        }

        @Override
        public void onRightClick(EntityPlayer player, GatePart tile) {
            openGui(player, tile);
        }

        @Override
        public void openGui(EntityPlayer player, GatePart tile) {
            PacketCustom packet = new PacketCustom(Configurator.integrationPacketChannel, IntegrationProxy.guiTimerOpen);
            packet.writeCoord(tile.x(), tile.y(), tile.z());
            packet.writeByte(tile.getFace());
            packet.writeInt(intervalTicks);
            packet.sendToPlayer(player);
        }

        @Override
        public void handleButtonPressed(String action, GatePart tile) {
            if (action.startsWith("-") || action.startsWith("+")) {
                int time = getInterval();
                time += Integer.parseInt(action.replace("+", ""));
                if (time < 4) {
                    time = 4;
                }
                if (time > 65535) {
                    time = 65535;
                }
                setInterval(time);
                updateWatchers(tile);
            }
        }

        @Override
        public void updateWatchers(GatePart tile) {
            Chunk c = tile.world().getChunkFromBlockCoords(tile.x(), tile.z());
            PacketCustom packet = new PacketCustom(Configurator.integrationPacketChannel, IntegrationProxy.guiTimerBroadcastChange);
            packet.writeCoord(tile.x(), tile.y(), tile.z());
            packet.writeByte(tile.getFace());
            packet.writeInt(intervalTicks);
            packet.sendToChunk(tile.world(), c.xPosition, c.zPosition);
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (outputs[oldFRONT] != 0 ? 1 : 0) | (outputs[oldFRONT] != 0 || inputs[oldFRONT] != 0 ? 32 : 0) | (outputs[oldLEFT] != 0 || inputs[oldLEFT] != 0 ? 2 : 0) | (outputs[oldRIGHT] != 0 || inputs[oldRIGHT] != 0 ? 8 : 0) | (inputs[oldBACK] != 0 ? 4 : 0) | (stopped ? 16 : 0);
        }

        @Override
        public int getPointerPosition() {
            return state || stopped ? 0 : 359 - (int) (ticksLeft * 360f / (intervalTicks - 2));
        }

        @Override
        public float getPointerSpeed() {
            return state || stopped ? 0 : 360f / (intervalTicks - 2);
        }

        @Override
        public void read(NBTTagCompound tag) {
            super.read(tag);

            intervalTicks = tag.getInteger("intv");
            ticksLeft = tag.getInteger("left");
            state = tag.getBoolean("state");
            stopped = tag.getBoolean("stopped");
        }

        @Override
        public void write(NBTTagCompound tag) {
            super.write(tag);

            tag.setInteger("intv", intervalTicks);
            tag.setInteger("left", ticksLeft);
            tag.setBoolean("state", state);
            tag.setBoolean("stopped", stopped);
        }

        @Override
        public int getInterval() {
            return intervalTicks;
        }

        @Override
        public void setInterval(int i) {
            intervalTicks = i;
            if (i > 0)
                ticksLeft %= intervalTicks;
            else
                ticksLeft = 0;
        }
    }

    public static class Counter extends GateLogic implements WithGui, WithPointer {

        int value = 0, max = 10, incr = 1, decr = 1;
        private boolean wasFront, wasBack;

        @Override
        public int getPointerPosition() {
            if (max == 0) {
                return 0;
            }
            return (value * 120) / max - 60;
        }

        @Override
        public float getPointerSpeed() {
            return 0;
        }

        @Override
        public void onRightClick(EntityPlayer player, GatePart tile) {
            openGui(player, tile);
        }

        @Override
        public void openGui(EntityPlayer player, GatePart tile) {
            PacketCustom packet = new PacketCustom(Configurator.integrationPacketChannel, IntegrationProxy.guiCounterOpen);
            packet.writeCoord(tile.x(), tile.y(), tile.z());
            packet.writeByte(tile.getFace());
            packet.writeShort(value);
            packet.writeShort(max);
            packet.writeShort(incr);
            packet.writeShort(decr);
            packet.sendToPlayer(player);
        }

        @Override
        public void handleButtonPressed(String action, GatePart tile) {
            int ID = Integer.parseInt(action.substring(0, 1));
            int delta = Integer.parseInt(action.substring(1).replace("+", ""));
            switch (ID) {
            case 0: // Max
                max = Math.min(32767, Math.max(1, max + delta));
                break;
            case 1: // Increment
                incr = Math.min(max, Math.max(1, incr + delta));
                break;
            case 2: // Decrement
                decr = Math.min(max, Math.max(1, decr + delta));
            }
            updateWatchers(tile);
        }

        @Override
        public void updateWatchers(GatePart tile) {
            PacketCustom packet = new PacketCustom(Configurator.integrationPacketChannel, IntegrationProxy.guiCounterBroadcastChange);
            packet.writeCoord(tile.x(), tile.y(), tile.z());
            packet.writeByte(tile.getFace());
            packet.writeShort(value);
            packet.writeShort(max);
            packet.writeShort(incr);
            packet.writeShort(decr);
            Chunk c = tile.world().getChunkFromBlockCoords(tile.x(), tile.z());
            packet.sendToChunk(tile.world(), c.xPosition, c.zPosition);
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (inputs[oldFRONT] != 0 ? 1 : 0) | (inputs[oldBACK] != 0 ? 2 : 0) | (outputs[oldLEFT] != 0 ? 4 : 0) | (outputs[oldRIGHT] != 0 ? 8 : 0) | (inputs[oldLEFT] != 0 || outputs[oldLEFT] != 0 ? 16 : 0) | (inputs[oldRIGHT] != 0 || outputs[oldRIGHT] != 0 ? 32 : 0)
            // render state is truncated to 16 bits, but this
            // causes the rendering to update when the pointer moves
            | (getPointerPosition() << 20);
        }

        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {

            if (inputs[oldFRONT] != 0 && !wasFront)
                value = Math.max(0, value - decr);

            if (inputs[oldBACK] != 0 && !wasBack)
                value = Math.min(max, value + incr);

            outputs[oldLEFT] = value == 0 ? (short) 255 : 0;
            outputs[oldRIGHT] = value == max ? (short) 255 : 0;

            wasFront = inputs[oldFRONT] != 0;
            wasBack = inputs[oldBACK] != 0;
        }

        @Override
        public void read(NBTTagCompound tag) {
            super.read(tag);
            value = tag.getInteger("cur");
            max = tag.getInteger("max");
            incr = tag.getInteger("+");
            decr = tag.getInteger("-");
            wasFront = tag.getBoolean("front");
            wasBack = tag.getBoolean("back");
        }

        @Override
        public void write(NBTTagCompound tag) {
            super.write(tag);
            tag.setInteger("cur", value);
            tag.setInteger("max", max);
            tag.setInteger("+", incr);
            tag.setInteger("-", decr);
            tag.setBoolean("front", wasFront);
            tag.setBoolean("back", wasBack);
        }
    }

    public static class Sequencer extends GateLogic implements WithGui, WithPointer, GateLogicTimed {

        public int intervalTicks = 20;
        public int ticksLeft;
        public int state;

        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            ticksLeft--;
            if (ticksLeft <= 0) {
                ticksLeft = intervalTicks;
                state++;
                if (state > 3) {
                    state = 0;
                }
            }
            outputs[oldFRONT] = state == 0 ? (short) 255 : 0;
            outputs[oldRIGHT] = state == 1 ? (short) 255 : 0;
            outputs[oldBACK] = state == 2 ? (short) 255 : 0;
            outputs[oldLEFT] = state == 3 ? (short) 255 : 0;
        }

        @Override
        public void onRightClick(EntityPlayer player, GatePart tile) {
            openGui(player, tile);
        }

        @Override
        public void openGui(EntityPlayer player, GatePart tile) {
            PacketCustom packet = new PacketCustom(Configurator.integrationPacketChannel, IntegrationProxy.guiTimerOpen);
            packet.writeCoord(tile.x(), tile.y(), tile.z());
            packet.writeByte(tile.getFace());
            packet.writeInt(intervalTicks);
            packet.sendToPlayer(player);
        }

        @Override
        public void handleButtonPressed(String action, GatePart tile) {
            if (action.startsWith("-") || action.startsWith("+")) {
                int time = getInterval();
                time += Integer.parseInt(action.replace("+", ""));
                if (time < 4) {
                    time = 4;
                }
                if (time > 65535) {
                    time = 65535;
                }
                setInterval(time);
                updateWatchers(tile);
            }
        }

        @Override
        public void updateWatchers(GatePart tile) {
            Chunk c = tile.world().getChunkFromBlockCoords(tile.x(), tile.z());
            PacketCustom packet = new PacketCustom(Configurator.integrationPacketChannel, IntegrationProxy.guiTimerBroadcastChange);
            packet.writeCoord(tile.x(), tile.y(), tile.z());
            packet.writeByte(tile.getFace());
            packet.writeInt(intervalTicks);
            packet.sendToChunk(tile.world(), c.xPosition, c.zPosition);
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return state;
        }

        @Override
        public int getPointerPosition() {
            return 89 - (int) (ticksLeft * 90f / intervalTicks) + state * 90;
        }

        @Override
        public float getPointerSpeed() {
            return 90f / intervalTicks;
        }

        @Override
        public void read(NBTTagCompound tag) {
            super.read(tag);

            intervalTicks = tag.getInteger("intv");
            ticksLeft = tag.getInteger("left");
            state = tag.getByte("state");
        }

        @Override
        public void write(NBTTagCompound tag) {
            super.write(tag);

            tag.setInteger("intv", intervalTicks);
            tag.setInteger("left", ticksLeft);
            tag.setByte("state", (byte) state);
        }

        @Override
        public int getInterval() {
            return intervalTicks;
        }

        @Override
        public void setInterval(int i) {
            intervalTicks = i;
            if (i > 0)
                ticksLeft %= intervalTicks;
            else
                ticksLeft = 0;
        }
    }

    public static class PulseFormer extends GateLogic {
        private boolean prevInput;
        private int ticksLeft;

        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            if (inputs[oldBACK] != 0 && !prevInput) {
                outputs[oldFRONT] = (short) 255;
                ticksLeft = 3;
            }
            if (ticksLeft > 0) {
                ticksLeft--;
                if (ticksLeft == 0)
                    outputs[oldFRONT] = 0;
            }
            prevInput = inputs[oldBACK] != 0;
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (inputs[oldBACK] != 0 ? 1 : 0) | (outputs[oldFRONT] != 0 ? 2 : 0) | (ticksLeft > 0 ? 4 : 0) | (outputs[oldFRONT] != 0 || inputs[oldFRONT] != 0 ? 8 : 0);
        }

        @Override
        public void write(NBTTagCompound tag) {
            super.write(tag);
            tag.setByte("ticksLeft", (byte) ticksLeft);
        }

        @Override
        public void read(NBTTagCompound tag) {
            super.read(tag);
            ticksLeft = tag.getByte("ticksLeft");
        }
    }

    public static class Randomizer extends GateLogic {
        private int ticksLeft;
        private Random random = new Random();

        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            if (inputs[oldBACK] != 0 && ticksLeft == 0) {
                ticksLeft = 20;
                outputs[oldFRONT] = random.nextBoolean() ? (short) 255 : 0;
                outputs[oldLEFT] = random.nextBoolean() ? (short) 255 : 0;
                outputs[oldRIGHT] = random.nextBoolean() ? (short) 255 : 0;
            }

            if (inputs[oldBACK] == 0)
                ticksLeft = 0;

            if (ticksLeft > 0)
                ticksLeft--;
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (inputs[oldBACK] != 0 ? 1 : 0) | (outputs[oldLEFT] != 0 ? 2 : 0) | (outputs[oldRIGHT] != 0 ? 4 : 0) | (outputs[oldFRONT] != 0 ? 8 : 0) | (outputs[oldFRONT] != 0 || inputs[oldFRONT] != 0 ? 16 : 0) | (outputs[oldLEFT] != 0 || inputs[oldLEFT] != 0 ? 32 : 0) | (outputs[oldRIGHT] != 0 || inputs[oldRIGHT] != 0 ? 64 : 0);
        }

        @Override
        public void write(NBTTagCompound tag) {
            super.write(tag);
            tag.setByte("ticksLeft", (byte) ticksLeft);
        }

        @Override
        public void read(NBTTagCompound tag) {
            super.read(tag);
            ticksLeft = tag.getByte("ticksLeft");
        }
    }

    public static class StateCell extends GateLogic implements WithGui, WithPointer, GateLogicTimed {

        private int intervalTicks = 20, ticksLeft, pulseTicks;
        private boolean timing, paused;

        @Override
        public int getPointerPosition() {
            if (!timing)
                return 0;
            return 44 - (int) (ticksLeft * 45f / intervalTicks);
        }

        @Override
        public float getPointerSpeed() {
            if (!timing || paused)
                return 0;
            return 45f / intervalTicks;
        }

        @Override
        public void onRightClick(EntityPlayer player, GatePart tile) {
            openGui(player, tile);
        }

        @Override
        public void openGui(EntityPlayer player, GatePart tile) {
            PacketCustom packet = new PacketCustom(Configurator.integrationPacketChannel, IntegrationProxy.guiTimerOpen);
            packet.writeCoord(tile.x(), tile.y(), tile.z());
            packet.writeByte(tile.getFace());
            packet.writeInt(intervalTicks);
            packet.sendToPlayer(player);
        }

        @Override
        public void handleButtonPressed(String action, GatePart tile) {
            if (action.startsWith("-") || action.startsWith("+")) {
                int time = getInterval();
                time += Integer.parseInt(action.replace("+", ""));
                if (time < 4) {
                    time = 4;
                }
                if (time > 65535) {
                    time = 65535;
                }
                setInterval(time);
                updateWatchers(tile);
            }
        }

        @Override
        public void updateWatchers(GatePart tile) {
            Chunk c = tile.world().getChunkFromBlockCoords(tile.x(), tile.z());
            PacketCustom packet = new PacketCustom(Configurator.integrationPacketChannel, IntegrationProxy.guiTimerBroadcastChange);
            packet.writeCoord(tile.x(), tile.y(), tile.z());
            packet.writeByte(tile.getFace());
            packet.writeInt(intervalTicks);
            packet.sendToChunk(tile.world(), c.xPosition, c.zPosition);
        }

        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            if (inputs[oldLEFT] != 0 && !timing) {
                timing = true;
                ticksLeft = intervalTicks;
            }
            paused = inputs[oldLEFT] != 0 || inputs[oldBACK] != 0;
            if (timing && !paused) {
                ticksLeft--;
                if (ticksLeft <= 0) {
                    pulseTicks = 2;
                    timing = false;
                }
            }
            outputs[oldFRONT] = timing ? (short) 255 : 0;
            outputs[oldRIGHT] = pulseTicks > 0 ? (short) 255 : 0;
            if (pulseTicks > 0)
                pulseTicks--;
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (outputs[oldFRONT] != 0 || inputs[oldFRONT] != 0 ? 1 : 0) | (inputs[oldBACK] != 0 ? 2 : 0) | (inputs[oldLEFT] != 0 ? 4 : 0) | (outputs[oldRIGHT] != 0 ? 8 : 0) | (timing ? 16 : 0) | (paused ? 32 : 0) | (inputs[oldRIGHT] != 0 || outputs[oldRIGHT] != 0 ? 64 : 0);
        }

        @Override
        public int getInterval() {
            return intervalTicks;
        }

        @Override
        public void setInterval(int i) {
            intervalTicks = i;
            if (ticksLeft > i)
                ticksLeft = i;
        }

        @Override
        public void read(NBTTagCompound tag) {
            super.read(tag);

            intervalTicks = tag.getInteger("intv");
            ticksLeft = tag.getInteger("left");
            pulseTicks = tag.getByte("pulse");
            timing = tag.getBoolean("timing");
            paused = tag.getBoolean("paused");
        }

        @Override
        public void write(NBTTagCompound tag) {
            super.write(tag);

            tag.setInteger("intv", intervalTicks);
            tag.setInteger("left", ticksLeft);
            tag.setByte("pulse", (byte) pulseTicks);
            tag.setBoolean("timing", timing);
            tag.setBoolean("paused", paused);
        }
    }

    public static class Synchronizer extends GateLogic {
        private boolean leftLatch, rightLatch, wasLeft, wasRight;
        private int pulseTicks;

        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            if (inputs[oldLEFT] == 0 && wasLeft)
                leftLatch = true;
            if (inputs[oldRIGHT] == 0 && wasRight)
                rightLatch = true;
            if (inputs[oldBACK] != 0)
                leftLatch = rightLatch = false;
            if (leftLatch && rightLatch) {
                pulseTicks = 2;
                leftLatch = rightLatch = false;
            }

            wasLeft = inputs[oldLEFT] != 0;
            wasRight = inputs[oldRIGHT] != 0;

            if (pulseTicks > 0) {
                outputs[oldFRONT] = (short) 255;
                pulseTicks--;
            } else
                outputs[oldFRONT] = 0;
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (inputs[oldLEFT] != 0 ? 1 : 0) | (inputs[oldRIGHT] != 0 ? 2 : 0) | (inputs[oldBACK] != 0 ? 4 : 0) | (outputs[oldFRONT] != 0 ? 8 : 0) | (leftLatch ? 16 : 0) | (rightLatch ? 32 : 0) | (outputs[oldFRONT] != 0 || inputs[oldFRONT] != 0 ? 64 : 0);
        }

        @Override
        public void write(NBTTagCompound tag) {
            super.write(tag);

            tag.setByte("f", (byte) (pulseTicks | (leftLatch ? 4 : 0) | (rightLatch ? 8 : 0) | (wasLeft ? 16 : 0) | (wasRight ? 32 : 0)));
        }

        @Override
        public void read(NBTTagCompound tag) {
            super.read(tag);

            byte f = tag.getByte("f");
            pulseTicks = f & 3;
            leftLatch = (f & 4) != 0;
            rightLatch = (f & 8) != 0;
            wasLeft = (f & 16) != 0;
            wasRight = (f & 32) != 0;
        }
    }

    public static class DLatch extends GateLogic implements Stateless {
        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            if (inputs[oldRIGHT] != 0) {
                outputs[oldFRONT] = outputs[oldLEFT] = (short) (inputs[oldBACK] != 0 ? 255 : 0);
            }
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (inputs[oldBACK] != 0 ? 1 : 0) | (inputs[oldRIGHT] != 0 ? 2 : 0) | (outputs[oldFRONT] != 0 || inputs[oldFRONT] != 0 ? 4 : 0) | (outputs[oldLEFT] != 0 || inputs[oldLEFT] != 0 ? 8 : 0);
        }
    }

    public static class DFlop extends GateLogic {
        private boolean clockWasOn;

        @Override
        public void write(NBTTagCompound tag) {
            tag.setBoolean("clock", clockWasOn);
        }

        @Override
        public void read(NBTTagCompound tag) {
            clockWasOn = tag.getBoolean("clock");
        }

        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            if (inputs[oldRIGHT] != 0 && !clockWasOn) {
                outputs[oldFRONT] = outputs[oldLEFT] = (short) (inputs[oldBACK] != 0 ? 255 : 0);
            }
            clockWasOn = inputs[oldRIGHT] != 0;
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (inputs[oldBACK] != 0 ? 1 : 0) | (inputs[oldRIGHT] != 0 ? 2 : 0) | (outputs[oldFRONT] != 0 || inputs[oldFRONT] != 0 ? 4 : 0) | (outputs[oldLEFT] != 0 || inputs[oldLEFT] != 0 ? 8 : 0);
        }
    }

    public static class BundledLatch extends GateLogic implements Stateless, WithBundledConnections {
        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            if (inputs[oldRIGHT] != 0) {
                outputs[oldFRONT] = inputs[oldBACK];
            }
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (inputs[oldRIGHT] != 0 ? 1 : 0);
        }

        @Override
        public boolean connectsToWire(int side) {
            return side == oldRIGHT;
        }

        @Override
        public boolean connectsToBundled(int side) {
            return side == oldFRONT || side == oldBACK;
        }

    }

    public static class BundledRelay extends GateLogic implements Stateless, WithBundledConnections {
        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            outputs[oldFRONT] = (inputs[oldRIGHT] != 0) ? inputs[oldBACK] : 0;
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (inputs[oldRIGHT] != 0 ? 1 : 0);
        }

        @Override
        public boolean connectsToWire(int side) {
            return side == oldRIGHT;
        }

        @Override
        public boolean connectsToBundled(int side) {
            return side == oldFRONT || side == oldBACK;
        }

    }

    public static class BundledMultiplexer extends GateLogic implements Stateless, WithBundledConnections {
        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            outputs[oldFRONT] = (inputs[oldBACK] != 0) ? inputs[oldRIGHT] : inputs[oldLEFT];
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (inputs[oldBACK] != 0 ? 1 : 0);
        }

        @Override
        public boolean connectsToWire(int side) {
            return side == oldBACK;
        }

        @Override
        public boolean connectsToBundled(int side) {
            return side == oldFRONT || side == oldLEFT || side == oldRIGHT;
        }
    }

    public static class LightSensor extends GateLogic implements WorldStateBound {
        World w;
        int x;
        int y;
        int z;
        int threshold;

        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            threshold = gateSettings;
            // Dont do this calculation every tick.
            if (w != null && w.getTotalWorldTime() % 8 == 0) {
                int brightness = BasicUtils.getAbsoluteBrightness(w, x, y, z);
                if (threshold < 5) {
                    outputs[oldBACK] = (short) (brightness >= (threshold * 3 + 3) ? 255 : 0);
                } else {
                    outputs[oldBACK] = (short) (brightness * 17 > 255 ? 255 : brightness * 17);
                }
            }
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (inputs[oldBACK] != 0 || outputs[oldBACK] != 0 ? 1 : 0) | (gateSettings == 0 ? 2 : 0) | (gateSettings == 1 ? 4 : 0) | (gateSettings == 2 ? 8 : 0) | (gateSettings == 3 ? 16 : 0) | (gateSettings == 4 ? 32 : 0) | (gateSettings == 5 ? 64 : 0);
        }

        @Override
        public void setWorldInfo(World world, int x, int y, int z) {
            w = world;
            this.x = x;
            this.y = y;
            this.z = z;
        }

        @Override
        public boolean needsWorldInfo() {
            return w == null;
        }

        @Override
        public int configure(int gateSettings) {
            gateSettings++;
            return (gateSettings > 5 ? 0 : gateSettings);
        }

        public boolean connectsToWire(int side) {
            return side == oldBACK;
        }
    }

    public static class RainSensor extends GateLogic implements WorldStateBound {
        World w;
        int x;
        int y;
        int z;

        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            // Dont do this calculation every tick.
            if (w != null && w.getTotalWorldTime() % 8 == 0) {
                outputs[oldBACK] = (short) (w.isRaining() && BasicUtils.canBlockSeeSky(w, x, y, z) ? 255 : 0);
            }
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            return (inputs[oldBACK] != 0 || outputs[oldBACK] != 0 ? 1 : 0);
        }

        @Override
        public void setWorldInfo(World world, int x, int y, int z) {
            w = world;
            this.x = x;
            this.y = y;
            this.z = z;
        }

        @Override
        public boolean needsWorldInfo() {
            return w == null;
        }

        public boolean connectsToWire(int side) {
            return side == oldBACK;
        }
    }

    public static class CCIOExpander extends GateLogic implements Stateless, WithBundledConnections, IPeripheral {

        private boolean isConnected = false;
        private byte[] cachedInputs = new byte[16];
        private byte[] cachedOutputs = new byte[16];

        @Override
        public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
            cachedInputs = new byte[16];
            short inputMask = inputs[oldBACK];
            for (int k = 0; k < 16; k++) {
                cachedInputs[k] = ((inputMask & 1) != 0) ? (byte) 255 : 0;
                inputMask >>= 1;
            }

            short outputMask = 0;
            for (int k = 15; k >= 0; k--) {
                outputMask <<= 1;
                if (cachedOutputs[k] != 0) {
                    outputMask |= 1;
                }
            }
            outputs[oldBACK] = outputMask;
        }

        @Override
        public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
            int outputMask = 0;
            for (int k = 15; k >= 0; k--) {
                outputMask <<= 1;
                if (cachedOutputs[k] != 0) {
                    outputMask |= 1;
                }
            }
            if (isConnected) {
                
            }
            return outputMask;
        }

        @Override
        public boolean connectsToBundled(int side) {
            return side == oldBACK;
        }

        @Override
        public String getType() {
            // Handled by gate
            return null;
        }

        @Override
        public String[] getMethodNames() {
            return new String[] { "help", "setOutput", "testInput", "allOff", "allOn" };
        }

        @Override
        public Object[] callMethod(IComputerAccess computer, ILuaContext context, int method, Object[] arguments) throws Exception {
            switch (method) {
            case 0:
                return new Object[] { "Available Commands:", "setOutput(int color 0-15, boolean state)", "testInput(int color 0-15)", "allOff()", "allOn()" };
            case 1:
                if (arguments.length != 2 || !(arguments[0] instanceof Double) || !(arguments[1] instanceof Boolean)) {
                    throw new Exception("invalid arguments");
                }
                double color = (Double) arguments[0];
                boolean on = (Boolean) arguments[1];
                if (color > 15 || color < 0) {
                    throw new Exception("invalid color");
                }
                cachedOutputs[(int) color] = (byte) (on ? 255 : 0);
                return null;
            case 2:
                if (arguments.length != 1 || !(arguments[0] instanceof Double)) {
                    throw new Exception("invalid arguments");
                }
                double testcolor = (Double) arguments[0];
                if (testcolor > 15 || testcolor < 0) {
                    throw new Exception("invalid color");
                }
                return new Object[] { cachedInputs[(int) testcolor] != 0 ? true : false };
            case 3:
                for (int i = 0; i < 16; i++) {
                    cachedOutputs[i] = 0;
                }
                return null;
            case 4:
                for (int i = 0; i < 16; i++) {
                    cachedOutputs[i] = (byte) 255;
                }
                return null;
            default:
                throw new Exception("invalid call");
            }
        }

        @Override
        public boolean canAttachToSide(int rel) {
            return rel == oldFRONT;
        }

        @Override
        public void attach(IComputerAccess computer) {
            cachedOutputs = new byte[16];
            isConnected = true;
        }

        @Override
        public void detach(IComputerAccess computer) {
            cachedOutputs = new byte[16];
            isConnected = false;
        }
    }
}
