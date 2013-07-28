package mrtjp.projectred.multipart.wiring.gates;

import static mrtjp.projectred.utils.BasicWireUtils.BACK;
import static mrtjp.projectred.utils.BasicWireUtils.FRONT;
import static mrtjp.projectred.utils.BasicWireUtils.LEFT;
import static mrtjp.projectred.utils.BasicWireUtils.RIGHT;

import java.util.Random;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.integration.TileGate;
import mrtjp.projectred.network.GuiIDs;
import mrtjp.projectred.utils.BasicUtils;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.world.World;

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

	public boolean connectsToDirection(int side) {
		return true;
	}

	/**
	 * Used to check if side specified should connect to a bundled cable.
	 */
	public static interface WithBundledConnections {
		public boolean isBundledConnection(int side);
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
	 * Marker interface for gates which can be horizontally flipped by
	 * shift-clicking with a screwdriver. Disabled, as gates can no longer
	 * render flipped without substantial effort.
	 */
	public static interface Flippable {
	}

	/**
	 * Used to do something if gate has a rightclick action such as a gui.
	 */
	public static interface WithRightClickAction {
		public void onRightClick(EntityPlayer ply, TileGate tile);
	}

	/**
	 * Used to ask logic about pointer conditions.
	 */
	public static interface WithPointer {
		public int getPointerPosition(); // degrees

		public float getPointerSpeed(); // degrees per tick
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
			boolean left = inputs[LEFT] != 0 || (gateSettings & 1) != 0;
			boolean back = inputs[BACK] != 0 || (gateSettings & 2) != 0;
			boolean right = inputs[RIGHT] != 0 || (gateSettings & 4) != 0;
			outputs[FRONT] = (short) (left && back && right ? 255 : 0);
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (outputs[FRONT] != 0 ? 1 : 0) | (inputs[BACK] != 0 ? 2 : 0) | (inputs[LEFT] != 0 || outputs[LEFT] != 0 ? 4 : 0) | (inputs[RIGHT] != 0 || outputs[RIGHT] != 0 ? 8 : 0) | (inputs[FRONT] != 0 || outputs[FRONT] != 0 ? 128 : 0) | (gateSettings << 4);
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
			boolean left = inputs[LEFT] != 0 && (gateSettings & 1) == 0;
			boolean back = inputs[BACK] != 0 && (gateSettings & 2) == 0;
			boolean right = inputs[RIGHT] != 0 && (gateSettings & 4) == 0;
			outputs[FRONT] = (short) (left || back || right ? 255 : 0);
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (inputs[FRONT] != 0 || outputs[FRONT] != 0 ? 1 : 0) | (inputs[BACK] != 0 ? 2 : 0) | (inputs[LEFT] != 0 || outputs[LEFT] != 0 ? 4 : 0) | (inputs[RIGHT] != 0 || outputs[RIGHT] != 0 ? 8 : 0) | (outputs[FRONT] != 0 ? 16 : 0) | (gateSettings << 5);
		}

		@Override
		public int configure(int gateSettings) {
			return (gateSettings + 1) & 7;
		}
	}

	public static class NOT extends GateLogic implements Stateless {
		@Override
		public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
			outputs[FRONT] = outputs[LEFT] = outputs[RIGHT] = (short) (inputs[BACK] != 0 ? 0 : 255);
			if ((gateSettings & 1) != 0)
				outputs[LEFT] = 0;
			if ((gateSettings & 2) != 0)
				outputs[FRONT] = 0;
			if ((gateSettings & 4) != 0)
				outputs[RIGHT] = 0;
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (inputs[FRONT] != 0 || outputs[FRONT] != 0 ? 1 : 0) | (inputs[BACK] != 0 ? 2 : 0) | (inputs[LEFT] != 0 || outputs[LEFT] != 0 ? 4 : 0) | (inputs[RIGHT] != 0 || outputs[RIGHT] != 0 ? 8 : 0) | (outputs[FRONT] != 0 || outputs[LEFT] != 0 || outputs[RIGHT] != 0 ? 16 : 0) | (gateSettings << 5);
		}

		@Override
		public int configure(int gateSettings) {
			return (gateSettings + 1) & 7;
		}
	}

	public static class RSLatch extends GateLogic implements Stateless, Flippable {
		@Override
		public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
			if (inputs[LEFT] != 0 && inputs[RIGHT] != 0)
				outputs[LEFT] = outputs[RIGHT] = 0;
			else if (inputs[LEFT] != 0) {
				outputs[LEFT] = (short) 255;
				outputs[RIGHT] = 0;
			} else if (inputs[RIGHT] != 0) {
				outputs[RIGHT] = (short) 255;
				outputs[LEFT] = 0;
			}

			outputs[FRONT] = !(inputs[RIGHT] != 0 || outputs[RIGHT] != 0) ? (short) 255 : 0;
			outputs[BACK] = !(inputs[LEFT] != 0 || outputs[LEFT] != 0) ? (short) 255 : 0;
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (inputs[LEFT] != 0 || outputs[LEFT] != 0 ? 1 : 0) | (inputs[RIGHT] != 0 || outputs[RIGHT] != 0 ? 2 : 0) | (outputs[FRONT] != 0 ? 4 : 0) | (outputs[BACK] != 0 ? 8 : 0);
		}
	}

	public static class ToggleLatch extends GateLogic implements Flippable, WithRightClickAction {
		private boolean wasLeft, wasRight;
		private boolean state;

		@Override
		public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
			if (inputs[LEFT] != 0 && !wasLeft)
				state = !state;
			if (inputs[RIGHT] != 0 && !wasRight)
				state = !state;
			wasLeft = inputs[LEFT] != 0;
			wasRight = inputs[RIGHT] != 0;
			outputs[FRONT] = !state ? (short) 255 : 0;
			outputs[BACK] = state ? (short) 255 : 0;
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
			return (inputs[LEFT] != 0 ? 1 : 0) | (inputs[RIGHT] != 0 ? 2 : 0) | (outputs[FRONT] != 0 ? 4 : 0) | (outputs[BACK] != 0 ? 8 : 0);
		}

		@Override
		public void onRightClick(EntityPlayer ply, TileGate tile) {
			state = !state;
		}
	}

	public static class NOR extends GateLogic implements Stateless {
		@Override
		public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
			boolean left = inputs[LEFT] != 0 && (gateSettings & 1) == 0;
			boolean back = inputs[BACK] != 0 && (gateSettings & 2) == 0;
			boolean right = inputs[RIGHT] != 0 && (gateSettings & 4) == 0;
			outputs[FRONT] = !left && !back && !right ? (short) 255 : 0;
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (inputs[FRONT] != 0 || outputs[FRONT] != 0 ? 1 : 0) | (inputs[BACK] != 0 ? 2 : 0) | (inputs[LEFT] != 0 ? 4 : 0) | (inputs[RIGHT] != 0 ? 8 : 0) | (outputs[FRONT] != 0 ? 16 : 0) | (gateSettings << 5);
		}

		@Override
		public int configure(int gateSettings) {
			return (gateSettings + 1) & 7;
		}
	}

	public static class NAND extends GateLogic implements Stateless {
		@Override
		public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
			boolean left = inputs[LEFT] != 0 || (gateSettings & 1) != 0;
			boolean back = inputs[BACK] != 0 || (gateSettings & 2) != 0;
			boolean right = inputs[RIGHT] != 0 || (gateSettings & 4) != 0;
			outputs[FRONT] = !(back && left && right) ? (short) 255 : 0;
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (inputs[FRONT] != 0 || outputs[FRONT] != 0 ? 1 : 0) | (inputs[BACK] != 0 ? 2 : 0) | (inputs[LEFT] != 0 ? 4 : 0) | (inputs[RIGHT] != 0 ? 8 : 0) | (gateSettings << 4);
		}

		@Override
		public int configure(int gateSettings) {
			return (gateSettings + 1) & 7;
		}
	}

	public static class XOR extends GateLogic implements Stateless {
		@Override
		public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
			outputs[FRONT] = (inputs[LEFT] != 0) ^ (inputs[RIGHT] != 0) ? (short) 255 : 0;
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (inputs[LEFT] != 0 ? 1 : 0) | (inputs[RIGHT] != 0 ? 2 : 0) | (outputs[FRONT] != 0 ? 4 : 0);
		}

		@Override
		public boolean connectsToDirection(int side) {
			return super.connectsToDirection(side) && side != BACK;
		}
	}

	public static class XNOR extends GateLogic implements Stateless {
		@Override
		public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
			outputs[FRONT] = !((inputs[LEFT] != 0) ^ (inputs[RIGHT] != 0)) ? (short) 255 : 0;
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (inputs[LEFT] != 0 ? 1 : 0) | (inputs[RIGHT] != 0 ? 2 : 0) | (outputs[FRONT] != 0 ? 4 : 0) | (outputs[FRONT] != 0 || inputs[FRONT] != 0 ? 8 : 0);
		}

		@Override
		public boolean connectsToDirection(int side) {
			return super.connectsToDirection(side) && side != BACK;
		}
	}

	public static class Buffer extends GateLogic implements Stateless {
		@Override
		public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
			outputs[FRONT] = outputs[LEFT] = outputs[RIGHT] = inputs[BACK];
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (outputs[FRONT] != 0 ? 1 : 0) | (inputs[BACK] != 0 ? 2 : 0) | (inputs[LEFT] != 0 || outputs[LEFT] != 0 ? 4 : 0) | (inputs[RIGHT] != 0 || outputs[RIGHT] != 0 ? 8 : 0) | (outputs[FRONT] != 0 || inputs[FRONT] != 0 ? 16 : 0);
		}
	}

	public static class Multiplexer extends GateLogic implements Stateless, Flippable {
		@Override
		public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
			outputs[FRONT] = inputs[BACK] != 0 ? inputs[LEFT] : inputs[RIGHT];
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (inputs[BACK] != 0 ? 1 : 0) | (inputs[LEFT] != 0 ? 2 : 0) | (inputs[RIGHT] != 0 ? 4 : 0) | (outputs[FRONT] != 0 ? 8 : 0) | (inputs[FRONT] != 0 || outputs[FRONT] != 0 ? 16 : 0);
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
			if (inputs[BACK] != 0 && state) {
				timer = 0;
				return;
			}

			if ((inputs[BACK] != 0) != state && timer == 0) {
				// Account for 2 game ticks = 1 RS tick
				timer = DELAYS[gateSettings] * 2 - 2;

				if (timer == 0) {
					state = !state;
					outputs[FRONT] = state ? (short) 255 : 0;
				}
			}

			if (timer > 0) {
				timer--;
				if (timer == 0) {
					state = !state;
					outputs[FRONT] = state ? (short) 255 : 0;
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
			return gateSettings | (outputs[FRONT] != 0 ? 32768 : 0) | (inputs[BACK] != 0 ? 64 : 0);
		}

		@Override
		public int configure(int gateSettings) {
			return (gateSettings + 1) % DELAYS.length;
		}

		@Override
		public boolean connectsToDirection(int side) {
			return side == FRONT || side == BACK;
		}
	}

	public static class Timer extends GateLogic implements WithRightClickAction, WithPointer, GateLogicTimed {

		public int intervalTicks = 20;
		public int ticksLeft;
		public boolean state, stopped;

		@Override
		public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
			stopped = inputs[BACK] != 0;
			if (inputs[BACK] != 0) {
				state = true;
				outputs[FRONT] = outputs[LEFT] = outputs[RIGHT] = 0;
				ticksLeft = 0;
				return;
			}

			ticksLeft--;
			if (ticksLeft <= 0) {
				state = !state;
				outputs[FRONT] = outputs[LEFT] = outputs[RIGHT] = state ? (short) 255 : 0;
				ticksLeft = state ? 2 : intervalTicks - 2;
			}
		}

		@Override
		public void onRightClick(EntityPlayer ply, TileGate tile) {
			ply.openGui(ProjectRed.instance, GuiIDs.ID_Timer, tile.world(), tile.x(), tile.y(), tile.z());
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (outputs[FRONT] != 0 ? 1 : 0) | (outputs[FRONT] != 0 || inputs[FRONT] != 0 ? 32 : 0) | (outputs[LEFT] != 0 || inputs[LEFT] != 0 ? 2 : 0) | (outputs[RIGHT] != 0 || inputs[RIGHT] != 0 ? 8 : 0) | (inputs[BACK] != 0 ? 4 : 0) | (stopped ? 16 : 0);
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

	public static class Counter extends GateLogic implements WithRightClickAction, WithPointer, Flippable {

		int value = 0, max = 10, incr = 1, decr = 1;
		private boolean wasFront, wasBack;

		@Override
		public int getPointerPosition() {
			if (max == 0)
				return 0;
			return (value * 120) / max - 60;
		}

		@Override
		public float getPointerSpeed() {
			return 0;
		}

		@Override
		public void onRightClick(EntityPlayer ply, TileGate tile) {
			ply.openGui(ProjectRed.instance, GuiIDs.ID_Counter, tile.world(), tile.x(), tile.y(), tile.z());
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (inputs[FRONT] != 0 ? 1 : 0) | (inputs[BACK] != 0 ? 2 : 0) | (outputs[LEFT] != 0 ? 4 : 0) | (outputs[RIGHT] != 0 ? 8 : 0) | (inputs[LEFT] != 0 || outputs[LEFT] != 0 ? 16 : 0) | (inputs[RIGHT] != 0 || outputs[RIGHT] != 0 ? 32 : 0)
			// render state is truncated to 16 bits, but this
			// causes the rendering to update when the pointer moves
			| (getPointerPosition() << 20);
		}

		@Override
		public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {

			if (inputs[FRONT] != 0 && !wasFront)
				value = Math.max(0, value - decr);

			if (inputs[BACK] != 0 && !wasBack)
				value = Math.min(max, value + incr);

			outputs[LEFT] = value == 0 ? (short) 255 : 0;
			outputs[RIGHT] = value == max ? (short) 255 : 0;

			wasFront = inputs[FRONT] != 0;
			wasBack = inputs[BACK] != 0;
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

	public static class Sequencer extends GateLogic implements WithRightClickAction, WithPointer, GateLogicTimed, Flippable {

		public int intervalTicks = 20;
		public int ticksLeft;
		public int state;

		@Override
		public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
			ticksLeft--;
			if (ticksLeft <= 0) {
				ticksLeft = intervalTicks;
				state = (state + 1) & 3;
			}

			outputs[FRONT] = state == 0 ? (short) 255 : 0;
			outputs[RIGHT] = state == 1 ? (short) 255 : 0;
			outputs[BACK] = state == 2 ? (short) 255 : 0;
			outputs[LEFT] = state == 3 ? (short) 255 : 0;
		}

		@Override
		public void onRightClick(EntityPlayer ply, TileGate tile) {
			ply.openGui(ProjectRed.instance, GuiIDs.ID_Timer, tile.world(), tile.x(), tile.y(), tile.z());
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

		@Override
		public boolean connectsToDirection(int side) {
			return side == FRONT || side == BACK;
		}

	}

	public static class PulseFormer extends GateLogic {
		private boolean prevInput;
		private int ticksLeft;

		@Override
		public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
			if (inputs[BACK] != 0 && !prevInput) {
				outputs[FRONT] = (short) 255;
				ticksLeft = 3;
			}
			if (ticksLeft > 0) {
				ticksLeft--;
				if (ticksLeft == 0)
					outputs[FRONT] = 0;
			}
			prevInput = inputs[BACK] != 0;
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (inputs[BACK] != 0 ? 1 : 0) | (outputs[FRONT] != 0 ? 2 : 0) | (ticksLeft > 0 ? 4 : 0) | (outputs[FRONT] != 0 || inputs[FRONT] != 0 ? 8 : 0);
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
			if (inputs[BACK] != 0 && ticksLeft == 0) {
				ticksLeft = 20;
				outputs[FRONT] = random.nextBoolean() ? (short) 255 : 0;
				outputs[LEFT] = random.nextBoolean() ? (short) 255 : 0;
				outputs[RIGHT] = random.nextBoolean() ? (short) 255 : 0;
			}

			if (inputs[BACK] == 0)
				ticksLeft = 0;

			if (ticksLeft > 0)
				ticksLeft--;
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (inputs[BACK] != 0 ? 1 : 0) | (outputs[LEFT] != 0 ? 2 : 0) | (outputs[RIGHT] != 0 ? 4 : 0) | (outputs[FRONT] != 0 ? 8 : 0) | (outputs[FRONT] != 0 || inputs[FRONT] != 0 ? 16 : 0) | (outputs[LEFT] != 0 || inputs[LEFT] != 0 ? 32 : 0) | (outputs[RIGHT] != 0 || inputs[RIGHT] != 0 ? 64 : 0);
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

	public static class StateCell extends GateLogic implements WithRightClickAction, WithPointer, GateLogicTimed, Flippable {

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
		public void onRightClick(EntityPlayer ply, TileGate tile) {
			ply.openGui(ProjectRed.instance, GuiIDs.ID_Timer, tile.world(), tile.x(), tile.y(), tile.z());
		}

		@Override
		public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
			if (inputs[LEFT] != 0 && !timing) {
				timing = true;
				ticksLeft = intervalTicks;
			}
			paused = inputs[LEFT] != 0 || inputs[BACK] != 0;
			if (timing && !paused) {
				ticksLeft--;
				if (ticksLeft <= 0) {
					pulseTicks = 2;
					timing = false;
				}
			}
			outputs[FRONT] = timing ? (short) 255 : 0;
			outputs[RIGHT] = pulseTicks > 0 ? (short) 255 : 0;
			if (pulseTicks > 0)
				pulseTicks--;
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (outputs[FRONT] != 0 || inputs[FRONT] != 0 ? 1 : 0) | (inputs[BACK] != 0 ? 2 : 0) | (inputs[LEFT] != 0 ? 4 : 0) | (outputs[RIGHT] != 0 ? 8 : 0) | (timing ? 16 : 0) | (paused ? 32 : 0) | (inputs[RIGHT] != 0 || outputs[RIGHT] != 0 ? 64 : 0);
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
			if (inputs[LEFT] == 0 && wasLeft)
				leftLatch = true;
			if (inputs[RIGHT] == 0 && wasRight)
				rightLatch = true;
			if (inputs[BACK] != 0)
				leftLatch = rightLatch = false;
			if (leftLatch && rightLatch) {
				pulseTicks = 2;
				leftLatch = rightLatch = false;
			}

			wasLeft = inputs[LEFT] != 0;
			wasRight = inputs[RIGHT] != 0;

			if (pulseTicks > 0) {
				outputs[FRONT] = (short) 255;
				pulseTicks--;
			} else
				outputs[FRONT] = 0;
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (inputs[LEFT] != 0 ? 1 : 0) | (inputs[RIGHT] != 0 ? 2 : 0) | (inputs[BACK] != 0 ? 4 : 0) | (outputs[FRONT] != 0 ? 8 : 0) | (leftLatch ? 16 : 0) | (rightLatch ? 32 : 0) | (outputs[FRONT] != 0 || inputs[FRONT] != 0 ? 64 : 0);
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

	public static class DLatch extends GateLogic implements Stateless, Flippable {
		@Override
		public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
			if (inputs[RIGHT] != 0) {
				outputs[FRONT] = outputs[LEFT] = (short) (inputs[BACK] != 0 ? 255 : 0);
			}
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (inputs[BACK] != 0 ? 1 : 0) | (inputs[RIGHT] != 0 ? 2 : 0) | (outputs[FRONT] != 0 || inputs[FRONT] != 0 ? 4 : 0) | (outputs[LEFT] != 0 || inputs[LEFT] != 0 ? 8 : 0);
		}
	}

	public static class DFlop extends GateLogic implements Flippable {
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
			if (inputs[RIGHT] != 0 && !clockWasOn) {
				outputs[FRONT] = outputs[LEFT] = (short) (inputs[BACK] != 0 ? 255 : 0);
			}
			clockWasOn = inputs[RIGHT] != 0;
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (inputs[BACK] != 0 ? 1 : 0) | (inputs[RIGHT] != 0 ? 2 : 0) | (outputs[FRONT] != 0 || inputs[FRONT] != 0 ? 4 : 0) | (outputs[LEFT] != 0 || inputs[LEFT] != 0 ? 8 : 0);
		}
	}

	public static class BundledLatch extends GateLogic implements Stateless, Flippable, WithBundledConnections {
		@Override
		public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
			if (inputs[RIGHT] != 0) {
				outputs[FRONT] = inputs[BACK];
			}
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (inputs[RIGHT] != 0 ? 1 : 0);
		}

		@Override
		public boolean connectsToDirection(int side) {
			return side == RIGHT || side == FRONT || side == BACK;
		}

		@Override
		public boolean isBundledConnection(int side) {
			return side == FRONT || side == BACK;
		}

	}

	public static class BundledRelay extends GateLogic implements Stateless, Flippable, WithBundledConnections {
		@Override
		public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
			outputs[FRONT] = (inputs[RIGHT] != 0) ? inputs[BACK] : 0;
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (inputs[RIGHT] != 0 ? 1 : 0);
		}

		@Override
		public boolean connectsToDirection(int side) {
			return side == RIGHT || side == FRONT || side == BACK;
		}

		@Override
		public boolean isBundledConnection(int side) {
			return side == FRONT || side == BACK;
		}

	}

	public static class BundledMultiplexer extends GateLogic implements Stateless, Flippable, WithBundledConnections {
		@Override
		public void computeOutFromIn(short[] inputs, short[] outputs, int gateSettings) {
			outputs[FRONT] = (inputs[BACK] != 0) ? inputs[RIGHT] : inputs[LEFT];
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (inputs[BACK] != 0 ? 1 : 0);
		}

		@Override
		public boolean isBundledConnection(int side) {
			return side == FRONT || side == LEFT || side == RIGHT;
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
					outputs[BACK] = (short) (brightness >= (threshold * 3 + 3) ? 255 : 0);
				} else {
					outputs[BACK] = (short) (brightness * 17 > 255 ? 255 : brightness * 17);
				}
			}
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (inputs[BACK] != 0 || outputs[BACK] != 0 ? 1 : 0) | (gateSettings == 0 ? 2 : 0) | (gateSettings == 1 ? 4 : 0) | (gateSettings == 2 ? 8 : 0) | (gateSettings == 3 ? 16 : 0) | (gateSettings == 4 ? 32 : 0) | (gateSettings == 5 ? 64 : 0);
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

		public boolean connectsToDirection(int side) {
			return side == BACK;
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
				outputs[BACK] = (short) (w.isRaining() && BasicUtils.canBlockSeeSky(w, x, y, z) ? 255 : 0);
			}
		}

		@Override
		public int getRenderState(short[] inputs, short[] outputs, int gateSettings) {
			return (inputs[BACK] != 0 || outputs[BACK] != 0 ? 1 : 0);
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

		public boolean connectsToDirection(int side) {
			return side == BACK;
		}
	}
}
