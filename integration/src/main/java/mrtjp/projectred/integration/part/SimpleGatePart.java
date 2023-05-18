package mrtjp.projectred.integration.part;

import codechicken.multipart.util.PartRayTraceResult;
import mrtjp.projectred.api.IScrewdriver;
import mrtjp.projectred.core.part.IOrientableFacePart;
import mrtjp.projectred.integration.GateType;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.LightLayer;

import java.util.Random;

public abstract class SimpleGatePart extends RedstoneGatePart {

    private static final int[] ADVANCE_DEAD = { 1, 2, 4, 0, 5, 6, 3 };

    public SimpleGatePart(GateType type) {
        super(type);
    }

    //region Shape cycling
    @Override
    protected boolean gateLogicCycleShape() {
        int oldShape = shape();
        int newShape = cycleShape(oldShape);
        if (newShape != oldShape) {
            setShape(newShape);
            return true;
        }
        return false;
    }

    protected int cycleShape(int shape) {
        if (deadSides() != 0) {
            int shape1 = shape();
            do {
                shape1 = ADVANCE_DEAD[shape1];
            }
            while (Integer.bitCount(shape1) > maxDeadSides() || 32 - Integer.numberOfLeadingZeros(shape1) > deadSides());
            return shape1;
        }

        return shape;
    }

    protected int deadSides() {
        return 0;
    }

    protected int maxDeadSides() {
        return deadSides() - 1;
    }
    //endregion

    //region Gate logic
    @Override
    protected void gateLogicOnChange() {
        int iMask = inputMask(shape());
        int oMask = outputMask(shape());
        int fMask = feedbackMask(shape());
        int oldInput = getState() & 0xF;
        int newInput = getInput(iMask | fMask);
        if (oldInput != newInput) {
            setState(getState() & 0xF0 | newInput);
            onInputChange();
        }

        int newOutput = calcOutput(state() & iMask) & oMask;
        if (newOutput != (state() >> 4)) scheduleTick(getDelay(shape()));
    }

    @Override
    protected void gateLogicOnScheduledTick() {
        int iMask = inputMask(shape());
        int oMask = outputMask(shape());
        int oldOutput = state() >> 4;
        int newOutput = calcOutput(state() & iMask) & oMask;
        if (oldOutput != newOutput) {
            setState(state() & 0xF | newOutput << 4);
            onOutputChange(oMask);
        }
        gateLogicOnChange();
    }

    @Override
    protected void gateLogicSetup() {
        int iMask = inputMask(shape());
        int oMask = outputMask(shape());
        int output = calcOutput(getInput(iMask)) & oMask;
        if (output != 0) {
            setState(output << 4);
            onOutputChange(output); //use output for change mask because nothing is going low
        }
    }

    int getDelay(int shape) {
        return 2;
    }

    int feedbackMask(int shape) {
        return 0;
    }

    int calcOutput(int input) {
        return 0;
    }
    //endregion

    public static class OR extends SimpleGatePart {

        public OR(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return 1;
        }

        @Override
        protected int inputMask(int shape) {
            return ~shape << 1 & 0xE;
        }

        @Override
        protected int deadSides() {
            return 3;
        }

        @Override
        int calcOutput(int input) {
            return input != 0 ? 1 : 0;
        }
    }

    public static class NOR extends SimpleGatePart {

        public NOR(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return 1;
        }

        @Override
        protected int inputMask(int shape) {
            return ~shape << 1 & 0xE;
        }

        @Override
        int feedbackMask(int shape) {
            return 1;
        }

        @Override
        protected int deadSides() {
            return 3;
        }

        @Override
        int calcOutput(int input) {
            return input == 0 ? 1 : 0;
        }
    }

    public static class NOT extends SimpleGatePart {

        public NOT(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return ~((shape & 1) << 1 | (shape & 2) >> 1 | (shape & 4) << 1) & 0xB;
        }

        @Override
        protected int inputMask(int shape) {
            return 4;
        }

        @Override
        int feedbackMask(int shape) {
            return outputMask(shape);
        }

        @Override
        protected int deadSides() {
            return 3;
        }

        @Override
        int calcOutput(int input) {
            return input == 0 ? 0xB : 0;
        }
    }

    public static class AND extends SimpleGatePart {

        public AND(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return 1;
        }

        @Override
        protected int inputMask(int shape) {
            return ~shape << 1 & 0xE;
        }

        @Override
        protected int deadSides() {
            return 3;
        }

        @Override
        int calcOutput(int input) {
            return input == inputMask(shape()) ? 1 : 0;
        }
    }

    public static class NAND extends SimpleGatePart {

        public NAND(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return 1;
        }

        @Override
        protected int inputMask(int shape) {
            return ~shape << 1 & 0xE;
        }

        @Override
        protected int deadSides() {
            return 3;
        }

        @Override
        int calcOutput(int input) {
            return input == inputMask(shape()) ? 0 : 1;
        }
    }

    public static class XOR extends SimpleGatePart {

        public XOR(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return 1;
        }

        @Override
        protected int inputMask(int shape) {
            return 10;
        }

        @Override
        int calcOutput(int input) {
            boolean side1 = (input & 1 << 1) != 0;
            boolean side2 = (input & 1 << 3) != 0;
            return side1 != side2 ? 1 : 0;
        }
    }

    public static class XNOR extends SimpleGatePart {

        public XNOR(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return 1;
        }

        @Override
        protected int inputMask(int shape) {
            return 10;
        }

        @Override
        int calcOutput(int input) {
            boolean side1 = (input & 1 << 1) != 0;
            boolean side2 = (input & 1 << 3) != 0;
            return side1 == side2 ? 1 : 0;
        }
    }

    public static class Buffer extends SimpleGatePart {

        public Buffer(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return ~((shape & 1) << 1 | (shape & 2) << 2) & 0xB;
        }

        @Override
        protected int inputMask(int shape) {
            return 4;
        }

        @Override
        int feedbackMask(int shape) {
            return outputMask(shape);
        }

        @Override
        protected int deadSides() {
            return 2;
        }

        @Override
        protected int maxDeadSides() {
            return 2;
        }

        @Override
        int calcOutput(int input) {
            return input != 0 ? 0xB : 0;
        }
    }

    public static class Multiplexer extends SimpleGatePart {

        public Multiplexer(GateType type) {
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
        int calcOutput(int input) {
            return (input & 1 << 2) != 0 ? (input >> 3) & 1 : (input >> 1) & 1;
        }
    }

    public static class Pulse extends SimpleGatePart {

        public Pulse(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return 1;
        }

        @Override
        protected int inputMask(int shape) {
            return 4;
        }

        @Override
        int calcOutput(int input) {
            return 0;
        }

        @Override
        protected void gateLogicOnChange() {
            int oldInput = state() & 0xF;
            int newInput = getInput(4);

            if (oldInput != newInput) {
                setState(state() & 0xF0 | newInput);
                onInputChange();
                if (newInput != 0 && (state() & 0xF0) == 0) {
                    setState(state() & 0xF | 0x10);
                    scheduleTick(2);
                    onOutputChange(1);
                }
            }
        }
    }

    public static class Repeater extends SimpleGatePart {

        private static final int[] DELAYS = { 2, 4, 6, 8, 16, 32, 64, 128, 256 };

        public Repeater(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return 1;
        }

        @Override
        protected int inputMask(int shape) {
            return 4;
        }

        @Override
        int getDelay(int shape) {
            return DELAYS[shape];
        }

        @Override
        protected int cycleShape(int shape) {
            return (shape + 1) % DELAYS.length;
        }

        @Override
        int calcOutput(int input) {
            return input != 0 ? 1 : 0;
        }

        @Override
        protected void gateLogicOnChange() {
            if (!isTickScheduled()) super.gateLogicOnChange();
        }

        @Override
        protected boolean gateLogicActivate(Player player, ItemStack held, PartRayTraceResult hit) {
            // Allow configuring without screwdriver
            if (held.isEmpty() || !(held.getItem() instanceof IScrewdriver)) {
                if (!level().isClientSide) {
                    configure();
                }
                return true;
            }
            return false;
        }
    }

    public static class Randomizer extends SimpleGatePart {

        private static final Random RANDOM = new Random();

        public Randomizer(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return ~((shape & 1) << 1 | (shape & 2) >> 1 | (shape & 4) << 1) & 0xB;
        }

        @Override
        protected int inputMask(int shape) {
            return 4;
        }

        @Override
        int feedbackMask(int shape) {
            return outputMask(shape);
        }

        @Override
        protected int deadSides() {
            return 3;
        }

        @Override
        int calcOutput(int input) {
            if (input == 0) {
                return state() >> 4;
            } else {
                int r = IOrientableFacePart.shiftMask(RANDOM.nextInt(8), 3);
                return outputMask(shape()) & r;
            }
        }

        @Override
        protected void gateLogicOnChange() {
            super.gateLogicOnChange();
            if ((state() & 4) != 0) {
                scheduleTick(2);
            }
        }
    }

    public static class TransparentLatch extends SimpleGatePart {

        public TransparentLatch(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return shape == 0 ? 3 : 9;
        }

        @Override
        protected int inputMask(int shape) {
            return shape == 0 ? 0xC : 6;
        }

        @Override
        protected int cycleShape(int shape) {
            return shape ^ 1;
        }

        @Override
        int calcOutput(int input) {
            return (input & 4) == 0 ? state() >> 4 : (input & 0xA) == 0 ? 0 : 0xF;
        }
    }

    public static class LightSensor extends SimpleGatePart {

        public LightSensor(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return 4;
        }

        @Override
        protected int inputMask(int shape) {
            return 0;
        }

        @Override
        int feedbackMask(int shape) {
            return 4;
        }

        @Override
        protected int cycleShape(int shape) {
            return (shape + 1) % 3;
        }

        @Override
        int getOutput(int r) {
            return r == 2 ? state() >> 4 : 0;
        }

        private int calcSkyLight() {
            return level().getBrightness(LightLayer.SKY, pos()) - level().getSkyDarken();
        }

        private int calcBlockLight() {
            return level().getBrightness(LightLayer.BLOCK, pos());
        }

        @Override
        protected void gateLogicOnTick() {
            if (level().isClientSide) return;

            int newOutput = shape() == 1 ? calcSkyLight() : shape() == 2 ? calcBlockLight() : Math.max(calcSkyLight(), calcBlockLight());

            if (newOutput != (state() >> 4)) {
                setState(newOutput << 4 | state() & 0xF);
                onOutputChange(4);
            }
        }

        @Override
        protected void gateLogicOnChange() {
            int oldInput = state() & 0xF;
            int newInput = getInput(4);
            if (oldInput != newInput) {
                setState(state() & 0xF0 | newInput);
                onInputChange();
            }
        }

        @Override
        public int getLightEmission() {
            return 0;
        }
    }

    public static class RainSensor extends SimpleGatePart {

        public RainSensor(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return 4;
        }

        @Override
        protected int inputMask(int shape) {
            return 0;
        }

        @Override
        int feedbackMask(int shape) {
            return 4;
        }

        @Override
        protected void gateLogicOnTick() {
            if (level().isClientSide) return;

            int newOutput = level().isRaining() && level().canSeeSky(pos()) ? 4 : 0;
            int oldOutput = state() >> 4;
            if (newOutput != oldOutput) {
                setState(newOutput << 4 | state() & 0xF);
                onOutputChange(4);
            }
        }

        @Override
        public int getLightEmission() {
            return 0;
        }
    }

    public static class DecodingRandomizer extends SimpleGatePart {

        private static final Random RANDOM = new Random();
        private static final int[] OUTPUTS = { 1, 8, 2 };

        public DecodingRandomizer(GateType type) {
            super(type);
        }

        @Override
        protected int outputMask(int shape) {
            return shape == 0 ? 0xB : 9;
        }

        @Override
        protected int inputMask(int shape) {
            return 4;
        }

        @Override
        int feedbackMask(int shape) {
            return 2;
        }

        @Override
        protected int cycleShape(int shape) {
            return shape ^ 1;
        }

        @Override
        int calcOutput(int input) {
            if (input == 0) {
                return ((state() >> 4) == 0) ? 1 : state() >> 4;
            } else {
                int r = RANDOM.nextInt((~shape() | 2) & 3);
                return OUTPUTS[r];
            }
        }

        @Override
        protected void gateLogicOnChange() {
            super.gateLogicOnChange();
            if ((state() & 4) != 0) {
                scheduleTick(2);
            }
        }
    }
}
