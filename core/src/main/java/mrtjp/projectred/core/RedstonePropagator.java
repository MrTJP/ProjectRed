package mrtjp.projectred.core;

import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.block.TileMultipart;
import codechicken.multipart.init.CBMultipartModContent;
import com.google.common.collect.HashMultimap;
import mrtjp.projectred.core.part.IPropagationPart;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.RedStoneWireBlock;

import javax.annotation.Nullable;
import java.util.*;

public class RedstonePropagator {
    /**
     * Standard operation procedure, no special propagation rules. The
     * propagator signal may not have increased.
     */
    public static final int RISING = 0;
    /**
     * Used when the propagator signal dropped (to 0). Propagation should
     * continue until a rising or constant change is encountered at which point
     * a RISING should be propagated back to this wire.
     */
    public static final int DROPPING = 1;
    /**
     * Used when a wire's connection state has changed. Even if the signal
     * remains the same, new connections still need to be recalculated
     */
    public static final int FORCE = 2;
    /**
     * Used when the propagator did not change signal, but a new connection may
     * have been established and signal needs recalculating
     */
    public static final int FORCED = 3;

    private static final LinkedList<PropagationRun> reusableRuns = new LinkedList<>();
    private static @Nullable PropagationRun currentRun = null;
    private static @Nullable PropagationRun finishingRun = null;

    private static boolean canRedwiresProvidePower = true;
    private static boolean canConnectRedwires = true;

    public static boolean canConnectRedwires() {
        return canConnectRedwires;
    }
    public static void setCanConnectRedwires(boolean canConnectRedwires) {
        RedstonePropagator.canConnectRedwires = canConnectRedwires;
    }

    public static boolean canRedwiresProvidePower() {
        return canRedwiresProvidePower;
    }
    public static void setDustProvidesPower(boolean dustProvidesPower) {
        ((RedStoneWireBlock) Blocks.REDSTONE_WIRE).shouldSignal = dustProvidesPower;
    }
    public static void setRedwiresProvidePower(boolean canRedwiresProvidePower) {
        RedstonePropagator.canRedwiresProvidePower = canRedwiresProvidePower;
    }

    public static void resetPowerFlags() {
        setDustProvidesPower(true);
        setRedwiresProvidePower(true);
        setCanConnectRedwires(true);
    }

    public static void addNeighborChange(Level world, BlockPos sourcePos, BlockPos neighborPos) {
        assert currentRun != null;
        currentRun.neighborChanges.put(world, neighborPos);

        // Track source of change for first time a particular neighbor is changed
        if (!currentRun.neighborChangeSources.containsKey(neighborPos))
            currentRun.neighborChangeSources.put(neighborPos, sourcePos);
    }

    public static void addPartChange(MultiPart part) {
        assert currentRun != null;
        currentRun.partChanges.put(part.tile(), part);
    }

    public static void logCalculation() {
        if (finishingRun != null) {
            finishingRun.recalcs++;
        }
    }

    public static void propagateTo(IPropagationPart part, @Nullable IPropagationPart from, int mode) {
        if (currentRun != null) {
            currentRun.add(part, from, mode);
            return;
        }
        currentRun = reusableRuns.isEmpty() ? new PropagationRun() : reusableRuns.removeFirst();
        currentRun.add(part, from, mode);
        currentRun.executeRun(finishingRun);
    }

    public static void propagateTo(IPropagationPart part, int mode) {
        propagateTo(part, null, mode);
    }

    public static void propagateAnalogDrop(IPropagationPart part) {
        assert currentRun != null;
        currentRun.addAnalogDrop(part);
    }

    private static class PropagationRun {

        private @Nullable PropagationRun parent;
        private @Nullable IPropagationPart lastCaller;
        private int count = 0;
        private int recalcs = 0;

        HashMultimap<TileMultipart, MultiPart> partChanges = HashMultimap.create();
        HashMultimap<Level, BlockPos> neighborChanges = HashMultimap.create();
        HashMap<BlockPos, BlockPos> neighborChangeSources = new HashMap<>();
        List<Runnable> propagationTasks = new LinkedList<>();
        List<Runnable> analogDropPropagationTasks = new LinkedList<>();

        void clear() {
            partChanges.clear();
            neighborChanges.clear();
            neighborChangeSources.clear();
            count = 0;
            recalcs = 0;
            lastCaller = null;
            reusableRuns.add(this);
        }

        void executeRun(@Nullable PropagationRun parent) {
            this.parent = parent;
            RedstonePropagator.currentRun = this;
            runLoop();
            finishRun();
        }

        void runLoop() {
            do {
                List<Runnable> pTasks = propagationTasks;
                propagationTasks = new LinkedList<>();

                pTasks.forEach(Runnable::run);

                if (propagationTasks.isEmpty() && !analogDropPropagationTasks.isEmpty()) {
                    propagationTasks = analogDropPropagationTasks;
                    analogDropPropagationTasks = new LinkedList<>();
                }
            } while (!propagationTasks.isEmpty());
        }

        void finishRun() {
            RedstonePropagator.currentRun = null;

            if (partChanges.isEmpty() && neighborChanges.isEmpty()) {
                RedstonePropagator.finishingRun = parent;
                clear();
                return;
            }

            RedstonePropagator.finishingRun = this;

            // Notify part changes in bulk
            for (Map.Entry<TileMultipart, Collection<MultiPart>> entry : partChanges.asMap().entrySet()) {
                Collection<MultiPart> parts = entry.getValue();
                for (MultiPart part : parts) {
                    ((IPropagationPart) part).onSignalUpdate();
                }
                entry.getKey().multiPartChange(parts);
            }

            // Notify normal neighbor changes in bulk
            for (Map.Entry<Level, Collection<BlockPos>> entry : neighborChanges.asMap().entrySet()) {
                Level world = entry.getKey();
                Collection<BlockPos> positions = entry.getValue();
                for (BlockPos pos : positions) {
                    world.neighborChanged(pos, CBMultipartModContent.MULTIPART_BLOCK.get(), neighborChangeSources.get(pos));
                }
            }

            RedstonePropagator.finishingRun = parent;
            clear();
        }

        void add(IPropagationPart part, @Nullable IPropagationPart from, int mode) {
            if (from != lastCaller) {
                lastCaller = from;
                count++;
            }
            propagationTasks.add(() -> part.updateAndPropagate(from, mode));
        }

        void addAnalogDrop(IPropagationPart part) {
            analogDropPropagationTasks.add(() -> part.updateAndPropagate(null, RISING));
        }
    }

}
