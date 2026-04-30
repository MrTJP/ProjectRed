package mrtjp.projectred.expansion;

import codechicken.lib.vec.Vector3;
import mrtjp.projectred.api.MovementDescriptor;
import net.minecraft.core.Direction;

public interface MovingStructureInfo extends MovementDescriptor {

    MovingStructureInfo NO_MOVEMENT_INFO = new MovingStructureInfo() {
        //@formatter:off
        @Override public Vector3 getMovementOffset() { return Vector3.ZERO.copy(); }
        @Override public Vector3 getRenderOffset(float partialTicks) { return Vector3.ZERO.copy(); }
        @Override public MovementStatus getStatus() { return MovementStatus.UNKNOWN; }
        @Override public boolean isMoving() { return false; }
        @Override public double getProgress() { return 0; }
        @Override public int getSize() { return 0; }
        public Direction getDirection() { return Direction.DOWN; }
        //@formatter:on
    };

    static MovingStructureInfo failedMovement(int size) {
        return new MovingStructureInfo() {
            //@formatter:off
            @Override public Vector3 getMovementOffset() { return Vector3.ZERO.copy(); }
            @Override public Vector3 getRenderOffset(float partialTicks) { return Vector3.ZERO.copy(); }
            @Override public MovementStatus getStatus() { return MovementStatus.FAILED; }
            @Override public boolean isMoving() { return false; }
            @Override public double getProgress() { return 0; }
            @Override public int getSize() { return size; }
            @Override public Direction getDirection() { return Direction.DOWN; }
            //@formatter:on
        };
    }

    Direction getDirection();

    Vector3 getMovementOffset();

    Vector3 getRenderOffset(float partialTicks);
}
