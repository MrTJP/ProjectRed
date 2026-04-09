package mrtjp.projectred.expansion.client;

import mrtjp.projectred.expansion.MovementManager;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;

public class MovingBlockRenderManager {

    public static boolean isMoving(MovementManager manager, BlockPos pos) {
        return manager.getMovementInfo(pos).isMoving();
    }

    public static boolean isAdjacentToMoving(MovementManager manager, BlockPos pos) {
        for (int s = 0; s < 6; s++) {
            var rPos = pos.relative(Direction.values()[s]);
            if (manager.getMovementInfo(rPos).isMoving()) return true;
        }
        return false;
    }
}
