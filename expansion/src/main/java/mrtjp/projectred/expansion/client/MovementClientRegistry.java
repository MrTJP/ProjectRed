package mrtjp.projectred.expansion.client;

import mrtjp.projectred.api.MovingBlockEntityRenderCallback;
import net.covers1624.quack.collection.FastStream;

import java.util.LinkedList;
import java.util.List;

public class MovementClientRegistry {

    private static List<MovingBlockEntityRenderCallback> callbacks = new LinkedList<>();

    public static void registerBlockEntityRendererCallback(MovingBlockEntityRenderCallback callback) {
        callbacks.add(callback);
    }

    public static void dispatchPreRender(double offsetX, double offsetY, double offsetZ) {
        FastStream.of(callbacks).forEach(c -> c.onMovingPreRender(offsetX, offsetY, offsetZ));
    }

    public static void dispatchPostRender() {
        FastStream.of(callbacks).forEach(MovingBlockEntityRenderCallback::onMovingPostRender);
    }
}
