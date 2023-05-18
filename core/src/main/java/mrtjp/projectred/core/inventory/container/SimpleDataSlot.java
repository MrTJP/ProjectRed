package mrtjp.projectred.core.inventory.container;

import net.minecraft.world.inventory.DataSlot;

import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * Data slot that can be initialized with lambdas for cleaner code
 */
public class SimpleDataSlot extends DataSlot {

    private final Supplier<Integer> getter;
    private final Consumer<Integer> setter;

    public SimpleDataSlot(Supplier<Integer> getter, Consumer<Integer> setter) {
        this.getter = getter;
        this.setter = setter;
    }

    @Override
    public int get() {
        return getter.get();
    }

    @Override
    public void set(int value) {
        setter.accept(value);
    }
}
