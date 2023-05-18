package mrtjp.projectred.core.power;

import net.minecraft.nbt.CompoundTag;

import java.util.List;

public class PowerConductor {

    private double vCap = 0;
    private double iCap = 0;

    private final double resistance;
    private final double capacitance;
    private final double inverseCapacitance;

    private long time = -1;

    private final IPowerConductorSource parent;

    public PowerConductor(IPowerConductorSource parent, double resistance, double capacitance) {
        this.parent = parent;
        this.resistance = resistance;
        this.capacitance = capacitance;
        this.inverseCapacitance = 1 / capacitance;
    }

    //region Save/load
    public void save(CompoundTag tag) {
        tag.putDouble("vCap", vCap);
        tag.putDouble("iCap", iCap);
    }

    public void load(CompoundTag tag) {
        if (tag.contains("vCap")) {
            vCap = tag.getDouble("vCap");
            iCap = tag.getDouble("iCap");
        } else {
            // TODO Remove legacy support
            vCap = tag.getDouble("vl");
            iCap = tag.getDouble("il");
        }
    }
    //endregion

    public double getVoltage() {
        long t = parent.getTime();
        if (this.time != t) {
            time = t;
            // I = C * dV/dT
            // dV = dT * I / C
            vCap += 0.05D * iCap * inverseCapacitance;
            iCap = 0; // This will re-accumulate during the tick
        }
        return vCap;
    }

    public double getCurrent() {
        return iCap * 0.05D;
    }

    public double getEnergy() {
        return 0.5D * capacitance * vCap * vCap;
    }

    public void applyCurrent(double i) {
        getVoltage();
        iCap += i;
    }

    /**
     * Apply continuous power for 1 tick. Negative values will draw power out
     * @param p Power (W)
     */
    public void applyPower(double p) {
        // 1. Find the voltage change after power is applied for 1 tick
        //   Given:
        //     P = dE(t) / dt
        //     P = d(1/2 * C * V(t)^2) / dt
        //
        //   In the above power equation, we want to find dV(t). By moving the values around, we get:
        //     P * 2 * 1/C * dt = dV(t)^2
        //     sqrt(P * 2 * 1/C * dt) = dV(t)
        //
        //   By substituting known values, we can now compute hypothetical voltage drop if the given power
        //   was drawn for 1 tick:
        //     dt = 1/20 = 0.05
        //     1/C = inverseCapacitance
        //     dV(t) = sqrt(P * 2 * inverseCapacitance * 0.05)
        //     dV(t) = sqrt(P * 0.1 * inverseCapacitance)
        //
        // 2. Calculate current required to achieve the voltage drop
        //    Given:
        //      I = C * dV(t) / dt
        //    Current to be drawn to arrive at a dV(t) amount of drop is:
        //      dt = 1/20 = 0.05
        //      C = capacitance
        //      I = capacitance * dV(t) / 0.05
        //      I = capacitance * 20 * dV(t)
        double dVSquared = Math.abs(p) * 0.1D * inverseCapacitance;
        if (p < 0 && dVSquared >= vCap * vCap) {
            // Not enough energy stored to supply the power
            return;
        }

        double dI = 20 * Math.sqrt(dVSquared) * capacitance;
        applyCurrent(p < 0 ? -dI : dI);
    }

    public void tick() {
        // Force voltage re-calculation
        getVoltage();

        // Exchange current with all connected conductors
        List<PowerConductor> conductors = parent.getConnectedConductors();
        for (PowerConductor cond : conductors) {

            // Easy way to make sure pairs of conductors only do current exchange once per tick:
            // Only allow the one with higher voltage to do the exchange.
            if (cond.getVoltage() > getVoltage())
                continue;

            double v = getVoltage() - cond.getVoltage();
            double r = resistance + cond.resistance;
            double i = v / r;

            applyCurrent(-i);
            cond.applyCurrent(i);
        }
    }
}
