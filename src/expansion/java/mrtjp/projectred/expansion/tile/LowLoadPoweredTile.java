package mrtjp.projectred.expansion.tile;

import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.ILowLoadMachine;
import mrtjp.projectred.core.ILowLoadPowerLine;
import mrtjp.projectred.core.JDrawPointPowerConductor;
import mrtjp.projectred.core.PowerConductor;
import mrtjp.projectred.core.tile.BasePoweredTile;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.tileentity.TileEntityType;

import java.util.stream.Collectors;
import java.util.stream.IntStream;

public abstract class LowLoadPoweredTile extends BasePoweredTile implements ILowLoadMachine {

    protected final JDrawPointPowerConductor conductor = new JDrawPointPowerConductor(this,
            IntStream.rangeClosed(0, 29)
                    .boxed()
                    .collect(Collectors.toList()));

    public LowLoadPoweredTile(TileEntityType<?> type) {
        super(type);
    }

    @Override
    public void saveToNBT(CompoundNBT tag) {
        super.saveToNBT(tag);
        conductor.save(tag);
    }

    @Override
    public void loadFromNBT(CompoundNBT tag) {
        super.loadFromNBT(tag);
        conductor.load(tag);
    }

    @Override
    public void tick() {
        if (getLevel().isClientSide) return;
        conductor.update();
    }

    @Override
    public boolean canConnectPart(IConnectable part, int s, int edgeRot) {
        if (part instanceof ILowLoadMachine) return true;
        if (part instanceof ILowLoadPowerLine) return true;

        return false;
    }

    @Override
    public PowerConductor conductor(int dir) {
        return conductor;
    }
}
