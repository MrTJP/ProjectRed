package mrtjp.projectred.transmission;

import codechicken.lib.vec.Rotation;
import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.block.BlockMultipart;
import codechicken.multipart.block.TileMultipart;
import mrtjp.projectred.api.IBundledEmitter;
import mrtjp.projectred.api.IBundledTile;
import mrtjp.projectred.api.IBundledTileInteraction;
import mrtjp.projectred.api.ITransmissionAPI;
import mrtjp.projectred.core.BundledSignalsLib;
import mrtjp.projectred.transmission.part.BaseCenterWirePart;
import mrtjp.projectred.transmission.part.IBundledCablePart;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;

import static mrtjp.projectred.core.BundledSignalsLib.raiseSignal;

public class TransmissionAPI implements ITransmissionAPI {

    public static final TransmissionAPI INSTANCE = new TransmissionAPI();

    private TransmissionAPI() {
    }

    @Override
    public void registerBundledTileInteraction(IBundledTileInteraction interaction) {
        BundledSignalsLib.registerBundledTileInteraction(interaction);
    }

    @Override
    public byte[] getBundledInput(Level world, BlockPos pos, Direction facing) {
        return BundledSignalsLib.getBundledInput(world, pos, facing);
    }

    @Override
    public boolean containsBundledCable(Level world, BlockPos pos, Direction side) {
        MultiPart part = BlockMultipart.getPart(world, pos, side.ordinal());
        return part instanceof IBundledCablePart;
    }

    @Override
    public boolean containsFramedWire(Level world, BlockPos pos) {
        MultiPart part = BlockMultipart.getPart(world, pos, 6);
        return part instanceof IBundledCablePart;
    }

    @Override
    public int getFramedWireConnectionMask(Level world, BlockPos pos) {
        MultiPart part = BlockMultipart.getPart(world, pos, 6);
        if (part instanceof BaseCenterWirePart) {
            return ((BaseCenterWirePart) part).packedConnMap();
        }
        return -1;
    }
}
