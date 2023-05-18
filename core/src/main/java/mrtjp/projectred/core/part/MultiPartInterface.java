package mrtjp.projectred.core.part;

import codechicken.multipart.block.TileMultiPart;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

/**
 * Base interface for all multipart trait interfaces. In 1.18+, TMultiPart itself is an interface,
 * so this isn't needed.
 */
public interface MultiPartInterface {

    World level();

    BlockPos pos();

    TileMultiPart tile();
}
