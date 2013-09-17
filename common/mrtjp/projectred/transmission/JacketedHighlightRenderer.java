package mrtjp.projectred.transmission;

import mrtjp.projectred.core.BasicUtils;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.util.MovingObjectPosition;
import net.minecraft.world.World;
import scala.Tuple2;
import codechicken.lib.raytracer.ExtendedMOP;
import codechicken.lib.vec.BlockCoord;
import codechicken.microblock.MicroMaterialRegistry;
import codechicken.microblock.MicroMaterialRegistry.IMicroHighlightRenderer;
import codechicken.microblock.MicroblockClass;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

public class JacketedHighlightRenderer implements IMicroHighlightRenderer
{
    @Override
    public boolean renderHighlight(World world, EntityPlayer player, MovingObjectPosition hit, MicroblockClass mcrClass, int size, int material) {
        TileMultipart tile = BasicUtils.getMultipartTile(world, new BlockCoord(hit.blockX, hit.blockY, hit.blockZ));
        if(tile == null || mcrClass.classID() != 0 || size != 1 || player.isSneaking() || MicroMaterialRegistry.getMaterial(material).isTransparent())
            return false;
        
        Tuple2<Integer, ?> hitData = ExtendedMOP.getData(hit);
        
        TMultiPart part = tile.partList().apply(hitData._1$mcI$sp());
        if(!(part instanceof FramedWirePart))
            return false;
        
        FramedWirePart fpart = (FramedWirePart)part;
        if(fpart.material == material)
            return false;
        
        RenderFramedWire.renderCoverHighlight(fpart, material);
        return true;
    }
}
