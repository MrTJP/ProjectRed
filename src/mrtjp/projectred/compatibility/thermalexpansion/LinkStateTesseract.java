package mrtjp.projectred.compatibility.thermalexpansion;

import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.TMultiPart;
import cofh.api.transport.IEnderItemHandler;
import cofh.api.transport.RegistryEnderAttuned;
import mrtjp.projectred.api.ISpecialLinkState;
import mrtjp.projectred.core.libmc.PRLib;
import mrtjp.projectred.transportation.RoutedJunctionPipePart;
import net.minecraft.tileentity.TileEntity;
import thermalexpansion.block.ender.TileTesseract;

import java.util.LinkedList;
import java.util.List;

public class LinkStateTesseract implements ISpecialLinkState
{
    @Override
    public TileEntity getLink(TileEntity te)
    {
        if (te instanceof TileTesseract)
        {
            TileTesseract tess = (TileTesseract) te;
            List<TileTesseract> links = getConnectedTesseracts(tess);
            if (links.size() == 1)
            {
                TileTesseract tess2 = links.get(0);

                int localConns = getConnectedPipeCount(tess);
                List<TileEntity> linkedConns = getConnectedPipeTiles(tess2);

                if (localConns == 1 && linkedConns.size() == 1)
                    return linkedConns.get(0);

            }
        }
        return null;
    }

    private List<TileTesseract> getConnectedTesseracts(TileTesseract tess)
    {
        List<TileTesseract> links = new LinkedList<TileTesseract>();
        List<IEnderItemHandler> conns = RegistryEnderAttuned.getLinkedItemOutputs(tess);

        if (conns == null)
            return links;

        for (IEnderItemHandler obj : conns)
            if (obj.canReceiveItems() && obj instanceof TileTesseract)
                links.add((TileTesseract) obj);

        return links;
    }

    private List<TileEntity> getConnectedPipeTiles(TileTesseract tess)
    {
        BlockCoord bc = new BlockCoord(tess);
        List<TileEntity> multipartTiles = new LinkedList<TileEntity>();

        for (int i = 0; i < 6; i++)
        {
            TMultiPart part = PRLib.getMultiPart(tess.getWorldObj(), bc.copy().offset(i), 6);
            if (part instanceof RoutedJunctionPipePart)
            {
                RoutedJunctionPipePart pipe = (RoutedJunctionPipePart) part;
                if (pipe.maskConnects(i^1))
                    multipartTiles.add(part.tile());
            }
        }

        return multipartTiles;
    }

    private int getConnectedPipeCount(TileTesseract tess)
    {
        int conns = 0;
        BlockCoord bc = new BlockCoord(tess);

        for (int i = 0; i < 6; i++)
        {
            TMultiPart part = PRLib.getMultiPart(tess.getWorldObj(), bc.copy().offset(i), 6);
            if (part instanceof RoutedJunctionPipePart)
            {
                RoutedJunctionPipePart pipe = (RoutedJunctionPipePart) part;
                if (pipe.maskConnects(i^1))
                    conns++;
            }
        }

        return conns;
    }

    @Override
    public boolean matches(TileEntity tile)
    {
        return tile instanceof TileTesseract;
    }
}
