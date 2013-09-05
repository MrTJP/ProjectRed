package mrtjp.projectred.integration;

import java.util.Random;

import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.transmission.IRedwireEmitter;
import net.minecraft.block.Block;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Rotation;
import codechicken.multipart.IFaceRedstonePart;
import codechicken.multipart.IRandomDisplayTick;
import codechicken.multipart.IRedstonePart;
import codechicken.multipart.RedstoneInteractions;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;
import codechicken.multipart.handler.MultipartProxy;

@SuppressWarnings("unchecked")
public abstract class RedstoneGatePart extends GatePart implements IFaceRedstonePart, IRandomDisplayTick
{
    @Override
    public abstract RedstoneGateLogic getLogic();

    @Override
    public int strongPowerLevel(int side) {
        if((side&6) == (side()&6))
            return 0;
        
        return getLogic().getOutput(this, relRot(side));
    }

    @Override
    public int weakPowerLevel(int side) {
        return strongPowerLevel(side);
    }

    @Override
    public boolean canConnectRedstone(int side) {
        if((side&6) == (side()&6))
            return false;
        
        return getLogic().canConnect(this, relRot(side));
    }
    
    @Override
    public int getFace() {
        return side();
    }

    @Override
    public void notifyNeighbors(int mask) {
        int smask = 0;

        int blockID = ((Block)MultipartProxy.block()).blockID;
        BlockCoord pos = new BlockCoord();
        BlockCoord pos2 = new BlockCoord();
        
        for(int r = 0; r < 4; r++)
            if((mask & 1<<r) != 0) {
                int absSide = Rotation.rotateSide(side(), r);
                pos.set(x(), y(), z()).offset(absSide);
                
                world().notifyBlockOfNeighborChange(pos.x, pos.y, pos.z, blockID);
                for(int s = 0; s < 6; s++)
                    if(s != (absSide^1) && (smask & 1<<s) == 0) {
                        pos2.set(pos).offset(s);
                        world().notifyBlockOfNeighborChange(pos2.x, pos2.y, pos2.z, blockID);
                    }
                
                smask |= 1<<absSide;
            }
    }
    
    public int getRedstoneInput(int r) {
        r = toAbsolute(r);
        if((connMap & 1<<r) != 0)
            return calculateCornerSignal(r);
        else if((connMap & 0x10<<r) != 0)
            return calculateStraightSignal(r);
        else if((connMap & 0x100<<r) != 0)
            return calculateInternalSignal(r);
        
        return calculateRedstoneSignal(r);
    }
    
    public int calculateRedstoneSignal(int r) {
        int absDir = Rotation.rotateSide(side(), r);
        
        int i = RedstoneInteractions.getPowerTo(this, absDir)*17;
        if(i > 0 || getLogic().requireStrongInput(r))
            return i;
        
        BlockCoord pos = new BlockCoord(getTile()).offset(absDir);
        return world().getIndirectPowerLevelTo(pos.x, pos.y, pos.z, absDir)*17;
    }

    public int calculateCornerSignal(int r) {
        int absDir = Rotation.rotateSide(side(), r);
        
        BlockCoord pos = new BlockCoord(getTile()).offset(absDir).offset(side());
        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null)
            return getPartSignal(t.partMap(absDir^1), Rotation.rotationTo(absDir^1, side()^1));
        
        return 0;
    }
    
    public int calculateStraightSignal(int r) {
        int absDir = Rotation.rotateSide(side(), r);

        BlockCoord pos = new BlockCoord(getTile()).offset(absDir);
        TileMultipart t = BasicUtils.getMultipartTile(world(), pos);
        if (t != null)
            return getPartSignal(t.partMap(side()), (r+2)%4);
        
        return 0;
    }

    public int calculateInternalSignal(int r) {
        int absDir = Rotation.rotateSide(side(), r);
        
        TMultiPart tp = tile().partMap(absDir);
        int i = getPartSignal(tp, Rotation.rotationTo(absDir, side()));
        if(i > 0)
            return i;
        
        if(tp instanceof IRedstonePart) {
            IRedstonePart rp = (IRedstonePart) tp;
            return Math.max(rp.strongPowerLevel(side()), rp.weakPowerLevel(side())) << 4;
        }
        
        return 0;
    }

    public int getPartSignal(TMultiPart part, int r) {
        if(part instanceof IRedwireEmitter)
            return ((IRedwireEmitter) part).getRedwireSignal(r);
        
        return 0;
    }
    
    @Override
    public void randomDisplayTick(Random rand) {
        RenderGate.spawnParticles(this, rand);
    }
}
