package mrtjp.projectred.integration2;

import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.transmission.IRedwireEmitter;
import net.minecraft.block.Block;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Rotation;
import codechicken.multipart.IFaceRedstonePart;
import codechicken.multipart.IRedstonePart;
import codechicken.multipart.RedstoneInteractions;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

@SuppressWarnings("unchecked")
public abstract class RedstoneGatePart extends GatePart implements IFaceRedstonePart
{
    @Override
    public abstract RedstoneGateLogic getLogic();

    @Override
    public int strongPowerLevel(int side) {
        if((side&6) == (side()&6))
            return 0;
        
        return getLogic().getOutput(this, relRot(side)) ? 15 : 0;
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

    public int getRedwireInput(int r) {
        if(maskConnects(r)) {
            if((connMap & 1<<r) != 0)
                return calculateCornerSignal(r);
            else if((connMap & 0x10<<r) != 0)
                return calculateStraightSignal(r);
            else
                return calculateInternalSignal(r);
        }
        
        return 0;
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
        if (t != null) {
            int i = getPartSignal(t.partMap(side()), (r+2)%4);
            if(i > 0)
                return i;
        }
    
        int blockID = world().getBlockId(pos.x, pos.y, pos.z);
        if(blockID == Block.redstoneWire.blockID)
            return world().getBlockMetadata(pos.x, pos.y, pos.z)-1;
        
        return RedstoneInteractions.getPowerTo(this, absDir)*17;
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
}
