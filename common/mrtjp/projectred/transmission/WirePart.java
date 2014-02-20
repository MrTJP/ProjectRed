package mrtjp.projectred.transmission;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.lighting.LazyLightMatrix;
import codechicken.lib.raytracer.IndexedCuboid6;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.TextureUtils;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.*;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;
import mrtjp.projectred.ProjectRedCore;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.BasicWireUtils;
import mrtjp.projectred.core.CommandDebug;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.WireConnLib;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.Icon;
import net.minecraft.util.MovingObjectPosition;
import net.minecraftforge.common.ForgeDirection;
import org.lwjgl.opengl.GL11;

import java.util.Arrays;

/**
 * This is the base class for all wire types. It can be used for any sub type,
 * as it contains the base calculations necessary to create a working wire. This
 * calculates all possible connections to sides, around corners, and inside
 * corners, while checking for microblock obstructions.
 *
 * @author MrTJP
 */
public abstract class WirePart extends TMultiPart implements IConnectable, TFacePart, JNormalOcclusion, IWirePart
{

    public static Cuboid6[][] selectionBounds = new Cuboid6[3][6];
    public static Cuboid6[][] occlusionBounds = new Cuboid6[3][6];

    static
    {
        for (int t = 0; t < 3; t++)
        {
            Cuboid6 selection = new Cuboid6(0, 0, 0, 1, (t+2)/16D, 1).expand(-0.005); // subtract the box a little because we'd like things like posts to get first hit
            Cuboid6 occlusion = new Cuboid6(2/8D, 0, 2/8D, 6/8D, (t+2)/16D, 6/8D);
            for (int s = 0; s < 6; s++)
            {
                selectionBounds[t][s] = selection.copy().apply(Rotation.sideRotations[s].at(Vector3.center));
                occlusionBounds[t][s] = occlusion.copy().apply(Rotation.sideRotations[s].at(Vector3.center));
            }
        }
    }

    public byte side;
    /**
     * Currently split into 4 nybbles (from lowest)
     * 0 = Corner connections (this wire should connect around a corner to something external)
     * 1 = Straight connections (this wire should connect to something external)
     * 2 = Internal connections (this wire should connect to something internal)
     * 3 = Internal open connections (this wire is not blocked by a cover/edge part and *could* connect through side)
     * bit 16 = connection to the centerpart
     * 5 = Render corner connections. Like corner connections but set to low if the other wire part is smaller than this (they render to us not us to them)
     */
    public int connMap;

    public void preparePlacement(int side, int meta)
    {
        this.side = (byte)(side^1);
    }

    @Override
    public void save(NBTTagCompound tag)
    {
        super.save(tag);
        tag.setByte("side", side);
        tag.setInteger("connMap", connMap);
    }

    @Override
    public void load(NBTTagCompound tag)
    {
        super.load(tag);
        side = tag.getByte("side");
        connMap = tag.getInteger("connMap");
    }

    @Override
    public void writeDesc(MCDataOutput packet)
    {
        packet.writeByte(side);
        packet.writeInt(connMap);
    }

    @Override
    public void readDesc(MCDataInput packet)
    {
        side = packet.readByte();
        connMap = packet.readInt();
    }

    @Override
    public void read(MCDataInput packet)
    {
        read(packet, packet.readUByte());
    }

    public void read(MCDataInput packet, int switch_key)
    {
        if (switch_key == 0)
        {
            connMap = packet.readInt();
            if (useStaticRenderer())
                tile().markRender();
        }
    }

    @Override
    public void onPartChanged(TMultiPart part)
    {
        if (!world().isRemote)
        {
            WirePropogator.logCalculation();

            boolean changed = updateInternalConnections();
            if (updateOpenConnections())
                changed |= updateExternalConnections();
            if (changed)
            {
                sendConnUpdate();
                WirePropogator.propogateTo(this, FORCE);
            }
            else
                WirePropogator.propogateTo(this, RISING);
        }
    }

    @Override
    public void onNeighborChanged()
    {
        if (!world().isRemote)
        {
            if (dropIfCantStay())
                return;

            WirePropogator.logCalculation();

            if (updateExternalConnections())
            {
                sendConnUpdate();
                WirePropogator.propogateTo(this, FORCE);
            }
            else
                WirePropogator.propogateTo(this, RISING);
        }
    }

    @Override
    public void onAdded()
    {
        super.onAdded();
        if (!world().isRemote)
        {
            updateOpenConnections();
            boolean changed = updateInternalConnections();
            changed |= updateExternalConnections();// don't use || because it's fail fast
            if (changed)
                sendConnUpdate();

            WirePropogator.propogateTo(this, RISING);
        }
    }

    @Override
    public void onRemoved()
    {
        super.onRemoved();

        if (!world().isRemote)
            for (int r = 0; r < 4; r++)
                if (maskConnects(r))
                    if ((connMap&1<<r) != 0)
                        notifyCornerChange(r);
                    else if ((connMap&0x10<<r) != 0)
                        notifyStraightChange(r);
    }

    @Override
    public void onChunkLoad()
    {
        if ((connMap&0x80000000) != 0) // compat with converters, recalc connections
        {
            if (dropIfCantStay())
                return;

            connMap = 0;

            updateInternalConnections();
            if (updateOpenConnections())
                updateExternalConnections();

            tile().markDirty();
        }
    }

    public void sendConnUpdate()
    {
        tile().getWriteStream(this).writeByte(0).writeInt(connMap);
    }

    public boolean canStay()
    {
        BlockCoord pos = new BlockCoord(tile()).offset(side);
        return BasicWireUtils.canPlaceWireOnSide(world(), pos.x, pos.y, pos.z, ForgeDirection.getOrientation(side^1), false);
    }

    public boolean dropIfCantStay()
    {
        if (!canStay())
        {
            drop();
            return true;
        }
        return false;
    }

    public void drop()
    {
        TileMultipart.dropItem(getItem(), world(), Vector3.fromTileEntityCenter(tile()));
        tile().remPart(this);
    }

    /**
     * Recalculates connections to blocks outside this space
     *
     * @return true if a new connection was added or one was removed
     */
    protected boolean updateExternalConnections()
    {
        int newConn = 0;
        for (int r = 0; r < 4; r++)
        {
            if (!maskOpen(r))
                continue;

            if (connectStraight(r))
                newConn |= 0x10<<r;
            else
            {
                int cnrMode = connectCorner(r);
                if (cnrMode != 0)
                {
                    newConn |= 1<<r;
                    if (cnrMode == 2)
                        newConn |= 0x100000<<r;// render flag
                }
            }
        }

        if (newConn != (connMap&0xF000FF))
        {
            int diff = connMap^newConn;
            connMap = connMap&~0xF000FF|newConn;

            // notify corner disconnections
            for (int r = 0; r < 4; r++)
                if ((diff&1<<r) != 0)
                    notifyCornerChange(r);

            return true;
        }
        return false;
    }

    /**
     * Recalculates connections to other parts within this space
     *
     * @return true if a new connection was added or one was removed
     */
    protected boolean updateInternalConnections()
    {
        int newConn = 0;
        for (int r = 0; r < 4; r++)
            if (connectInternal(r))
                newConn |= 0x100<<r;

        if (connectCenter())
            newConn |= 0x10000;

        if (newConn != (connMap&0x10F00))
        {
            connMap = connMap&~0x10F00|newConn;
            return true;
        }
        return false;
    }

    /**
     * Recalculates connections that can be made to other parts outside of this
     * space
     *
     * @return true if external connections should be recalculated
     */
    protected boolean updateOpenConnections()
    {
        int newConn = 0;
        for (int r = 0; r < 4; r++)
            if (connectionOpen(r))
                newConn |= 0x1000<<r;

        if (newConn != (connMap&0xF000))
        {
            connMap = connMap&~0xF000|newConn;
            return true;
        }
        return false;
    }

    public boolean connectionOpen(int r)
    {
        int absDir = Rotation.rotateSide(side, r);
        TMultiPart facePart = tile().partMap(absDir);
        if (facePart != null && (!(facePart instanceof WirePart) || !canConnectToType((WirePart)facePart)))
            return false;

        if (tile().partMap(PartMap.edgeBetween(side, absDir)) != null)
            return false;

        return true;
    }

    /**
     * Return a corner connection state.
     * 0 = No connection
     * 1 = Physical connection
     * 2 = Render connection
     */
    public int connectCorner(int r)
    {
        int absDir = Rotation.rotateSide(side, r);

        IConnectable tp = WireConnLib.getCorner(world(), side, r, new BlockCoord(tile()));
        if (tp != null)
        {
            boolean b = tp.connectCorner(this, Rotation.rotationTo(absDir^1, side^1), -1);
            if (b)
            {
                if (!renderThisCorner(tp))// let them connect to us
                    return 1;

                return 2;
            }
        }

        return connectCornerOverride(absDir) ? 2 : 0;
    }

    public boolean connectCornerOverride(int absDir)
    {
        return false;
    }

    public boolean connectStraight(int r)
    {
        IConnectable tp = WireConnLib.getStraight(world(), side, r, new BlockCoord(tile()));
        if (tp != null)
            return tp.connectStraight(this, (r+2)%4, -1);

        int absDir = Rotation.rotateSide(side, r);
        return connectStraightOverride(absDir);
    }

    public boolean connectStraightOverride(int absDir)
    {
        return false;
    }

    public boolean connectInternal(int r)
    {
        int absDir = Rotation.rotateSide(side, r);
        if (tile().partMap(PartMap.edgeBetween(absDir, side)) != null)
            return false;

        TMultiPart tp = tile().partMap(absDir);
        if (tp instanceof IConnectable)
            return ((IConnectable)tp).connectInternal(this, Rotation.rotationTo(absDir, side));

        return connectInternalOverride(tp, r);
    }

    public boolean connectInternalOverride(TMultiPart p, int r)
    {
        return false;
    }

    public boolean connectCenter()
    {
        IConnectable t = WireConnLib.getCenter(world(), new BlockCoord(tile()));
        if (t != null)
            return t.connectInternal(this, side);

        return false;
    }

    public boolean renderThisCorner(IConnectable part)
    {
        if (!(part instanceof WirePart))
            return true;

        WirePart wire = (WirePart)part;
        if (wire.getThickness() == getThickness())
            return side < wire.side;

        return wire.getThickness() > getThickness();
    }

    @Override
    public boolean connectCorner(IConnectable wire, int r, int edgeSide)
    {
        if (canConnectToType(wire) && maskOpen(r))
        {
            int oldConn = connMap;
            connMap |= 0x1<<r;
            if (renderThisCorner(wire))// render connection
                connMap |= 0x100000<<r;

            if (oldConn != connMap)
                sendConnUpdate();
            return true;
        }
        return false;
    }

    @Override
    public boolean connectStraight(IConnectable wire, int r, int edgeSide)
    {
        if (canConnectToType(wire) && maskOpen(r))
        {
            int oldConn = connMap;
            connMap |= 0x10<<r;
            if (oldConn != connMap)
                sendConnUpdate();
            return true;
        }
        return false;
    }

    @Override
    public boolean connectInternal(IConnectable wire, int r)
    {
        if (canConnectToType(wire))
        {
            int oldConn = connMap;
            connMap |= 0x100<<r;
            if (oldConn != connMap)
                sendConnUpdate();
            return true;
        }
        return false;
    }

    @Override
    public boolean canConnectCorner(int r)
    {
        return true;
    }

    public abstract boolean canConnectToType(IConnectable part);

    public void notifyCornerChange(int r)
    {
        int absDir = Rotation.rotateSide(side, r);

        BlockCoord pos = new BlockCoord(tile()).offset(absDir).offset(side);
        world().notifyBlockOfNeighborChange(pos.x, pos.y, pos.z, tile().getBlockType().blockID);
    }

    public void notifyStraightChange(int r)
    {
        int absDir = Rotation.rotateSide(side, r);

        BlockCoord pos = new BlockCoord(tile()).offset(absDir);
        world().notifyBlockOfNeighborChange(pos.x, pos.y, pos.z, tile().getBlockType().blockID);
    }

    public boolean maskConnects(int r)
    {
        return (connMap&0x111<<r) != 0;
    }

    public boolean maskOpen(int r)
    {
        return (connMap&0x1000<<r) != 0;
    }

    public void propogate(TMultiPart prev, int mode)
    {
        if (mode != FORCED)
            WirePropogator.addPartChange(this);

        for (int r = 0; r < 4; r++)
            if ((connMap&1<<r) != 0)
                propogateCorner(r, prev, mode);
            else if ((connMap&0x10<<r) != 0)
                propogateStraight(r, prev, mode);
            else if ((connMap&0x100<<r) != 0)
                propogateInternal(r, prev, mode);

        if ((connMap&0x10000) != 0)
            propogateCenter(prev, mode);

        propogateOther(mode);
    }

    public void propogateCorner(int r, TMultiPart prev, int mode)
    {
        int absDir = Rotation.rotateSide(side, r);
        BlockCoord pos = new BlockCoord(tile()).offset(absDir).offset(side);

        IConnectable tp = WireConnLib.getCorner(world(), side, r, new BlockCoord(tile()));

        if (tp == prev)
            return;
        if (propogateTo(tp, mode))
            return;

        WirePropogator.addNeighborChange(pos);
    }

    public void propogateStraight(int r, TMultiPart prev, int mode)
    {
        int absDir = Rotation.rotateSide(side, r);
        BlockCoord pos = new BlockCoord(tile()).offset(absDir);

        IConnectable tp = WireConnLib.getStraight(world(), side, r, new BlockCoord(tile()));
        if (tp == prev)
            return;
        if (propogateTo(tp, mode))
            return;

        WirePropogator.addNeighborChange(pos);
    }

    public void propogateInternal(int r, TMultiPart prev, int mode)
    {
        IConnectable tp = WireConnLib.getInsideFace(world(), side, r, new BlockCoord(tile()));
        if (tp == prev)
            return;
        propogateTo(tp, mode);
    }

    public void propogateCenter(TMultiPart prev, int mode)
    {
        IConnectable tp = WireConnLib.getCenter(world(), new BlockCoord(tile()));
        if (tp == prev)
            return;
        propogateTo(tp, mode);
    }

    public void propogateOther(int mode)
    {
    }

    public boolean propogateTo(Object part, int mode)
    {
        if (part instanceof IWirePart)
        {
            WirePropogator.propogateTo((IWirePart)part, this, mode);
            return true;
        }

        return false;
    }

    @Override
    public void onSignalUpdate()
    {
        tile().markDirty();
    }

    @Override
    public boolean isWireSide(int side)
    {
        return true;
    }

    protected abstract boolean debug(EntityPlayer player);

    protected abstract boolean test(EntityPlayer player);

    public abstract WireDef getWireType();

    public int getThickness()
    {
        return getWireType().thickness();
    }

    @Override
    public String getType()
    {
        return getWireType().wireType();
    }

    /**
     * START TILEMULTIPART INTERACTIONS *
     */
    @Override
    public float getStrength(MovingObjectPosition hit, EntityPlayer player)
    {
        return 4;
    }

    public ItemStack getItem()
    {
        return getWireType().getItemStack();
    }

    @Override
    public Iterable<ItemStack> getDrops()
    {
        return Arrays.asList(getItem());
    }

    @Override
    public ItemStack pickItem(MovingObjectPosition hit)
    {
        return getItem();
    }

    @Override
    public int getSlotMask()
    {
        return 1<<side;
    }

    @Override
    public Iterable<IndexedCuboid6> getSubParts()
    {
        return Arrays.asList(new IndexedCuboid6(0, selectionBounds[getThickness()][side]));
    }

    @Override
    public boolean occlusionTest(TMultiPart npart)
    {
        return NormalOcclusionTest.apply(this, npart);
    }

    @Override
    public Iterable<Cuboid6> getOcclusionBoxes()
    {
        return Arrays.asList(occlusionBounds[getThickness()][side]);
    }

    @Override
    public int redstoneConductionMap()
    {
        return 0xF;
    }

    @Override
    public boolean solid(int arg0)
    {
        return false;
    }

    @Override
    public boolean activate(EntityPlayer player, MovingObjectPosition hit, ItemStack held)
    {
        if (CommandDebug.WIRE_READING)
            return debug(player);
        if (held != null && held.itemID == ProjectRedCore.itemWireDebugger().itemID)
        {
            held.damageItem(1, player);
            player.swingItem();
            return test(player);
        }
        return false;
    }

    @SideOnly(Side.CLIENT)
    public Icon getIcon()
    {
        return getWireType().wireSprites()[0];
    }

    public int getColour()
    {
        return -1;
    }

    public boolean useStaticRenderer()
    {
        return Configurator.staticWires;
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void renderStatic(Vector3 pos, LazyLightMatrix olm, int pass)
    {
        if (pass == 0 && useStaticRenderer())
        {
            CCRenderState.setBrightness(world(), x(), y(), z());
            RenderWire.render(this, pos);
            CCRenderState.setColour(-1);
        }
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void renderDynamic(Vector3 pos, float frame, int pass)
    {
        if (pass == 0 && !useStaticRenderer())
        {
            GL11.glDisable(GL11.GL_LIGHTING);
            TextureUtils.bindAtlas(0);
            CCRenderState.useModelColours(true);
            CCRenderState.startDrawing(7);
            RenderWire.render(this, pos);
            CCRenderState.draw();
            CCRenderState.setColour(-1);
            GL11.glEnable(GL11.GL_LIGHTING);
        }
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void drawBreaking(RenderBlocks renderBlocks)
    {
        CCRenderState.reset();
        RenderWire.renderBreakingOverlay(renderBlocks.overrideBlockTexture, this);
    }

    @Override
    public boolean doesTick()
    {
        return false;
    }
}
