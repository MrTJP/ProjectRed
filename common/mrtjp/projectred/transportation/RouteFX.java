package mrtjp.projectred.transportation;

import java.util.Random;

import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.core.fx.CoreParticle;
import mrtjp.projectred.core.fx.ParticleLogicArcToEntity;
import mrtjp.projectred.core.fx.ParticleLogicFade;
import mrtjp.projectred.core.fx.ParticleLogicIconShift;
import mrtjp.projectred.core.fx.ParticleLogicOrbitPoint;
import mrtjp.projectred.core.fx.ParticleLogicScale;
import mrtjp.projectred.core.fx.ParticleManagement;
import net.minecraft.client.Minecraft;
import net.minecraft.util.MathHelper;
import net.minecraft.world.World;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Vector3;

public class RouteFX
{
    private static final Random rand = new Random();

    public static final int color_receive = PRColors.ORANGE.ordinal();
    public static final int color_send = PRColors.PURPLE.ordinal();

    public static final int color_relay = PRColors.CYAN.ordinal();
    public static final int color_routeLost = PRColors.MAGENTA.ordinal();

    public static final int color_route = PRColors.RED.ordinal();
    public static final int color_sync = PRColors.LIGHT_BLUE.ordinal();

    public static final int color_request = PRColors.PINK.ordinal();

    public static final int color_checkInv = PRColors.WHITE.ordinal();

    public static void sendSpawnPacket(int color, int count, BlockCoord bc, World world)
    {
        if (world.isRemote)
            return;

        PacketCustom packet = new PacketCustom(TransportationSPH.channel, NetConstants.particle_Spawn);

        packet.writeByte(color).writeByte(count).writeCoord(bc);
        packet.sendPacketToAllAround(bc.x, bc.y, bc.z, 64.0D, world.provider.dimensionId);
    }

    public static void handleClientPacket(MCDataInput in, World world)
    {
        PRColors color = PRColors.get(in.readUByte());
        int count = in.readUByte();
        BlockCoord bc = in.readCoord();

        for (int i = 0; i < count; i++)
            spawn(color, bc, world);
    }

    public static void spawn(PRColors color, BlockCoord bc, World world)
    {
        if (!world.isRemote)
            return;

        double x = bc.x + 0.5 + MathHelper.getRandomDoubleInRange(rand, -1 / 4d, 1 / 4d);
        double y = bc.y + 0.5 + MathHelper.getRandomDoubleInRange(rand, -1 / 8d, 1 / 8d);
        double z = bc.z + 0.5 + MathHelper.getRandomDoubleInRange(rand, -1 / 4d, 1 / 4d);

        double x1 = bc.x + 0.5;
        double y1 = bc.y + 0.5 + 1;
        double z1 = bc.z + 0.5;

        CoreParticle c = ParticleManagement.instance.spawn(world, "flutter1", x, y, z);
        if (c != null)
        {
            ParticleLogicOrbitPoint orbit = new ParticleLogicOrbitPoint(new Vector3(x1, y1, z1), 1, false);
            orbit.setOrbitSpeed(0.5f * rand.nextDouble()).setTargetDistance(0.3D);
            orbit.setShrinkingOrbit(0.01, 0.01);

            ParticleLogicScale scale = new ParticleLogicScale(1, false);
            scale.setRate(-0.001F, -0.0001F * rand.nextFloat());
            scale.setTerminate(true);

            ParticleLogicIconShift iconshift = new ParticleLogicIconShift(1, false);
            iconshift.addIcon("flutter1");
            iconshift.addIcon("flutter2");
            iconshift.addIcon("flutter3");
            iconshift.addIcon("flutter4");
            iconshift.setTicksBetweenChange(3);

            c.setIgnoreMaxAge(true);
            c.setScale(0.05f + 0.02f * rand.nextFloat());
            c.setPRColor(color);

            c.addLogic(orbit);
            c.addLogic(scale);
            c.addLogic(iconshift);
        }
    }
}
