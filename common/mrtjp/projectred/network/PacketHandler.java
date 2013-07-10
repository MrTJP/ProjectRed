package mrtjp.projectred.network;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import mrtjp.projectred.network.abstractpackets.ModernPacket;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.network.INetworkManager;
import net.minecraft.network.packet.Packet250CustomPayload;

import com.google.common.reflect.ClassPath;
import com.google.common.reflect.ClassPath.ClassInfo;

import cpw.mods.fml.common.network.IPacketHandler;
import cpw.mods.fml.common.network.Player;

public class PacketHandler implements IPacketHandler {

	public static List<ModernPacket> packetlist;
	public static Map<Class<? extends ModernPacket>, ModernPacket> packetmap;

	public static <T extends ModernPacket> T getPacket(Class<T> clazz) {
		return (T) packetmap.get(clazz).template();
	}

	public PacketHandler() {
		try {
			final List<ClassInfo> classes = new ArrayList<ClassInfo>(ClassPath.from(this.getClass().getClassLoader()).getTopLevelClassesRecursive("mrtjp.projectred.network.packets"));
			Collections.sort(classes, new Comparator<ClassInfo>() {
				@Override
				public int compare(ClassInfo o1, ClassInfo o2) {
					return o1.getSimpleName().compareTo(o2.getSimpleName());
				}
			});

			packetlist = new ArrayList<ModernPacket>(classes.size());
			packetmap = new HashMap<Class<? extends ModernPacket>, ModernPacket>(classes.size());

			int currentid = 0;
			System.out.println("Loading " + classes.size() + " Packets");

			for (ClassInfo c : classes) {
				final Class<?> cls = c.load();
				final ModernPacket instance = (ModernPacket) cls.getConstructors()[0].newInstance(currentid);
				packetlist.add(instance);
				packetmap.put((Class<? extends ModernPacket>) cls, instance);
				currentid++;
			}

		} catch (Throwable e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public void onPacketData(INetworkManager manager, Packet250CustomPayload packet, Player player) {
		final DataInputStream data = new DataInputStream(new ByteArrayInputStream(packet.data));
		onPacketData(data, player);
	}

	public static void onPacketData(DataInputStream data, Player player) {
		try {
			final int packetID = data.read();
			final ModernPacket packet = packetlist.get(packetID).template();
			packet.readData(data);
			packet.processPacket((EntityPlayer) player);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
