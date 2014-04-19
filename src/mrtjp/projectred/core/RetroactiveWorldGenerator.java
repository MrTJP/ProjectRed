package mrtjp.projectred.core;

import cpw.mods.fml.common.eventhandler.SubscribeEvent;
import cpw.mods.fml.common.gameevent.TickEvent.Phase;
import cpw.mods.fml.common.gameevent.TickEvent.WorldTickEvent;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.world.World;
import net.minecraft.world.WorldSavedData;
import net.minecraftforge.event.world.ChunkDataEvent;

import java.util.ArrayList;
import java.util.HashMap;

public class RetroactiveWorldGenerator
{
    public static final RetroactiveWorldGenerator instance = new RetroactiveWorldGenerator();

    String RetroGenID = Configurator.retroGenID;

    String RetroGenNBT = "RetrogenData_";

    private static ArrayList<IRetroGenerator> generators = new ArrayList<IRetroGenerator>();

    private static ArrayList<RetroGenQueue> generationQueue = new ArrayList<RetroGenQueue>();

    public static void registerRetroGenerator(IRetroGenerator g)
    {
        generators.add(g);
    }

    @SubscribeEvent
    public void chunkLoad(ChunkDataEvent.Load event)
    {
        queueRetroactiveGeneration(event);
    }

    private void queueRetroactiveGeneration(ChunkDataEvent.Load event)
    {
        World w = event.world;

        RetroDatabase database = getRetroDatabase(w);

        int x = event.getChunk().xPosition;
        int z = event.getChunk().zPosition;

        for (IRetroGenerator rg : generators)
            if (rg.shouldGenerateInLocation(w, event.getChunk()) && database.isGenerationNeeded(x, z, rg.getSubgenerationID()))
            {
                PRLogger.info("Chunk @(" + x + "," + z + ",DIM" + w.provider.dimensionId + ") has been marked for retroactive generation for ID " + rg.getSubgenerationID());
                generationQueue.add(new RetroGenQueue(w, x, z, rg));
                database.markChunkRetroGenerated(x, z, rg.getSubgenerationID());
            }
    }

    private RetroDatabase getRetroDatabase(World w)
    {
        RetroDatabase database = (RetroDatabase) w.perWorldStorage.loadData(RetroDatabase.class, RetroGenNBT + RetroGenID);

        if (database == null)
        {
            database = new RetroDatabase(RetroGenNBT + RetroGenID);
            w.perWorldStorage.setData(RetroGenNBT + RetroGenID, database);
            PRLogger.info("Created Retrogen database for dimension " + w.provider.dimensionId);
        }

        return database;
    }

    class RetroGenQueue
    {
        final World w;
        final int chunkX;
        final int chunkZ;

        final IRetroGenerator gen;

        public RetroGenQueue(World w, int chunkX, int chunkZ, IRetroGenerator gen)
        {
            this.w = w;
            this.chunkX = chunkX;
            this.chunkZ = chunkZ;
            this.gen = gen;
        }
    }


    @SubscribeEvent
    public void tickEnd(WorldTickEvent event)
    {
        if (event.phase == Phase.END) runRetroactiveGeneration(event.world);
    }

    private void runRetroactiveGeneration(World w1)
    {
        if (generationQueue.size() == 0)
            return;

        if (w1.getWorldTime() % 10 != 0)
            return;

        ArrayList<RetroGenQueue> removeQueue = new ArrayList<RetroGenQueue>();
        ArrayList<RetroGenQueue> iterationQueue = (ArrayList<RetroGenQueue>) generationQueue.clone();

        int count = 0;

        for (RetroGenQueue q : iterationQueue)
        {
            q.gen.generate(q.w.rand, q.w, q.chunkX, q.chunkZ);
            removeQueue.add(q);
            count++;
            if (count >= 32)
                break; // Dont do so many in one tick.
        }
        generationQueue.removeAll(removeQueue);

        PRLogger.info(count + " chunks have been re-generated. " + generationQueue.size() + " left.");
    }

    public static class RetroDatabase extends WorldSavedData
    {

        private class LocHash
        {
            int x;
            int z;

            public LocHash()
            {
            }

            public LocHash(int x, int z)
            {
                this.x = x;
                this.z = z;
            }

            @Override
            public boolean equals(Object o)
            {
                if (o instanceof LocHash)
                {
                    LocHash l = (LocHash) o;
                    return l.x == x && l.z == z;
                }
                return false;
            }

            @Override
            public int hashCode()
            {
                return (x ^ z) * 31 + 255;
            }

            public LocHash writeToNBT(String index, NBTTagCompound nbt)
            {
                nbt.setInteger(index + "locx", x);
                nbt.setInteger(index + "locz", z);
                return this;
            }

            public LocHash readFromNBT(String index, NBTTagCompound nbt)
            {
                x = nbt.getInteger(index + "locx");
                z = nbt.getInteger(index + "locz");
                return this;
            }
        }

        private HashMap<LocHash, NBTTagCompound> chunks = new HashMap<LocHash, NBTTagCompound>();

        public RetroDatabase(String par1Str)
        {
            super(par1Str);
        }

        public boolean isGenerationNeeded(int chunkX, int chunkZ, String subID)
        {
            NBTTagCompound chunkData = chunks.get(new LocHash(chunkX, chunkZ));
            return chunkData == null || !chunkData.hasKey(subID) || !chunkData.getBoolean(subID);
        }

        public void markChunkRetroGenerated(int chunkX, int chunkZ, String subID)
        {
            NBTTagCompound chunkData = chunks.get(new LocHash(chunkX, chunkZ));

            if (chunkData == null)
                chunkData = new NBTTagCompound();

            chunkData.setBoolean(subID, true);
            chunks.put(new LocHash(chunkX, chunkZ), chunkData);

            markDirty();
        }

        @Override
        public void readFromNBT(NBTTagCompound nbt)
        {
            int size = nbt.getInteger("size");
            for (int i = 0; i < size; i++)
                chunks.put(new LocHash().readFromNBT(i + "hash", nbt), nbt.getCompoundTag(i + "tag"));
        }

        @Override
        public void writeToNBT(NBTTagCompound nbt)
        {
            nbt.setInteger("size", chunks.size());
            int index = 0;
            for (LocHash i : chunks.keySet())
            {
                i.writeToNBT(index + "hash", nbt);
                nbt.setTag(index + "tag", chunks.get(i));
                index++;
            }
        }
    }
}
