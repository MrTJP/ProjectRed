package mrtjp.projectred.core;

import java.util.Random;

import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;

public interface IRetroGenerator
{
    /**
     * This is the string that is stored in the chunk data when generating. It
     * is checked every chunk load, and if it isn't there, the generation is run
     * again.
     * 
     * @return The unique ID of this structure
     */
    public String getSubgenerationID();

    /**
     * @param w The world that is generating.
     * @param c The chunk in which generation is being determined.
     * @return True if this can generate in the chunk.
     */
    public boolean shouldGenerateInLocation(World w, Chunk c);

    /**
     * Called when it is determined that generation should occur in this chunk.
     * 
     * @param r The world's random generator
     * @param w The world to generate in
     * @param chunkX The chunk x position
     * @param chunkZ The chunk y position
     * @param c The chunk
     * @param chunkData The NBT data associated with the chunk.
     */
    public void generate(Random r, World w, int chunkX, int chunkZ);
}
