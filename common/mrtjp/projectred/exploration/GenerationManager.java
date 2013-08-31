package mrtjp.projectred.exploration;

import java.util.Random;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.exploration.BlockOre.EnumOre;
import mrtjp.projectred.exploration.BlockSpecialStone.EnumSpecialStone;
import net.minecraft.util.MathHelper;
import net.minecraft.world.World;
import net.minecraft.world.chunk.IChunkProvider;
import net.minecraft.world.gen.ChunkProviderEnd;
import net.minecraft.world.gen.ChunkProviderHell;
import cpw.mods.fml.common.IWorldGenerator;

public class GenerationManager implements IWorldGenerator {

    public static GenerationManager instance = new GenerationManager();

    @Override
    public void generate(Random rand, int chunkX, int chunkZ, World world, IChunkProvider chunkGenerator, IChunkProvider chunkProvider) {
        if (((chunkGenerator instanceof ChunkProviderHell)) || ((chunkGenerator instanceof ChunkProviderEnd))) 
            return;
        
        if (world.provider.dimensionId == -1 || world.provider.dimensionId == 1)
            return;
        
        if (world.provider.dimensionId == 0)
            runOverworldGeneration(rand, chunkX, chunkZ, world);

        // Ruby
        if (Configurator.gen_Ruby.getBoolean(true)) {
            for (int i = 0; i < 2; i++) {
                int x = chunkX * 16 + rand.nextInt(16);
                int y = rand.nextInt(48);
                int z = chunkZ * 16 + rand.nextInt(16);
                new GeneratorOre(ProjectRedExploration.blockOres.blockID, EnumOre.ORERUBY.meta, 5).generate(world, rand, x, y, z);
            }
        }

        // Saphire
        if (Configurator.gen_Sapphire.getBoolean(true)) {
            for (int i = 0; i < 2; i++) {
                int x = chunkX * 16 + rand.nextInt(16);
                int y = rand.nextInt(48);
                int z = chunkZ * 16 + rand.nextInt(16);
                new GeneratorOre(ProjectRedExploration.blockOres.blockID, EnumOre.ORESAPPHIRE.meta, 5).generate(world, rand, x, y, z);
            }
        }

        // Peridot
        if (Configurator.gen_Sapphire.getBoolean(true)) {
            for (int i = 0; i < 2; i++) {
                int x = chunkX * 16 + rand.nextInt(16);
                int y = rand.nextInt(48);
                int z = chunkZ * 16 + rand.nextInt(16);
                new GeneratorOre(ProjectRedExploration.blockOres.blockID, EnumOre.OREPERIDOT.meta, 5).generate(world, rand, x, y, z);
            }
        }
    }

    public static void runOverworldGeneration(Random rand, int chunkX, int chunkZ, World world) {
        // Marble caves
        if (Configurator.gen_MarbleCave.getBoolean(true)) {
            int x = chunkX * 16 + rand.nextInt(16);
            int y = 32 + rand.nextInt(32);
            int z = chunkZ * 16 + rand.nextInt(16);
            new GeneratorMetamorphicCave(ProjectRedExploration.blockStones.blockID, EnumSpecialStone.MARBLE.meta, rand.nextInt(4096)).generate(world, rand, x, y, z);
        }

        // Volcanos
        if (Configurator.gen_Volcano.getBoolean(true)) {
            int x = chunkX * 16 + rand.nextInt(16);
            int y = rand.nextInt(32);
            int z = chunkZ * 16 + rand.nextInt(16);
            new GeneratorVolcano(ProjectRedExploration.blockStones.blockID, EnumSpecialStone.BASALT.meta, MathHelper.getRandomIntegerInRange(rand, 32000, 64000)).generate(world, rand, x, y, z);
        }
    }

}
