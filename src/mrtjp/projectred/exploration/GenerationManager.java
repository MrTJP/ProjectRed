package mrtjp.projectred.exploration;

import cpw.mods.fml.common.IWorldGenerator;
import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.libmc.world.GeneratorCave;
import mrtjp.projectred.core.libmc.world.GeneratorOre;
import mrtjp.projectred.core.libmc.world.GeneratorVolcano;
import net.minecraft.util.MathHelper;
import net.minecraft.world.World;
import net.minecraft.world.WorldType;
import net.minecraft.world.chunk.IChunkProvider;
import net.minecraft.world.gen.ChunkProviderEnd;
import net.minecraft.world.gen.ChunkProviderHell;

import java.util.Random;

public class GenerationManager implements IWorldGenerator
{
    public static GenerationManager instance = new GenerationManager();

    @Override
    public void generate(Random r, int chunkX, int chunkZ, World w, IChunkProvider chunkGenerator, IChunkProvider chunkProvider)
    {
        if (chunkGenerator instanceof ChunkProviderHell || chunkGenerator instanceof ChunkProviderEnd)
            return;

        if (w.provider.terrainType == WorldType.FLAT)
            return;

        if (w.provider.dimensionId == -1 || w.provider.dimensionId == 1)
            return;

        if (w.provider.dimensionId == 0)
            runOverworldGeneration(r, chunkX, chunkZ, w);

        // Ruby
        if (Configurator.gen_Ruby) if (Configurator.gen_Ruby_resistance <= 0 || r.nextInt(Configurator.gen_Ruby_resistance) == 0)
            for (int i = 0; i < 2; i++)
            {
                int x = chunkX * 16 + r.nextInt(16);
                int y = r.nextInt(48);
                int z = chunkZ * 16 + r.nextInt(16);
                new GeneratorOre(ProjectRedExploration.blockOres(), OreDefs.ORERUBY().meta(), 5).generate(w, r, x, y, z);
            }

        // Saphire
        if (Configurator.gen_Sapphire) if (Configurator.gen_Sapphire_resistance <= 0 || r.nextInt(Configurator.gen_Sapphire_resistance) == 0)
            for (int i = 0; i < 2; i++)
            {
                int x = chunkX * 16 + r.nextInt(16);
                int y = r.nextInt(48);
                int z = chunkZ * 16 + r.nextInt(16);
                new GeneratorOre(ProjectRedExploration.blockOres(), OreDefs.ORESAPPHIRE().meta(), 5).generate(w, r, x, y, z);
            }

        // Peridot
        if (Configurator.gen_Peridot) if (Configurator.gen_Peridot_resistance <= 0 || r.nextInt(Configurator.gen_Peridot_resistance) == 0)
            for (int i = 0; i < 2; i++)
            {
                int x = chunkX * 16 + r.nextInt(16);
                int y = r.nextInt(48);
                int z = chunkZ * 16 + r.nextInt(16);
                new GeneratorOre(ProjectRedExploration.blockOres(), OreDefs.OREPERIDOT().meta(), 5).generate(w, r, x, y, z);
            }
    }

    public static void runOverworldGeneration(Random r, int chunkX, int chunkZ, World w)
    {
        // Marble caves
        if (Configurator.gen_MarbleCave) if (Configurator.gen_MarbleCave_resistance <= 0 || r.nextInt(Configurator.gen_MarbleCave_resistance) == 0)
        {
            int x = chunkX * 16 + r.nextInt(16);
            int y = 32 + r.nextInt(32);
            int z = chunkZ * 16 + r.nextInt(16);
            new GeneratorCave(ProjectRedExploration.blockDecoratives(), DecorativeStoneDefs.MARBLE().meta(), r.nextInt(4096)).generate(w, r, x, y, z);
        }

        // Volcanos
        if (Configurator.gen_Volcano) if (Configurator.gen_Volcano_resistance <= 0 || r.nextInt(Configurator.gen_Volcano_resistance) == 0)
        {
            int x = chunkX * 16 + r.nextInt(16);
            int y = r.nextInt(64);
            int z = chunkZ * 16 + r.nextInt(16);
            new GeneratorVolcano(ProjectRedExploration.blockDecoratives(), DecorativeStoneDefs.BASALT().meta(), MathHelper.getRandomIntegerInRange(r, 32000, 64000)).generate(w, r, x, y, z);
        }
    }
}