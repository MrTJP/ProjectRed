package mrtjp.projectred.exploration;

import cpw.mods.fml.common.IWorldGenerator;
import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.exploration.BlockOre.EnumOre;
import mrtjp.projectred.exploration.BlockSpecialStone.EnumSpecialStone;
import mrtjp.projectred.exploration.BlockStainedLeaf.EnumDyeTrees;
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
        if (Configurator.gen_Ruby.getBoolean(true))
            for (int i = 0; i < 2; i++)
            {
                int x = chunkX * 16 + r.nextInt(16);
                int y = r.nextInt(48);
                int z = chunkZ * 16 + r.nextInt(16);
                new GeneratorOre(ProjectRedExploration.blockOres().blockID, EnumOre.ORERUBY.meta, 5).generate(w, r, x, y, z);
            }

        // Saphire
        if (Configurator.gen_Sapphire.getBoolean(true))
            for (int i = 0; i < 2; i++)
            {
                int x = chunkX * 16 + r.nextInt(16);
                int y = r.nextInt(48);
                int z = chunkZ * 16 + r.nextInt(16);
                new GeneratorOre(ProjectRedExploration.blockOres().blockID, EnumOre.ORESAPPHIRE.meta, 5).generate(w, r, x, y, z);
            }

        // Peridot
        if (Configurator.gen_Peridot.getBoolean(true))
            for (int i = 0; i < 2; i++)
            {
                int x = chunkX * 16 + r.nextInt(16);
                int y = r.nextInt(48);
                int z = chunkZ * 16 + r.nextInt(16);
                new GeneratorOre(ProjectRedExploration.blockOres().blockID, EnumOre.OREPERIDOT.meta, 5).generate(w, r, x, y, z);
            }
    }

    public static void runOverworldGeneration(Random r, int chunkX, int chunkZ, World w)
    {
        // Marble caves
        if (Configurator.gen_MarbleCave.getBoolean(true))
        {
            int x = chunkX * 16 + r.nextInt(16);
            int y = 32 + r.nextInt(32);
            int z = chunkZ * 16 + r.nextInt(16);
            new GeneratorMetamorphicCave(ProjectRedExploration.blockStones().blockID, EnumSpecialStone.MARBLE.meta, r.nextInt(4096)).generate(w, r, x, y, z);
        }

        // Volcanos
        if (Configurator.gen_Volcano.getBoolean(true))
        {
            int x = chunkX * 16 + r.nextInt(16);
            int y = r.nextInt(64);
            int z = chunkZ * 16 + r.nextInt(16);
            new GeneratorVolcano(ProjectRedExploration.blockStones().blockID, EnumSpecialStone.BASALT.meta, MathHelper.getRandomIntegerInRange(r, 32000, 64000)).generate(w, r, x, y, z);
        }

        // Dye trees
        if (Configurator.gen_dyeTrees.getBoolean(true))
        {
            int saplingMeta = r.nextInt(16);
            int x = chunkX * 16 + r.nextInt(16);
            int z = chunkZ * 16 + r.nextInt(16);
            int y = w.getHeightValue(x, z);
            if (r.nextDouble() < EnumDyeTrees.VALID_FOLIAGE[saplingMeta].growthChance / 3)
                new GeneratorColorTree(ProjectRedExploration.blockStainedLeaf().blockID).generateTreeAnyType(w, x, y, z, PRColors.get(r.nextInt(16)));
        }
    }
}
