package mrtjp.projectred.exploration;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IRetroGenerator;
import mrtjp.projectred.core.RetroactiveWorldGenerator;
import mrtjp.projectred.core.libmc.world.GeneratorCave;
import mrtjp.projectred.core.libmc.world.GeneratorOre;
import mrtjp.projectred.core.libmc.world.GeneratorVolcano;
import net.minecraft.util.MathHelper;
import net.minecraft.world.World;
import net.minecraft.world.WorldType;
import net.minecraft.world.chunk.Chunk;

import java.util.Random;

public class RetroGenerationManager
{
    public static void registerRetroGenerators()
    {
        if (Configurator.gen_Ruby)
            RetroactiveWorldGenerator.registerRetroGenerator(new RetrogenRuby());
        if (Configurator.gen_Sapphire)
            RetroactiveWorldGenerator.registerRetroGenerator(new RetrogenSapphire());
        if (Configurator.gen_Peridot)
            RetroactiveWorldGenerator.registerRetroGenerator(new RetrogenPeridot());
        if (Configurator.gen_MarbleCave)
            RetroactiveWorldGenerator.registerRetroGenerator(new RetrogenMarbleCave());
        if (Configurator.gen_Volcano)
            RetroactiveWorldGenerator.registerRetroGenerator(new RetrogenVolcano());
//        if (Configurator.gen_dyeTrees)//TODO replace
//            RetroactiveWorldGenerator.registerRetroGenerator(new RetrogenDyeTrees());
    }

    static class RetrogenRuby implements IRetroGenerator
    {

        @Override
        public String getSubgenerationID()
        {
            return "pr_ruby";
        }

        @Override
        public boolean shouldGenerateInLocation(World w, Chunk c)
        {
            int id = w.provider.dimensionId;
            return id != -1 && id != 1;
        }

        @Override
        public void generate(Random r, World w, int chunkX, int chunkZ)
        {
            // Ruby
            for (int i = 0; i < 2; i++)
            {
                int x = chunkX * 16 + r.nextInt(16);
                int y = r.nextInt(48);
                int z = chunkZ * 16 + r.nextInt(16);
                new GeneratorOre(ProjectRedExploration.blockOres(), OreDefs.ORERUBY().meta(), 5).generate(w, r, x, y, z);
            }
        }
    }

    static class RetrogenSapphire implements IRetroGenerator
    {
        @Override
        public String getSubgenerationID()
        {
            return "pr_sapphire";
        }

        @Override
        public boolean shouldGenerateInLocation(World w, Chunk c)
        {
            int id = w.provider.dimensionId;
            return id != -1 && id != 1;
        }

        @Override
        public void generate(Random r, World w, int chunkX, int chunkZ)
        {
            // Saphire
            for (int i = 0; i < 2; i++)
            {
                int x = chunkX * 16 + r.nextInt(16);
                int y = r.nextInt(48);
                int z = chunkZ * 16 + r.nextInt(16);
                new GeneratorOre(ProjectRedExploration.blockOres(), OreDefs.ORESAPPHIRE().meta(), 5).generate(w, r, x, y, z);
            }
        }
    }

    static class RetrogenPeridot implements IRetroGenerator
    {
        @Override
        public String getSubgenerationID()
        {
            return "pr_peridot";
        }

        @Override
        public boolean shouldGenerateInLocation(World w, Chunk c)
        {
            int id = w.provider.dimensionId;
            return id != -1 && id != 1;
        }

        @Override
        public void generate(Random r, World w, int chunkX, int chunkZ)
        {
            // Peridot
            for (int i = 0; i < 2; i++)
            {
                int x = chunkX * 16 + r.nextInt(16);
                int y = r.nextInt(48);
                int z = chunkZ * 16 + r.nextInt(16);
                new GeneratorOre(ProjectRedExploration.blockOres(), OreDefs.OREPERIDOT().meta(), 5).generate(w, r, x, y, z);
            }
        }
    }

    static class RetrogenMarbleCave implements IRetroGenerator
    {
        @Override
        public String getSubgenerationID()
        {
            return "pr_marbleCave";
        }

        @Override
        public boolean shouldGenerateInLocation(World w, Chunk c)
        {
            return w.provider.dimensionId == 0;
        }

        @Override
        public void generate(Random r, World w, int chunkX, int chunkZ)
        {
            // Marble caves
            int x = chunkX * 16 + r.nextInt(16);
            int y = 32 + r.nextInt(32);
            int z = chunkZ * 16 + r.nextInt(16);
            new GeneratorCave(ProjectRedExploration.blockDecoratives(), DecorativeStoneDefs.MARBLE().meta(), r.nextInt(4096)).generate(w, r, x, y, z);
        }
    }

    static class RetrogenVolcano implements IRetroGenerator
    {
        @Override
        public String getSubgenerationID()
        {
            return "pr_volcano";
        }

        @Override
        public boolean shouldGenerateInLocation(World w, Chunk c)
        {
            if (w.provider.terrainType == WorldType.FLAT)
                return false;

            return w.provider.dimensionId == 0;
        }

        @Override
        public void generate(Random r, World w, int chunkX, int chunkZ)
        {
            // Volcanos
            int x = chunkX * 16 + r.nextInt(16);
            int y = r.nextInt(64);
            int z = chunkZ * 16 + r.nextInt(16);
            new GeneratorVolcano(ProjectRedExploration.blockDecoratives(), DecorativeStoneDefs.BASALT().meta(), MathHelper.getRandomIntegerInRange(r, 32000, 64000)).generate(w, r, x, y, z);
        }
    }

//    static class RetrogenDyeTrees implements IRetroGenerator//TODO replace
//    {
//        @Override
//        public String getSubgenerationID()
//        {
//            return "pr_dyetrees";
//        }
//
//        @Override
//        public boolean shouldGenerateInLocation(World w, Chunk c)
//        {
//            int id = w.provider.dimensionId;
//
//            if (w.provider.terrainType == WorldType.FLAT)
//                return false;
//
//            return id == 0;
//        }
//
//        @Override
//        public void generate(Random r, World w, int chunkX, int chunkZ)
//        {
//            // Dye trees
//            int saplingMeta = r.nextInt(16);
//            int x = chunkX * 16 + r.nextInt(16);
//            int z = chunkZ * 16 + r.nextInt(16);
//            int y = w.getHeightValue(x, z);
//            if (r.nextDouble() < EnumDyeTrees.VALID_FOLIAGE[saplingMeta].growthChance / 3)
//                new GeneratorColorTree(ProjectRedExploration.blockStainedLeaf()).generateTreeAnyType(w, x, y, z, PRColors.get(r.nextInt(16)));
//        }
//    }
}
