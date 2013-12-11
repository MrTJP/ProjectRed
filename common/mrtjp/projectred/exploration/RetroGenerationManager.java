package mrtjp.projectred.exploration;

import java.util.Random;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.api.IRetroGenerator;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.RetroactiveWorldGenerator;
import mrtjp.projectred.exploration.BlockStainedLeaf.EnumDyeTrees;
import mrtjp.projectred.exploration.BlockOre.EnumOre;
import mrtjp.projectred.exploration.BlockSpecialStone.EnumSpecialStone;
import net.minecraft.block.Block;
import net.minecraft.util.MathHelper;
import net.minecraft.world.World;
import net.minecraft.world.WorldType;
import net.minecraft.world.chunk.Chunk;

public class RetroGenerationManager
{
    public static void registerRetroGenerators()
    {
        if (Configurator.gen_Ruby.getBoolean(true))
            RetroactiveWorldGenerator.registerRetroGenerator(new RetrogenRuby());
        if (Configurator.gen_Sapphire.getBoolean(true))
            RetroactiveWorldGenerator.registerRetroGenerator(new RetrogenSapphire());
        if (Configurator.gen_Peridot.getBoolean(true))
            RetroactiveWorldGenerator.registerRetroGenerator(new RetrogenPeridot());
        if (Configurator.gen_MarbleCave.getBoolean(true))
            RetroactiveWorldGenerator.registerRetroGenerator(new RetrogenMarbleCave());
        if (Configurator.gen_Volcano.getBoolean(true))
            RetroactiveWorldGenerator.registerRetroGenerator(new RetrogenVolcano());
        if (Configurator.gen_dyeTrees.getBoolean(true))
            RetroactiveWorldGenerator.registerRetroGenerator(new RetrogenDyeTrees());
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
                new GeneratorOre(ProjectRedExploration.blockOres.blockID, EnumOre.ORERUBY.meta, 5).generate(w, r, x, y, z);
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
                new GeneratorOre(ProjectRedExploration.blockOres.blockID, EnumOre.ORESAPPHIRE.meta, 5).generate(w, r, x, y, z);
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
                new GeneratorOre(ProjectRedExploration.blockOres.blockID, EnumOre.OREPERIDOT.meta, 5).generate(w, r, x, y, z);
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
            new GeneratorMetamorphicCave(ProjectRedExploration.blockStones.blockID, EnumSpecialStone.MARBLE.meta, r.nextInt(4096)).generate(w, r, x, y, z);
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
            int y = r.nextInt(32);
            int z = chunkZ * 16 + r.nextInt(16);
            new GeneratorVolcano(ProjectRedExploration.blockStones.blockID, EnumSpecialStone.BASALT.meta, MathHelper.getRandomIntegerInRange(r, 32000, 64000)).generate(w, r, x, y, z);
        }
    }

    static class RetrogenDyeTrees implements IRetroGenerator
    {
        @Override
        public String getSubgenerationID()
        {
            return "pr_dyetrees";
        }

        @Override
        public boolean shouldGenerateInLocation(World w, Chunk c)
        {
            int id = w.provider.dimensionId;

            if (w.provider.terrainType == WorldType.FLAT)
                return false;

            return id == 0;
        }

        @Override
        public void generate(Random r, World w, int chunkX, int chunkZ)
        {
            // Dye trees
            int saplingMeta = r.nextInt(16);
            int x = chunkX * 16 + r.nextInt(16);
            int z = chunkZ * 16 + r.nextInt(16);
            int y = w.getHeightValue(x, z);
            if (r.nextDouble() < EnumDyeTrees.VALID_FOLIAGE[saplingMeta].growthChance / 3)
                new GeneratorCustomTree(false, 5, Block.wood.blockID, 0, ProjectRedExploration.blockStainedLeaf.blockID, saplingMeta, -1, -1).generate(w, r, x, y, z);
        }
    }
}
