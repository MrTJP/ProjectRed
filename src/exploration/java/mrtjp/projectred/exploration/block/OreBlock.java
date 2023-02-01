package mrtjp.projectred.exploration.block;

import net.minecraft.block.AbstractBlock;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.block.SoundType;
import net.minecraft.block.material.Material;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IWorldReader;
import net.minecraftforge.common.ToolType;

import java.util.Random;

public class OreBlock extends Block {

    private static final Random EXP_DROP_RAND = new Random();

    private final int minExp;
    private final int maxExp;

    public OreBlock(AbstractBlock.Properties properties, int harvestLevel, int minExp, int maxExp) {
        super(properties);
        this.minExp = minExp;
        this.maxExp = maxExp;
    }

    public OreBlock(int harvestLevel, int minExp, int maxExp) {
        this(AbstractBlock.Properties.of(Material.STONE)
                .strength(3.0F, 3.0F)
                .harvestLevel(harvestLevel)
                .requiresCorrectToolForDrops()
                .harvestTool(ToolType.PICKAXE)
                .sound(SoundType.STONE), harvestLevel, minExp, maxExp);
    }

    @Override
    public int getExpDrop(BlockState state, IWorldReader world, BlockPos pos, int fortune, int silktouch) {
        return silktouch != 0 ? 0 : minExp + EXP_DROP_RAND.nextInt(maxExp - minExp + 1);
    }
}
