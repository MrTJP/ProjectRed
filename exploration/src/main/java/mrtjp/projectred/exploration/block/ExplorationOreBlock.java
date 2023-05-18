package mrtjp.projectred.exploration.block;

import net.minecraft.util.valueproviders.UniformInt;
import net.minecraft.world.level.block.OreBlock;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.material.Material;

import java.util.Random;

public class ExplorationOreBlock extends OreBlock {

    private static final Random EXP_DROP_RAND = new Random();

    private final int minExp;
    private final int maxExp;

    public ExplorationOreBlock(BlockBehaviour.Properties properties, int minExp, int maxExp) {
        super(properties, UniformInt.of(minExp, maxExp));
        this.minExp = minExp;
        this.maxExp = maxExp;
    }

    public ExplorationOreBlock(int minExp, int maxExp) {
        this(BlockBehaviour.Properties.of(Material.STONE)
                .strength(3.0F, 3.0F)
                .requiresCorrectToolForDrops()
                .sound(SoundType.STONE), minExp, maxExp);
    }
}
