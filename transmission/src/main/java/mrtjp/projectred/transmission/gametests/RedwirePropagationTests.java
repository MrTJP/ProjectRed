package mrtjp.projectred.transmission.gametests;

import mrtjp.projectred.transmission.ProjectRedTransmission;
import net.minecraft.core.BlockPos;
import net.minecraft.gametest.framework.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.level.block.LeverBlock;
import net.minecraft.world.level.block.RedstoneLampBlock;
import net.neoforged.neoforge.gametest.GameTestHolder;
import net.neoforged.neoforge.gametest.PrefixGameTestTemplate;

@GameTestHolder(ProjectRedTransmission.MOD_ID)
@PrefixGameTestTemplate(false)
public class RedwirePropagationTests {

    @GameTest(template = "alloy_rw_wraparound_rs_block")
    public static void redAlloyPropagatesAroundRSBlock(GameTestHelper helper) {
        // Red alloy wires should propagate signal around a redsone block,
        // and also power the lamp underneath

        // Positions
        var lever = new BlockPos(0, 2, 1);
        var lampA = new BlockPos(3, 2, 1);
        var lampB = new BlockPos(6, 2, 1);

        // Assert start condition
        helper.assertBlockState(lever, state -> !state.getValue(LeverBlock.POWERED), () -> "Lever should be off!");
        helper.assertBlockState(lampA, state -> !state.getValue(RedstoneLampBlock.LIT), () -> "Lamp should be off!");
        helper.assertBlockState(lampB, state -> !state.getValue(RedstoneLampBlock.LIT), () -> "Lamp should be off!");

        // Pull lever, wait 2 ticks, then check lamp states
        helper.startSequence()
                .thenExecute(() -> helper.setBlock(lever, helper.getBlockState(lever).setValue(LeverBlock.POWERED, true)))
                .thenIdle(2)
                .thenExecute(() -> {
                    helper.assertBlockState(lever, state -> state.getValue(LeverBlock.POWERED), () -> "Lever should be on!");
                    helper.assertBlockState(lampA, state -> state.getValue(RedstoneLampBlock.LIT), () -> "Lamp A should be on!");
                    helper.assertBlockState(lampB, state -> state.getValue(RedstoneLampBlock.LIT), () -> "Lamp B should be on!");
                }).thenSucceed();
   }

    @GameTest(template = "insulated_rw_wraparound_rs_block")
    public static void insulatedWirePropagatesAroundRSBlock(GameTestHelper helper) {
        // Insulated wires should propagate signal around a redsone block,
        // but NOT power the lamp underneath

        // Positions
        var lever = new BlockPos(0, 2, 1);
        var lampA = new BlockPos(3, 2, 1);
        var lampB = new BlockPos(6, 2, 1);

        // Assert start condition
        helper.assertBlockState(lever, state -> !state.getValue(LeverBlock.POWERED), () -> "Lever should be off!");
        helper.assertBlockState(lampA, state -> !state.getValue(RedstoneLampBlock.LIT), () -> "Lamp should be off!");
        helper.assertBlockState(lampB, state -> !state.getValue(RedstoneLampBlock.LIT), () -> "Lamp should be off!");

        // Pull lever, wait 2 ticks, then check lamp states
        helper.startSequence()
                .thenExecute(() -> helper.setBlock(lever, helper.getBlockState(lever).setValue(LeverBlock.POWERED, true)))
                .thenIdle(2)
                .thenExecute(() -> {
                    helper.assertBlockState(lever, state -> state.getValue(LeverBlock.POWERED), () -> "Lever should be on!");
                    helper.assertBlockState(lampA, state -> !state.getValue(RedstoneLampBlock.LIT), () -> "Lamp A should be off!");
                    helper.assertBlockState(lampB, state -> state.getValue(RedstoneLampBlock.LIT), () -> "Lamp B should be on!");
                }).thenSucceed();
    }

}
