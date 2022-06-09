package mrtjp.projectred.expansion.block

import mrtjp.projectred.expansion.{BaseMachineBlock, TileBatteryBox}
import net.minecraft.block.{Block, BlockState}
import net.minecraft.state.StateContainer

class BatteryBoxBlock extends BaseMachineBlock(() => new TileBatteryBox) {

    override protected def createBlockStateDefinition(builder: StateContainer.Builder[Block, BlockState]): Unit = {
        super.createBlockStateDefinition(builder)
        builder.add(BaseMachineBlock.CHARGE_LEVEL_PROPERTY)
    }
}
