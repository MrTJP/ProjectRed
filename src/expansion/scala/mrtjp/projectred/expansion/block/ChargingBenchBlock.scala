package mrtjp.projectred.expansion.block

import mrtjp.projectred.expansion.{BaseMachineBlock, TileChargingBench}
import net.minecraft.block.{Block, BlockState}
import net.minecraft.item.BlockItemUseContext
import net.minecraft.state.StateContainer

class ChargingBenchBlock extends BaseMachineBlock(() => new TileChargingBench) {

    override protected def createBlockStateDefinition(builder: StateContainer.Builder[Block, BlockState]): Unit = {
        super.createBlockStateDefinition(builder)
        builder.add(BaseMachineBlock.CHARGED_PROPERTY)
    }

    override def getStateForPlacement(context: BlockItemUseContext): BlockState = {
        super.getStateForPlacement(context)
            .setValue(BaseMachineBlock.CHARGED_PROPERTY, Boolean.box(false))
    }
}
