package mrtjp.projectred.expansion.block

import mrtjp.projectred.expansion.{BaseMachineBlock, RotatableMachineBlock, TileInductiveFurnace}
import net.minecraft.block.{Block, BlockState}
import net.minecraft.item.BlockItemUseContext
import net.minecraft.state.StateContainer

class InductionFurnaceBlock extends RotatableMachineBlock(() => new TileInductiveFurnace) {

    override protected def createBlockStateDefinition(builder: StateContainer.Builder[Block, BlockState]): Unit = {
        super.createBlockStateDefinition(builder)
        builder.add(BaseMachineBlock.CHARGED_PROPERTY)
        builder.add(BaseMachineBlock.WORKING_PROPERTY)
    }

    override def getStateForPlacement(context: BlockItemUseContext): BlockState = {
        super.getStateForPlacement(context)
            .setValue(BaseMachineBlock.CHARGED_PROPERTY, Boolean.box(false))
            .setValue(BaseMachineBlock.WORKING_PROPERTY, Boolean.box(false))
    }
}
