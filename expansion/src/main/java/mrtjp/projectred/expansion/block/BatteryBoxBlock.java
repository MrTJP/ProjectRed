package mrtjp.projectred.expansion.block;

import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.expansion.tile.BatteryBoxTile;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.client.util.ITooltipFlag;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.BlockItemUseContext;
import net.minecraft.item.ItemStack;
import net.minecraft.loot.LootContext;
import net.minecraft.loot.LootParameters;
import net.minecraft.state.IntegerProperty;
import net.minecraft.state.StateContainer;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.world.IBlockReader;

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.List;

import static mrtjp.projectred.expansion.init.ExpansionUnlocal.UL_STORED_POWER_TOOLTIP;

public class BatteryBoxBlock extends ProjectRedBlock {

    public static final IntegerProperty CHARGE_LEVEL = IntegerProperty.create("charge_level", 0, 8);

    public BatteryBoxBlock() {
        super(WOODEN_PROPERTIES);
    }

    @Override
    protected TileEntity createTileEntityInstance(BlockState state, IBlockReader world) {
        return new BatteryBoxTile();
    }

    @Nullable
    @Override
    public BlockState getStateForPlacement(BlockItemUseContext context) {
        return this.defaultBlockState()
                .setValue(CHARGE_LEVEL, 0); // Tile takes care of updating state if placed with power
    }

    @Override
    protected void createBlockStateDefinition(StateContainer.Builder<Block, BlockState> builder) {
        builder.add(CHARGE_LEVEL);
    }

    @Override
    public List<ItemStack> getDrops(BlockState state, LootContext.Builder builder) {
        TileEntity tile = builder.getParameter(LootParameters.BLOCK_ENTITY);
        if (tile instanceof BatteryBoxTile) {
            BatteryBoxTile batteryBoxTile = (BatteryBoxTile) tile;
            return Collections.singletonList(batteryBoxTile.createStackWithStoredPower()); // Retain power inside itemstack
        }
        return super.getDrops(state, builder);
    }

    @Override
    public ItemStack getPickBlock(BlockState state, RayTraceResult target, IBlockReader world, BlockPos pos, PlayerEntity player) {
        TileEntity tile = world.getBlockEntity(pos);
        if (tile instanceof BatteryBoxTile)
            return ((BatteryBoxTile) tile).createStackWithStoredPower(); // Pick block with stored power

        return super.getPickBlock(state, target, world, pos, player);
    }

    @Override
    public void appendHoverText(ItemStack stack, @Nullable IBlockReader level, List<ITextComponent> toolTip, ITooltipFlag flag) {
        super.appendHoverText(stack, level, toolTip, flag);
        if (stack.hasTag()) {
            int power = stack.getTag().getInt(BatteryBoxTile.TAG_KEY_POWER_STORED);
            toolTip.add(new TranslationTextComponent(UL_STORED_POWER_TOOLTIP).append(": " + power + " / " + 8000).withStyle(TextFormatting.GRAY)); //TODO make this static constant
        }
    }
}
