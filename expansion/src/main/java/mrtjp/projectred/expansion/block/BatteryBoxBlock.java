package mrtjp.projectred.expansion.block;

import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.expansion.init.ExpansionBlocks;
import mrtjp.projectred.expansion.tile.BatteryBoxBlockEntity;
import net.minecraft.ChatFormatting;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.LevelReader;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.StateDefinition;
import net.minecraft.world.level.block.state.properties.IntegerProperty;
import net.minecraft.world.level.storage.loot.LootParams;
import net.minecraft.world.level.storage.loot.parameters.LootContextParams;
import net.minecraft.world.phys.HitResult;
import org.jetbrains.annotations.Nullable;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

import static mrtjp.projectred.expansion.init.ExpansionUnlocal.UL_STORED_POWER_TOOLTIP;

public class BatteryBoxBlock extends ProjectRedBlock {

    public static final IntegerProperty CHARGE_LEVEL = IntegerProperty.create("charge_level", 0, 8);

    public BatteryBoxBlock() {
        super(WOODEN_PROPERTIES);
    }

    @Nullable
    @Override
    public BlockEntity newBlockEntity(BlockPos pos, BlockState state) {
        return new BatteryBoxBlockEntity(pos, state);
    }

    @Override
    protected BlockEntityType<?> getBlockEntityType() {
        return ExpansionBlocks.BATTERY_BOX_BLOCK_ENTITY.get();
    }

    @Nullable
    @Override
    public BlockState getStateForPlacement(BlockPlaceContext context) {
        return this.defaultBlockState()
                .setValue(CHARGE_LEVEL, 0); // Tile takes care of updating state if placed with power
    }

    @Override
    protected void createBlockStateDefinition(StateDefinition.Builder<Block, BlockState> builder) {
        builder.add(CHARGE_LEVEL);
    }

    @Override
    public List<ItemStack> getDrops(BlockState state, LootParams.Builder builder) {
        BlockEntity tile = builder.getParameter(LootContextParams.BLOCK_ENTITY);
        if (tile instanceof BatteryBoxBlockEntity batteryBoxTile) {
            return Collections.singletonList(batteryBoxTile.createStackWithStoredPower()); // Retain power inside itemstack
        }
        return super.getDrops(state, builder);
    }

    @Override
    public ItemStack getCloneItemStack(BlockState state, HitResult target, LevelReader world, BlockPos pos, Player player) {
        BlockEntity tile = world.getBlockEntity(pos);
        if (tile instanceof BatteryBoxBlockEntity)
            return ((BatteryBoxBlockEntity) tile).createStackWithStoredPower(); // Pick block with stored power

        return super.getCloneItemStack(state, target, world, pos, player);
    }

    @Override
    public void appendHoverText(ItemStack stack, @Nullable BlockGetter level, List<Component> toolTip, TooltipFlag flag) {
        super.appendHoverText(stack, level, toolTip, flag);
        if (stack.hasTag()) {
            int power = Objects.requireNonNull(stack.getTag()).getInt(BatteryBoxBlockEntity.TAG_KEY_POWER_STORED);
            toolTip.add(Component.translatable(UL_STORED_POWER_TOOLTIP).append(": " + power + " / " + 8000).withStyle(ChatFormatting.GRAY)); //TODO make this static constant
        }
    }
}
