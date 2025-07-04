//package mrtjp.projectred.fabrication.item;
//
//import mrtjp.projectred.fabrication.init.FabricationItems;
//import net.minecraft.network.chat.Component;
//import net.minecraft.world.item.Item;
//import net.minecraft.world.item.ItemStack;
//import net.minecraft.world.item.TooltipFlag;
//
//import java.util.List;
//
//public class EtchedSiliconWaferItem extends Item {
//
//    public EtchedSiliconWaferItem() {
//        super(new Item.Properties()
//                .stacksTo(1));
//    }
//
//    @Override
//    public void appendHoverText(ItemStack stack, TooltipContext context, List<Component> tooltipList, TooltipFlag tooltipFlag) {
//        super.appendHoverText(stack, context, tooltipList, tooltipFlag);
//    }
//
//    public static ItemStack createFromPhotomaskSet(ItemStack photomaskSet, int waferLen, int dieLen, double defectChancePerLen) {
//        // Put NBT on new item stack and return
//        ItemStack output = new ItemStack(FabricationItems.ETCHED_SILICON_WAFER_ITEM.get());
//        output.setTag(tag);
//        return output;
//    }
//}
