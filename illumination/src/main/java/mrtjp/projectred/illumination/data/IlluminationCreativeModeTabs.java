package mrtjp.projectred.illumination.data;

import codechicken.lib.colour.EnumColour;
import mrtjp.projectred.illumination.BlockLightType;
import mrtjp.projectred.illumination.MultipartLightType;
import mrtjp.projectred.illumination.ProjectRedIllumination;
import mrtjp.projectred.illumination.init.IlluminationBlocks;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.Block;
import net.minecraftforge.registries.RegistryObject;

public class IlluminationCreativeModeTabs {

    public static RegistryObject<CreativeModeTab> ILLUMINATION_CREATIVE_TAB;

    public static void register() {
        ILLUMINATION_CREATIVE_TAB = ProjectRedIllumination.CREATIVE_TABS.register("illumination",
                () -> CreativeModeTab.builder()
                        .icon(() -> new ItemStack(BlockLightType.ILLUMAR_LAMP.getBlock(EnumColour.RED.getWoolMeta(), true)))
                        .title(Component.translatable("itemGroup." + ProjectRedIllumination.MOD_ID))
                        .displayItems((params, output) -> {

                            // Block lights
                            for (BlockLightType lampType : BlockLightType.values()) {
                                // Non-inverted
                                for (Block b : lampType.allColors(false))  {
                                    output.accept(b);
                                }
                                // Inverted
                                for (Block b : lampType.allColors(true))  {
                                    output.accept(b);
                                }
                            }

                            // Illumar smart lamp
                            output.accept(IlluminationBlocks.ILLUMAR_SMART_LAMP.get());

                            // Multipart lights
                            for (MultipartLightType type : MultipartLightType.values()) {
                                // Non-inverted
                                for (Item i : type.getAllItems(false)) {
                                    output.accept(i);
                                }
                                // Inverted
                                for (Item i : type.getAllItems(true)) {
                                    output.accept(i);
                                }
                            }

                        })
                        .build());

    }
}
