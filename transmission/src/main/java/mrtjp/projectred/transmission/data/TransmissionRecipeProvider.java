package mrtjp.projectred.transmission.data;

import codechicken.lib.datagen.recipe.RecipeProvider;
import codechicken.lib.util.CCLTags;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.data.DataGenerator;
import net.minecraft.item.Item;
import net.minecraft.tags.ItemTags;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.common.Tags;

import static mrtjp.projectred.transmission.ProjectRedTransmission.MOD_ID;
import static mrtjp.projectred.core.init.CoreReferences.*;
import static mrtjp.projectred.core.init.CoreTags.ELECTROTINE_ALLOY_INGOT_TAG;
import static mrtjp.projectred.core.init.CoreTags.RED_ALLOY_INGOT_TAG;
import static mrtjp.projectred.transmission.init.TransmissionTags.*;

public class TransmissionRecipeProvider extends RecipeProvider {

    public TransmissionRecipeProvider(DataGenerator generatorIn) {
        super(generatorIn);
    }

    @Override
    public String getName() {
        return "ProjectRed-Transmission: Recipes";
    }

    @Override
    protected void registerRecipes() {

        // Red alloy wire
        shapedRecipe(WireType.RED_ALLOY.getItem(), 12)
                .key('R', RED_ALLOY_INGOT_TAG)
                .patternLine(" R ")
                .patternLine(" R ")
                .patternLine(" R ");

        // Insulated wires
        for (WireType type : WireType.INSULATED_WIRES) {
            Item w = type.getItem();
            shapedRecipe(w, 12)
                    .key('W', ItemTags.bind(type.getColour().getWoolTagName().toString()))
                    .key('R', RED_ALLOY_INGOT_TAG)
                    .patternLine("WRW")
                    .patternLine("WRW")
                    .patternLine("WRW");
            // Re-colouring recipe
            shapelessRecipe(w, 1, new ResourceLocation(w.getRegistryName() + "_re_color"))
                    .addIngredient(INSULATED_WIRE_ITEM_TAG)
                    .addIngredient(ItemTags.bind(type.getColour().getDyeTagName().toString()));
        }

        // Bundled cables
        shapedRecipe(WireType.BUNDLED_NEUTRAL.getItem())
                .key('S', Tags.Items.STRING)
                .key('W', INSULATED_WIRE_ITEM_TAG)
                .patternLine("SWS")
                .patternLine("WWW")
                .patternLine("SWS");
        for (WireType type : WireType.COLOURED_BUNDLED_WIRES) {
            Item w = type.getItem();
            // Recolouring recipe
            shapelessRecipe(w, 1, new ResourceLocation(w.getRegistryName() + "_re_color"))
                    .addIngredient(BUNDLED_WIRE_ITEM_TAG)
                    .addIngredient(ItemTags.bind(type.getColour().getDyeTagName().toString()));
        }

        // Low Load power line
        shapedRecipe(WireType.POWER_LOWLOAD.getItem(), 12)
                .key('I', ELECTROTINE_ALLOY_INGOT_TAG)
                .key('B', CCLTags.Items.WOOL_BLUE)
                .key('Y', CCLTags.Items.WOOL_YELLOW)
                .patternLine("BIB")
                .patternLine("YIY")
                .patternLine("BIB");


        // Framed Red alloy wire
        framedWireRecipe(WireType.FRAMED_RED_ALLOY.getItem(), WireType.RED_ALLOY.getItem());

        // Framed insulated wires
        for (int i = 0; i < 16; i++) {
            WireType type = WireType.FRAMED_INSULATED_WIRES[i];
            Item w = type.getItem();

            framedWireRecipe(w, WireType.INSULATED_WIRES[i].getItem());

            // Re-colouring recipe
            shapelessRecipe(w, 1, new ResourceLocation(w.getRegistryName() + "_re_color"))
                    .addIngredient(FRAMED_INSULATED_WIRE_ITEM_TAG)
                    .addIngredient(ItemTags.bind(type.getColour().getDyeTagName().toString()));
        }

        // Framed bundled wires
        framedWireRecipe(WireType.FRAMED_BUNDLED_NEUTRAL.getItem(), WireType.BUNDLED_NEUTRAL.getItem());
        for (int i = 0; i < 16; i++) {
            WireType type = WireType.FRAMED_COLOURED_BUNDLED_WIRES[i];
            Item w = type.getItem();

            framedWireRecipe(w, WireType.COLOURED_BUNDLED_WIRES[i].getItem());

            // Re-colouring recipe
            shapelessRecipe(w, 1, new ResourceLocation(w.getRegistryName() + "_re_color"))
                    .addIngredient(FRAMED_BUNDLED_WIRE_ITEM_TAG)
                    .addIngredient(ItemTags.bind(type.getColour().getDyeTagName().toString()));
        }

        // Framed low load power line
        framedWireRecipe(WireType.FRAMED_POWER_LOWLOAD.getItem(), WireType.POWER_LOWLOAD.getItem());

        // Wired plate
        shapedRecipe(WIRED_PLATE_ITEM, 1, new ResourceLocation(MOD_ID, WIRED_PLATE_ITEM.getRegistryName().getPath()))
                .key('W', WireType.RED_ALLOY.getItem())
                .key('P', PLATE_ITEM)
                .patternLine("W")
                .patternLine("P");

        // Bundled plate
        shapedRecipe(BUNDLED_PLATE_ITEM, 1, new ResourceLocation(MOD_ID, BUNDLED_PLATE_ITEM.getRegistryName().getPath()))
                .key('W', BUNDLED_WIRE_ITEM_TAG)
                .key('P', PLATE_ITEM)
                .patternLine("W")
                .patternLine("P");
    }

    private void framedWireRecipe(Item result, Item input) {
        shapedRecipe(result)
                .key('S', Tags.Items.RODS_WOODEN)
                .key('I', input)
                .patternLine("SSS")
                .patternLine("SIS")
                .patternLine("SSS");
    }
}
