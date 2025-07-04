package mrtjp.projectred.exploration.init;

import net.minecraft.Util;
import net.minecraft.core.Holder;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.world.item.ArmorItem;
import net.minecraft.world.item.ArmorMaterial;
import net.minecraft.world.item.crafting.Ingredient;

import java.util.EnumMap;
import java.util.List;
import java.util.function.Supplier;

import static mrtjp.projectred.core.init.CoreTags.*;
import static mrtjp.projectred.exploration.ProjectRedExploration.ARMOR_MATERIALS;
import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;

@SuppressWarnings("NotNullFieldNotInitialized")
public class ExplorationArmorMaterials {

    public static final String ID_RUBY_ARMOR_MATERIAL = "ruby";
    public static final String ID_SAPPHIRE_ARMOR_MATERIAL = "sapphire";
    public static final String ID_PERIDOT_ARMOR_MATERIAL = "peridot";

    public static Holder<ArmorMaterial> RUBY_ARMOR_MATERIAL;
    public static Holder<ArmorMaterial> SAPPHIRE_ARMOR_MATERIAL;
    public static Holder<ArmorMaterial> PERIDOT_ARMOR_MATERIAL;


    public static void register() {

        EnumMap<ArmorItem.Type, Integer> defenseMap = Util.make(new EnumMap<>(ArmorItem.Type.class), map -> {
            map.put(ArmorItem.Type.BOOTS, 3);
            map.put(ArmorItem.Type.LEGGINGS, 6);
            map.put(ArmorItem.Type.CHESTPLATE, 8);
            map.put(ArmorItem.Type.HELMET, 3);
            map.put(ArmorItem.Type.BODY, 11);
        });

        RUBY_ARMOR_MATERIAL = ARMOR_MATERIALS.register(ID_RUBY_ARMOR_MATERIAL, () -> create(
                defenseMap,
                10,
                SoundEvents.ARMOR_EQUIP_DIAMOND,
                1.25F,
                0.0F,
                () -> Ingredient.of(RUBY_GEM_TAG),
                ID_RUBY_ARMOR_MATERIAL
        ));

        SAPPHIRE_ARMOR_MATERIAL = ARMOR_MATERIALS.register(ID_SAPPHIRE_ARMOR_MATERIAL, () -> create(
                defenseMap,
                10,
                SoundEvents.ARMOR_EQUIP_DIAMOND,
                1.25F,
                0.0F,
                () -> Ingredient.of(SAPPHIRE_GEM_TAG),
                ID_SAPPHIRE_ARMOR_MATERIAL
        ));

        PERIDOT_ARMOR_MATERIAL = ARMOR_MATERIALS.register(ID_PERIDOT_ARMOR_MATERIAL, () -> create(
                defenseMap,
                14,
                SoundEvents.ARMOR_EQUIP_DIAMOND,
                1.25F,
                0.0F,
                () -> Ingredient.of(PERIDOT_GEM_TAG),
                ID_PERIDOT_ARMOR_MATERIAL
        ));

    }

    private static ArmorMaterial create(
            EnumMap<ArmorItem.Type, Integer> defenseMap,
            int enchantmentValue,
            Holder<SoundEvent> equipSound,
            float toughness,
            float knockbackResistance,
            Supplier<Ingredient> repairIngredient,
            String layerTextureName
    ) {
        List<ArmorMaterial.Layer> list = List.of(new ArmorMaterial.Layer(ResourceLocation.fromNamespaceAndPath(MOD_ID, layerTextureName)));
        return create(defenseMap, enchantmentValue, equipSound, toughness, knockbackResistance, repairIngredient, list);
    }

    private static ArmorMaterial create(
            EnumMap<ArmorItem.Type, Integer> defenseMap,
            int enchantmentValue,
            Holder<SoundEvent> equipSound,
            float toughness,
            float knockbackResistance,
            Supplier<Ingredient> repairIngredient,
            List<ArmorMaterial.Layer> layers
    ) {
        EnumMap<ArmorItem.Type, Integer> defenseMapCopy = new EnumMap<>(ArmorItem.Type.class);

        for (ArmorItem.Type type : ArmorItem.Type.values()) {
            defenseMapCopy.put(type, defenseMap.get(type));
        }

        return new ArmorMaterial(
                defenseMapCopy,
                enchantmentValue,
                equipSound,
                repairIngredient,
                layers,
                toughness,
                knockbackResistance);
    }
}
