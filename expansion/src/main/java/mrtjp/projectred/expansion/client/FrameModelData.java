package mrtjp.projectred.expansion.client;

import net.neoforged.neoforge.client.model.data.ModelProperty;

public record FrameModelData(int mask) {
    public static final ModelProperty<FrameModelData> DATA = new ModelProperty<>();
}
