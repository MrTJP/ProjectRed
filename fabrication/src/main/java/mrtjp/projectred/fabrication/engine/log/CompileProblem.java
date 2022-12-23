package mrtjp.projectred.fabrication.engine.log;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.gui.ICRenderNode;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import java.util.List;

public abstract class CompileProblem {

    public final CompileProblemType type;
    public final CompileProblemSeverity severity;

    public CompileProblem(CompileProblemType type, CompileProblemSeverity severity) {
        this.type = type;
        this.severity = severity;
    }

    public abstract void save(CompoundTag tag);

    public abstract void load(CompoundTag tag);

    public abstract void writeDesc(MCDataOutput out);

    public abstract void readDesc(MCDataInput in);

    public abstract Component getName();

    public abstract void buildToolTip(List<Component> tooltip, TileCoord hoverPosition);

    public abstract void buildToolTip(List<Component> tooltip);

    @OnlyIn(Dist.CLIENT)
    public abstract void renderOverlay(Vector3 mousePosition, CCRenderState ccrs, MultiBufferSource getter, PoseStack matrixStack);
}
