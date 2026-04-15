package mrtjp.projectred.expansion.mixin;

import net.minecraft.core.Direction;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import java.util.EnumSet;

/**
 * Provides a mechanism to supress culling on certain sides. Used by {@link SodiumBlockRendererMixin}.
 */
@Mixin(targets = "net.caffeinemc.mods.sodium.client.render.frapi.render.AbstractBlockRenderContext")
public abstract class SodiumAbstractBlockRenderContextMixin {

    @Unique
    protected EnumSet<Direction> cullDisabledSides = EnumSet.noneOf(Direction.class);

    @Inject(method = "isFaceCulled", at = @At("HEAD"), cancellable = true)
    public void isFaceCulled(Direction face, CallbackInfoReturnable<Boolean> cir) {
        if (cullDisabledSides.contains(face)) {
            cir.setReturnValue(false);
        }
    }
}
