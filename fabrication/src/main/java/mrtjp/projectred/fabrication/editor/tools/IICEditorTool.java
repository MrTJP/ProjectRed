package mrtjp.projectred.fabrication.editor.tools;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.editor.ICEditorToolType;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.network.chat.Component;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import java.util.List;

public interface IICEditorTool {

    ICEditorToolType getToolType();

    void bindEditor(ICWorkbenchEditor editor);

    void readPacket(MCDataInput input);

    boolean toolStart(Vector3 mousePosition, int glfwMouseButton);

    boolean toolReleased(Vector3 mousePosition, int glfwMouseButton);

    boolean toolDragged(Vector3 mousePosition, Vector3 delta, int glfwMouseButton);

    boolean toolScrolled(Vector3 mousePosition, double scroll);

    boolean toolCanceled();

    void toolLayerChanged(int previousLayer, int newLayer);

    void toolActivated();

    void toolDeactivated();

    default boolean toolKeyPressed(Vector3 mousePosition, int glfwKeyCode, int glfwFlags) {
        return false;
    }

    default boolean toolKeyReleased(Vector3 mousePosition, int glfwKeyCode, int glfwFlags) {
        return false;
    }

    @OnlyIn(Dist.CLIENT)
    void renderOverlay(Vector3 mousePosition, boolean isFirstHit, CCRenderState ccrs, MultiBufferSource getter, PoseStack matrixStack);

    @OnlyIn(Dist.CLIENT)
    void buildTooltip(Vector3 mousePosition, boolean isFirstHit, List<Component> tooltip);

    static TileCoord toNearestPosition(Vector3 v) {
        return new TileCoord((int) Math.floor(v.x), (int) Math.floor(v.y), (int) Math.floor(v.z));
    }

    static boolean isSamePosition(Vector3 a, Vector3 b) {
        return Math.floor(a.x) == Math.floor(b.x) &&
                Math.floor(a.y) == Math.floor(b.y) &&
                Math.floor(a.z) == Math.floor(b.z);
    }

    static Cuboid6 toNearestCuboid(Vector3 v) {
        return new Cuboid6(v.copy().floor(), v.copy().ceil());
    }

    static Cuboid6 toSelectionCuboid(Vector3 a, Vector3 b) {
        return toNearestCuboid(a).enclose(toNearestCuboid(b));
    }

    /**
     * Converts Cuboid from internal tile space to world space
     *
     * @param pos Position of the tile
     * @param c Internal cuboid within bounds [0,0,0 -> 1, 1, 1]
     * @return Cuboid in world space
     */
    static Cuboid6 internalToGlobalCuboid(TileCoord pos, Cuboid6 c) {
        Cuboid6 box = c.copy();
        box.add(pos.x, pos.y, pos.z);
        return box;
    }
}
