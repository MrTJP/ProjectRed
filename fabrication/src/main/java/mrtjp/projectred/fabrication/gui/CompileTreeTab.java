package mrtjp.projectred.fabrication.gui;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.fengine.TileCoord;
import mrtjp.fengine.api.ICStepThroughAssembler;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.engine.log.ICCompilerLog.CompileTreeNode;
import mrtjp.projectred.fabrication.gui.screen.ICWorkbenchCompileTab;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.lib.Rect;
import mrtjp.projectred.redui.AbstractGuiNode;
import mrtjp.projectred.redui.RedUISprite;
import mrtjp.projectred.redui.ScrollBarNode;
import mrtjp.projectred.redui.SpriteButtonNode;
import net.covers1624.quack.collection.FastStream;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.Style;

import javax.annotation.Nullable;
import java.util.*;

import static mrtjp.fengine.api.ICStepThroughAssembler.AssemblerStepType.*;
import static mrtjp.projectred.fabrication.editor.ICWorkbenchEditor.UNIFORM_DARK_GRAY;
import static mrtjp.projectred.fabrication.editor.ICWorkbenchEditor.UNIFORM_GRAY;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.*;

public class CompileTreeTab extends AbstractGuiNode implements ICompileOverlayRenderer {

    public static final Map<ICStepThroughAssembler.AssemblerStepType, String> STEP_TYPE_NAMES;
    public static final Map<ICStepThroughAssembler.AssemblerStepType, String> STEP_TYPE_DESC;
    static {
        Map<ICStepThroughAssembler.AssemblerStepType, String> map = new EnumMap<>(ICStepThroughAssembler.AssemblerStepType.class);
        map.put(CHECK_OPEN_TILE_MAPS,       UL_COMPILE_CHECK_TILE_MAP);
        map.put(CHECK_OPEN_FLAT_MAPS,       UL_COMPILE_CHECK_FLAT_MAP);
        map.put(MERGE_TILE_MAP,             UL_COMPILE_MERGE_TILE_MAP);
        map.put(MERGE_FLAT_MAP,             UL_COMPILE_MERGE_FLAT_MAP);
        map.put(MERGE_TILE_MAP_PRE,         UL_COMPILE_MERGE_TILE_MAP_PRE);
        map.put(MERGE_TILE_MAP_PHASE1,      UL_COMPILE_PHASE_1);
        map.put(PHASE1_ALLOC,               UL_COMPILE_ALLOC);
        map.put(MERGE_TILE_MAP_PHASE2,      UL_COMPILE_PHASE_2);
        map.put(PHASE2_PATHFIND,            UL_COMPILE_PATHFIND);
        map.put(MERGE_TILE_MAP_PHASE3,      UL_COMPILE_PHASE_3);
        map.put(PHASE3_PF_MANIFEST_SEARCH,  UL_COMPILE_PF_MANIFEST);
        map.put(MERGE_TILE_MAP_PHASE4,      UL_COMPILE_PHASE_4);
        map.put(PHASE4_REGISTER_REMAPS,     UL_COMPILE_ADD_REMAPS);
        map.put(MERGE_TILE_MAP_PHASE5,      UL_COMPILE_PHASE_5);
        map.put(PHASE5_CONSUME_REMAPS,      UL_COMPILE_REMAP);
        map.put(MERGE_TILE_MAP_PHASE6,      UL_COMPILE_PHASE_6);
        map.put(PHASE6_COLLECT,             UL_COMPILE_COLLECT);
        map.put(MERGE_TILE_MAP_POST,        UL_COMPILE_MERGE_TILE_MAP_POST);
        STEP_TYPE_NAMES = Collections.unmodifiableMap(map);

        map = new EnumMap<>(ICStepThroughAssembler.AssemblerStepType.class);
        map.put(CHECK_OPEN_TILE_MAPS,       UL_COMPILE_CHECK_TILE_MAP_DESC);
        map.put(CHECK_OPEN_FLAT_MAPS,       UL_COMPILE_CHECK_FLAT_MAP_DESC);
        map.put(MERGE_TILE_MAP,             UL_COMPILE_MERGE_TILE_MAP_DESC);
        map.put(MERGE_FLAT_MAP,             UL_COMPILE_MERGE_FLAT_MAP_DESC);
        map.put(MERGE_TILE_MAP_PRE,         UL_COMPILE_MERGE_TILE_MAP_PRE_DESC);
        map.put(MERGE_TILE_MAP_PHASE1,      UL_COMPILE_PHASE_1_DESC);
        map.put(PHASE1_ALLOC,               UL_COMPILE_ALLOC_DESC);
        map.put(MERGE_TILE_MAP_PHASE2,      UL_COMPILE_PHASE_2_DESC);
        map.put(PHASE2_PATHFIND,            UL_COMPILE_PATHFIND_DESC);
        map.put(MERGE_TILE_MAP_PHASE3,      UL_COMPILE_PHASE_3_DESC);
        map.put(PHASE3_PF_MANIFEST_SEARCH,  UL_COMPILE_PF_MANIFEST_DESC);
        map.put(MERGE_TILE_MAP_PHASE4,      UL_COMPILE_PHASE_4_DESC);
        map.put(PHASE4_REGISTER_REMAPS,     UL_COMPILE_ADD_REMAPS_DESC);
        map.put(MERGE_TILE_MAP_PHASE5,      UL_COMPILE_PHASE_5_DESC);
        map.put(PHASE5_CONSUME_REMAPS,      UL_COMPILE_REMAP_DESC);
        map.put(MERGE_TILE_MAP_PHASE6,      UL_COMPILE_PHASE_6_DESC);
        map.put(PHASE6_COLLECT,             UL_COMPILE_COLLECT_DESC);
        map.put(MERGE_TILE_MAP_POST,        UL_COMPILE_MERGE_TILE_MAP_POST_DESC);
        STEP_TYPE_DESC = Collections.unmodifiableMap(map);
    }

    private static final RedUISprite STEP_OUT_SPRITE = new RedUISprite(ICWorkbenchCompileTab.TAB_BACKGROUND, 335, 41, 14, 14, 512, 512);
    private static final RedUISprite STEP_IN_SPRITE = new RedUISprite(ICWorkbenchCompileTab.TAB_BACKGROUND, 320, 41, 14, 14, 512, 512);

    private final ICWorkbenchEditor editor;

    private final CTNListNode ctnListNode = new CTNListNode();

    private final Stack<CompileTreeNode> backStack = new Stack<>();
    private @Nullable CompileTreeNode currentNode = null;

    public CompileTreeTab(ICWorkbenchEditor editor) {
        this.editor = editor;
        setSize(91, 134);
        initSubNodes();
    }

    private void initSubNodes() {

        // Stack
        ctnListNode.setPosition(6, 31);
        ctnListNode.setSize(79, 95);
        addChild(ctnListNode);

        // Scrollbar
        ScrollBar scrollBar = new ScrollBar();
        scrollBar.setPosition(77, 31);
        scrollBar.setZPosition(0.2);
        scrollBar.setSize(8, 95);
        scrollBar.setSliderSize(8, 16);
        addChild(scrollBar);

        // Step out button
        SpriteButtonNode stepOut = new SpriteButtonNode(STEP_OUT_SPRITE);
        stepOut.setClickReceiver(this::stepOut);
        stepOut.setIsDisabledProvider(() -> currentNode == null);
        stepOut.setPosition(5, 5);
        stepOut.setSize(16, 16);
        addChild(stepOut);

        // Step in button
        SpriteButtonNode stepIn = new SpriteButtonNode(STEP_IN_SPRITE);
        stepIn.setClickReceiver(this::stepIn);
        stepIn.setIsDisabledProvider(() -> {
            var n = ctnListNode.getSelectedNode();
            return n == null || n.children.isEmpty();
        });
        stepIn.setPosition(23, 5);
        stepIn.setSize(16, 16);
        addChild(stepIn);
    }

    @Override
    public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {
        graphics.blit(ICWorkbenchCompileTab.TAB_BACKGROUND, getFrame().x(), getFrame().y(), 92, 223, getFrame().width(), getFrame().height(), 512, 512);

        // Title of current node
        if (currentNode != null) {
            var fr = getRoot().getFontRenderer();
            Component c = getTitleForCTNNode(currentNode).copy().withStyle(UNIFORM_DARK_GRAY);
            graphics.drawString(fr, c, getFrame().x() + 6, getFrame().y() + 21, 0xFFFFFF, false);
        }
    }

    //region Navigation control
    private void stepIn() {
        // Get the selected node that we will step into
        var selected = ctnListNode.getSelectedNode();
        if (selected == null) return;

        // Add current node to backstack and switch to new node
        if (currentNode != null) backStack.push(currentNode);
        currentNode = selected;

        // Update list
        refreshList();
    }

    private void stepOut() {
        // Set to null if no more previous nodes. This will force root nodes to be displayed
        currentNode = backStack.isEmpty() ? null : backStack.pop();

        // Refresh list
        refreshList();
    }
    //endregion

    @Override
    public void onAddedToParent() {
        editor.getStateMachine().getCompilerLog().addTreeChangedListener(this::refreshList);
        refreshList();
    }

    private void refreshList() {
        var log = editor.getStateMachine().getCompilerLog();
        if (log.getCurrentStack().isEmpty()) { //TODO separate receiver for this case?
            currentNode = null;
            backStack.clear();
        } else if (currentNode == null) {
            ctnListNode.setNodeList(editor.getStateMachine().getCompilerLog().getRootNodes());
        } else {
            ctnListNode.setNodeList(currentNode.children);
        }
    }

    //region ICompileTabOverlayRenderer
    @Override
    public void renderOverlay(ICRenderNode renderNode, Vector3 mousePosition, boolean isFirstHit, CCRenderState ccrs, MultiBufferSource getter, PoseStack matrixStack) {

        var node = ctnListNode.getSelectedNode();
        if (node == null) node = currentNode;

        if (node != null) {
            renderOverlayForNode(node, ccrs, getter, matrixStack);
        }
    }

    @Override
    public void buildTooltip(ICRenderNode renderNode, Vector3 mousePosition, List<Component> tooltip) {

    }

    private void renderOverlayForNode(CompileTreeNode node, CCRenderState ccrs, MultiBufferSource getter, PoseStack matrixStack) {
        ccrs.reset();
        ccrs.bind(ICRenderTypes.selectionRenderType, getter, matrixStack);

        // Render subnode positions in fainter color
        ccrs.baseColour = EnumColour.WHITE.rgba(127);

        List<TileCoord> coords = new LinkedList<>();
        for (var c : node.children) {
            c.getPositionsInTree(coords);
        }

        for (var pos : coords) {
            Vector3 p = new Vector3(pos.x, pos.y, pos.z);
            ICRenderTypes.renderSelection(ccrs, p, p.copy().add(0.01), 3 / 16D, 2 / 16D);
        }

        // Render current node positions in full color
        ccrs.baseColour = EnumColour.WHITE.rgba();
        for (var pos : node.tileCoords) {
            Vector3 p = new Vector3(pos.x, pos.y, pos.z);
            ICRenderTypes.renderSelection(ccrs, p, p.copy().add(0.01), 3 / 16D, 2 / 16D);
        }
    }
    //endregion

    public static Component getTitleForCTNNode(CompileTreeNode node) {

        String pos = node.tileCoords.isEmpty() ? "" : "[%d, %d, %d]".formatted(node.tileCoords.get(0).x, node.tileCoords.get(0).y, node.tileCoords.get(0).z);
        return switch (node.step) {
            case PHASE1_ALLOC -> Component.translatable(UL_COMPILE_ALLOC).append(" " + pos);
            case PHASE2_PATHFIND -> Component.translatable(UL_COMPILE_PATHFIND).append(" " + pos);
            case PHASE3_PF_MANIFEST_SEARCH -> Component.translatable(UL_COMPILE_PF_MANIFEST).append(" " + pos);
            case PHASE4_REGISTER_REMAPS -> Component.translatable(UL_COMPILE_ADD_REMAPS).append(" " + pos);
            case PHASE5_CONSUME_REMAPS -> Component.translatable(UL_COMPILE_REMAP).append(" " + pos);
            case PHASE6_COLLECT -> Component.translatable(UL_COMPILE_COLLECT).append(" " + pos);

            default -> Component.translatable(STEP_TYPE_NAMES.get(node.step));
        };
    }

    public static void buildTooltipForCTNNode(CompileTreeNode node, List<Component> toolTip) {
        toolTip.add(getTitleForCTNNode(node));
        toolTip.add(Component.translatable(STEP_TYPE_DESC.get(node.step)).withStyle(UNIFORM_GRAY));

        if (!node.registerIds.isEmpty()) {
            toolTip.add(Component.translatable(UL_UNIT_ONLY_REGISTERS).append(":").withStyle(UNIFORM_GRAY));
            addIntegerList(toolTip, node.registerIds, "R", 2, 4, UNIFORM_GRAY);
        }

        if (!node.gateIds.isEmpty()) {
            toolTip.add(Component.translatable(UL_UNIT_ONLY_GATES).append(":").withStyle(UNIFORM_GRAY));
            addIntegerList(toolTip, node.gateIds, "G", 2, 4, UNIFORM_GRAY);
        }

        if (!node.registerRemaps.isEmpty()) {
            toolTip.add(Component.translatable(UL_UNIT_ONLY_REMAPS).append(":").withStyle(UNIFORM_GRAY));
            addIntegerMap(toolTip, node.registerRemaps, "R", 2, 4, UNIFORM_GRAY);
        }

        if (!node.children.isEmpty()) {
            int r = node.countRegIdsInSubtree();
            int g = node.countGateIdsInSubtree();
            int m = node.countRemapsInSubtree();

            if (r > 0) {
                toolTip.add(Component.literal("  " + r + " ").append(Component.translatable(UL_UNIT_ONLY_REGISTERS)).withStyle(UNIFORM_GRAY));
            }
            if (g > 0) {
                toolTip.add(Component.literal("  " + g + " ").append(Component.translatable(UL_UNIT_ONLY_GATES)).withStyle(UNIFORM_GRAY));
            }
            if (m > 0) {
                toolTip.add(Component.literal("  " + m + " ").append(Component.translatable(UL_UNIT_ONLY_REMAPS)).withStyle(UNIFORM_GRAY));
            }
        }
    }

    private static void addIntegerMap(List<Component> toolTip, Map<Integer, Integer> regMap, String prefix, int indent, int limitPerLine, Style style) {
        var list = FastStream.of(regMap.entrySet()).map(e -> prefix + e.getKey() + " -> " + prefix + e.getValue()).toList();
        addStringList(toolTip, list, indent, limitPerLine, style);
    }

    private static void addIntegerList(List<Component> toolTip, List<Integer> regList, String prefix, int indent, int limitPerLine, Style style) {
        var list = FastStream.of(regList).map(i -> prefix + i).toList();
        addStringList(toolTip, list, indent, limitPerLine, style);
    }

    private static void addStringList(List<Component> toolTip, List<String> strList, int indent, int limitPerLine, Style style) {

        Component indentStr = Component.literal(" ".repeat(indent));

        if (strList.isEmpty()) {
            toolTip.add(indentStr.copy().append(Component.translatable(UL_UNIT_ONLY_NONE)).withStyle(UNIFORM_GRAY));
            return;
        }

        StringBuilder s = new StringBuilder();
        int i = 0;
        for (var str : strList) {
            s.append(str);

            // Add comma if not last element
            if (i < strList.size() - 1) {
                s.append(", ");
            }

            i++;

            // If last string or line is ended
            if (i == strList.size() || (i > 0 && i % limitPerLine == 0)) {
                toolTip.add(indentStr.copy().append(s.toString()).withStyle(style));
                s = new StringBuilder();
            }
        }
    }

    private class ScrollBar extends ScrollBarNode {

        public ScrollBar() {
            super(ScrollAxis.VERTICAL);
        }

        @Override
        protected void drawSlider(GuiGraphics graphics, Rect sliderFrame) {
            graphics.blit(ICWorkbenchCompileTab.TAB_BACKGROUND, sliderFrame.x(), sliderFrame.y(), 305, 58, sliderFrame.width(), sliderFrame.height(), 512, 512);
        }

        @Override
        protected void adjustContent(double scrollPercentage) {
            ctnListNode.setScrollPercentage(scrollPercentage);
        }
    }
}
