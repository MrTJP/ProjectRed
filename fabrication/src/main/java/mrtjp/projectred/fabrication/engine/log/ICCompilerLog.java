package mrtjp.projectred.fabrication.engine.log;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.fengine.TileCoord;
import mrtjp.fengine.api.ICStepThroughAssembler;
import mrtjp.projectred.fabrication.editor.ICEditorStateMachine;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;

import java.util.*;

import static mrtjp.fengine.api.ICStepThroughAssembler.AssemblerStepType.CHECK_OPEN_TILE_MAPS;
import static mrtjp.projectred.fabrication.editor.EditorDataUtils.*;
import static mrtjp.projectred.fabrication.editor.ICEditorStateMachine.*;

public class ICCompilerLog implements ICStepThroughAssembler.EventReceiver {

    private final ICEditorStateMachine stateMachine;

    // Compile steps tracking
    private final CompileTree compileTree = new CompileTree();
    private final List<Integer> currentPath = new LinkedList<>();
    private int completedSteps = 0; //TODO ideally this would be a feature in the compile tree object

    // Compile problems
    private final List<CompileProblem> problems = new ArrayList<>();
    private int warningCount = 0;
    private int errorCount = 0;

    public ICCompilerLog(ICEditorStateMachine stateMachine) {
        this.stateMachine = stateMachine;
    }

    public void save(CompoundTag tag) {
        tag.putInt(KEY_COMPLETED_STEPS, completedSteps);
        tag.putIntArray(KEY_CURRENT_PATH, currentPath);
        compileTree.save(tag);

        ListTag list = new ListTag();
        for (CompileProblem problem : problems) {
            CompoundTag problemTag = new CompoundTag();
            problemTag.putByte("_type", (byte) problem.type.getID());
            problem.save(problemTag);
            list.add(problemTag);
        }
        tag.put(KEY_PROBLEMS_LIST, list);
        tag.putInt(KEY_WARNING_COUNT, warningCount);
        tag.putInt(KEY_ERROR_COUNT, errorCount);
    }

    public void load(CompoundTag tag) {
        completedSteps = tag.getInt(KEY_COMPLETED_STEPS);
        currentPath.clear();
        currentPath.addAll(Arrays.stream(tag.getIntArray(KEY_CURRENT_PATH)).boxed().toList());
        compileTree.load(tag);

        ListTag list = tag.getList(KEY_PROBLEMS_LIST, 10);
        for (int i = 0; i < list.size(); i++) {
            CompoundTag problemTag = list.getCompound(i);
            CompileProblem problem = CompileProblemType.createById(problemTag.getByte("_type") & 0xFF);
            problem.load(problemTag);
            addProblemInternal(problem);
        }
    }

    public void writeDesc(MCDataOutput out) {
        compileTree.writeDesc(out);
        out.writeInt(completedSteps);
        out.writeInt(currentPath.size());
        for (int i : currentPath) out.writeInt(i);

        out.writeVarInt(problems.size());
        for (CompileProblem problem : problems) {
            out.writeByte(problem.type.getID());
            problem.writeDesc(out);
        }
    }

    public void readDesc(MCDataInput in) {
        clear();
        compileTree.readDesc(in);
        completedSteps = in.readInt();
        int size = in.readInt();
        for (int i = 0; i < size; i++) currentPath.add(in.readInt());

        size = in.readVarInt();
        for (int i = 0; i < size; i++) {
            CompileProblem problem = CompileProblemType.createById(in.readUByte());
            problem.readDesc(in);
            addProblemInternal(problem);
        }
    }

    public void clear() {
        compileTree.clear();
        currentPath.clear();
        completedSteps = 0;
        problems.clear();
        warningCount = 0;
        errorCount = 0;
    }

    public int getTotalSteps() {
        return compileTree.size();
    }

    public int getCompletedSteps() {
        return completedSteps;
    }

    public List<CompileProblem> getProblems() {
        return problems;
    }

    public int getErrorCount() {
        return errorCount;
    }

    public int getWarningCount() {
        return warningCount;
    }

    //region ICStepThroughAssembler.EventReceiver
    @Override
    public void onStepAdded(ICStepThroughAssembler.AssemblerStepDescriptor descriptor) {
        CompileTreeNode node = compileTree.findOrCreateNode(descriptor.getTreePath());
        node.step = descriptor.getStepType();

        sendNodeAdded(node, descriptor.getTreePath());
    }

    @Override
    public void onStepExecuted(ICStepThroughAssembler.AssemblerStepResult result) {
        CompileTreeNode node = compileTree.findOrCreateNode(result.getTreePath());
        node.tileCoords.addAll(result.getTileCoords());
        node.registerIds.addAll(result.getRegisterIds());
        node.gateIds.addAll(result.getGateIds());
        node.registerRemaps.putAll(result.getRemappedRegisterIds());

        currentPath.clear();
        currentPath.addAll(result.getTreePath());
        completedSteps++;

        sendNodeExecuted(node, result.getTreePath());
    }
    //endregion

    //region Compile-time logging
    public void clearAndSend() {
        clear();
        sendClear();
    }

    public void addProblem(CompileProblem problem) {
        addProblemInternal(problem);
        sendProblemAdded(problem);
    }
    //endregion

    //region Packet handling
    public void readLogStream(MCDataInput in, int key) {
        switch (key) {
            case KEY_COMPILER_LOG_CLEARED:
                clear();
                break;
            case KEY_COMPILER_LOG_NODE_ADDED:
                readNodeAdded(in);
                break;
            case KEY_COMPILER_LOG_NODE_EXECUTED:
                readNodeExecuted(in);
                break;
            case KEY_COMPILER_LOG_PROBLEM_ADDED:
                readProblemAdded(in);
                break;
            default:
                throw new IllegalArgumentException("Unknown compiler stream key: " + key);
        }
    }
    //endregion

    //region Server-side utilities
    private void sendClear() {
        stateMachine.getStateMachineStream(KEY_COMPILER_LOG_CLEARED);
    }

    private void sendNodeAdded(CompileTreeNode node, List<Integer> treePath) {
        MCDataOutput out = stateMachine.getStateMachineStream(KEY_COMPILER_LOG_NODE_ADDED);
        out.writeByte(treePath.size());
        for (int i : treePath) {
            out.writeByte(i);
        }
        out.writeByte(node.step.ordinal());
    }

    private void sendNodeExecuted(CompileTreeNode node, List<Integer> treePath) {
        MCDataOutput out = stateMachine.getStateMachineStream(KEY_COMPILER_LOG_NODE_EXECUTED);
        out.writeByte(treePath.size());
        for (int i : treePath) {
            out.writeByte(i);
        }
        node.write(out);
    }

    private void sendProblemAdded(CompileProblem problem) {
        MCDataOutput out = stateMachine.getStateMachineStream(KEY_COMPILER_LOG_PROBLEM_ADDED);
        out.writeByte(problem.type.ordinal());
        problem.writeDesc(out);
    }

    private void addProblemInternal(CompileProblem problem) {
        problems.add(problem);
        switch (problem.severity) {
            case WARNING -> warningCount++;
            case ERROR -> errorCount++;
        }
    }
    //endregion

    //region Client-side utilities
    private void readNodeAdded(MCDataInput in) {
        int pathLength = in.readUByte();
        List<Integer> path = new ArrayList<>(pathLength);
        for (int i = 0; i < pathLength; i++) {
            path.add((int) in.readUByte());
        }
        CompileTreeNode node = compileTree.findOrCreateNode(path);
        node.step = ICStepThroughAssembler.AssemblerStepType.values()[in.readUByte()];
    }

    private void readNodeExecuted(MCDataInput in) {
        int pathLength = in.readUByte();
        List<Integer> path = new ArrayList<>(pathLength);
        for (int i = 0; i < pathLength; i++) {
            path.add((int) in.readUByte());
        }
        CompileTreeNode node = compileTree.findOrCreateNode(path);
        node.read(in);

        currentPath.clear();
        currentPath.addAll(path);
        completedSteps++;
    }

    private void readProblemAdded(MCDataInput in) {
        CompileProblem problem = CompileProblemType.createById(in.readUByte());
        problem.readDesc(in);
        addProblemInternal(problem);
    }

    public List<CompileTreeNode> getCurrentStack() {
        return compileTree.getStack(currentPath);
    }

    public int getProgressScaled(int scale) {
        int size = compileTree.size();
        return size == 0 ? 0 : completedSteps * scale / size;
    }
    //endregion

    public static class CompileTreeNode {

        public final List<CompileTreeNode> children = new ArrayList<>();

        public ICStepThroughAssembler.AssemblerStepType step = CHECK_OPEN_TILE_MAPS;
        public final List<TileCoord> tileCoords = new ArrayList<>();
        public final List<Integer> registerIds = new ArrayList<>();
        public final List<Integer> gateIds = new ArrayList<>();
        public final Map<Integer, Integer> registerRemaps = new HashMap<>();

        public void save(CompoundTag tag) {
            tag.putByte("step", (byte) step.ordinal());

            ListTag tileCoordsTag = new ListTag();
            for (TileCoord coord : tileCoords) {
                CompoundTag coordTag = new CompoundTag();
                coordTag.putByte("x", (byte) coord.x);
                coordTag.putByte("y", (byte) coord.y);
                coordTag.putByte("z", (byte) coord.z);
                tileCoordsTag.add(coordTag);
            }
            tag.put("tileCoords", tileCoordsTag);

            tag.putIntArray("registerIds", registerIds);
            tag.putIntArray("gateIds", gateIds);

            ListTag remapsTag = new ListTag();
            for (Map.Entry<Integer, Integer> entry : registerRemaps.entrySet()) {
                CompoundTag remapTag = new CompoundTag();
                remapTag.putInt("k", entry.getKey());
                remapTag.putInt("v", entry.getValue());
                remapsTag.add(remapTag);
            }
            tag.put("registerRemaps", remapsTag);

            ListTag childrenTag = new ListTag();
            for (CompileTreeNode child : children) {
                CompoundTag childTag = new CompoundTag();
                child.save(childTag);
                childrenTag.add(childTag);
            }
            tag.put("children", childrenTag);
        }

        public void load(CompoundTag tag) {
            step = ICStepThroughAssembler.AssemblerStepType.values()[tag.getByte("step")];

            // Note: this is only called on fresh instance. No need to clear lists

            ListTag tileCoordsTag = tag.getList("tileCoords", Tag.TAG_COMPOUND);
            for (Tag coordTag : tileCoordsTag) {
                CompoundTag coordTagCompound = (CompoundTag) coordTag;
                tileCoords.add(new TileCoord(
                        coordTagCompound.getByte("x"),
                        coordTagCompound.getByte("y"),
                        coordTagCompound.getByte("z")
                ));
            }

            registerIds.addAll(Arrays.stream(tag.getIntArray("registerIds")).boxed().toList());
            gateIds.addAll(Arrays.stream(tag.getIntArray("gateIds")).boxed().toList());

            ListTag remapsTag = tag.getList("registerRemaps", Tag.TAG_COMPOUND);
            for (Tag remapTag : remapsTag) {
                CompoundTag remapTagCompound = (CompoundTag) remapTag;
                registerRemaps.put(remapTagCompound.getInt("k"), remapTagCompound.getInt("v"));
            }

            children.clear();
            ListTag childrenTag = tag.getList("children", Tag.TAG_COMPOUND);
            for (Tag childTag : childrenTag) {
                CompileTreeNode child = new CompileTreeNode();
                child.load((CompoundTag) childTag);
                children.add(child);
            }
        }

        public void write(MCDataOutput out) {
            out.writeByte(step.ordinal());
            out.writeByte(tileCoords.size());
            for (TileCoord coord : tileCoords) {
                out.writeByte(coord.x).writeByte(coord.y).writeByte(coord.z);
            }
            out.writeByte(registerIds.size());
            for (int i : registerIds) {
                out.writeVarInt(i);
            }
            out.writeByte(gateIds.size());
            for (int i : gateIds) {
                out.writeVarInt(i);
            }
            out.writeByte(registerRemaps.size());
            registerRemaps.forEach((k, v) -> {
                out.writeVarInt(k);
                out.writeVarInt(v);
            });
        }

        public void read(MCDataInput in) {
            step = ICStepThroughAssembler.AssemblerStepType.values()[in.readUByte()];
            int size = in.readUByte();
            tileCoords.clear();
            for (int i = 0; i < size; i++) {
                tileCoords.add(new TileCoord(in.readByte(), in.readByte(), in.readByte()));
            }
            size = in.readUByte();
            registerIds.clear();
            for (int i = 0; i < size; i++) {
                registerIds.add(in.readVarInt());
            }
            size = in.readVarInt();
            gateIds.clear();
            for (int i = 0; i < size; i++) {
                gateIds.add(in.readVarInt());
            }
            size = in.readVarInt();
            registerRemaps.clear();
            for (int i = 0; i < size; i++) {
                registerRemaps.put(in.readVarInt(), in.readVarInt());
            }
        }

        protected void writeDesc(MCDataOutput out) {
            write(out);
            out.writeByte(children.size());
            for (CompileTreeNode child : children) {
                child.writeDesc(out);
            }
        }

        protected void readDesc(MCDataInput in) {
            read(in);
            children.clear();
            int size = in.readUByte();
            for (int i = 0; i < size; i++) {
                CompileTreeNode child = new CompileTreeNode();
                child.readDesc(in);
                children.add(child);
            }
        }
    }

    public static class CompileTree {

        private final List<CompileTreeNode> rootNodes = new ArrayList<>();

        private int size = 0;

        /**
         * Find the node at the given path, creating empty nodes when necessary
         */
        public CompileTreeNode findOrCreateNode(List<Integer> path) {
            Iterator<Integer> pathIterator = path.iterator();

            int ri = pathIterator.next();
            CompileTreeNode node = ri < rootNodes.size() ? rootNodes.get(ri) : null;
            if (node == null) {
                node = new CompileTreeNode();
                rootNodes.add(ri, node);
                size++;
            }

            while (pathIterator.hasNext()) {
                int i = pathIterator.next();
                CompileTreeNode next = i < node.children.size() ? node.children.get(i) : null;
                if (next == null) {
                    next = new CompileTreeNode();
                    node.children.add(i, next);
                    size++;
                }
                node = next;
            }
            return node;
        }

        public List<CompileTreeNode> getStack(List<Integer> path) {
            if (path.isEmpty()) return Collections.emptyList();

            List<CompileTreeNode> stack = new ArrayList<>(path.size());
            Iterator<Integer> pathIterator = path.iterator();

            int ri = pathIterator.next();
            CompileTreeNode node = ri < rootNodes.size() ? rootNodes.get(ri) : null;
            if (node == null) {
                return stack;
            }
            stack.add(node);

            while (pathIterator.hasNext()) {
                int i = pathIterator.next();
                CompileTreeNode next = i < node.children.size() ? node.children.get(i) : null;
                if (next == null) {
                    return stack;
                }
                stack.add(next);
                node = next;
            }
            return stack;
        }

        public int size() {
            return size;
        }

        public void clear() {
            rootNodes.clear();
            size = 0;
        }

        public void save(CompoundTag tag) {
            tag.putInt("size", size);
            ListTag rootNodesTag = new ListTag();
            for (CompileTreeNode rootNode : rootNodes) {
                CompoundTag rootNodeTag = new CompoundTag();
                rootNode.save(rootNodeTag);
                rootNodesTag.add(rootNodeTag);
            }
            tag.put("rootNodes", rootNodesTag);
        }

        public void load(CompoundTag tag) {
            size = tag.getInt("size");
            rootNodes.clear();
            ListTag rootNodesTag = tag.getList("rootNodes", Tag.TAG_COMPOUND);
            for (Tag rootNodeTag : rootNodesTag) {
                CompileTreeNode rootNode = new CompileTreeNode();
                rootNode.load((CompoundTag) rootNodeTag);
                rootNodes.add(rootNode);
            }
        }

        public void writeDesc(MCDataOutput out) {
            out.writeVarInt(size);
            out.writeByte(rootNodes.size());
            for (CompileTreeNode node : rootNodes) {
                node.writeDesc(out);
            }
        }

        public void readDesc(MCDataInput in) {
            size = in.readVarInt();
            int size = in.readUByte();
            rootNodes.clear();
            for (int i = 0; i < size; i++) {
                CompileTreeNode node = new CompileTreeNode();
                node.readDesc(in);
                rootNodes.add(node);
            }
        }
    }
}
