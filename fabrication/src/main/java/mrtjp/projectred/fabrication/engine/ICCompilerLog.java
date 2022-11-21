package mrtjp.projectred.fabrication.engine;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.fengine.TileCoord;
import mrtjp.fengine.api.ICStepThroughAssembler;
import mrtjp.projectred.fabrication.editor.ICEditorStateMachine;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;

import java.util.*;
import java.util.stream.Collectors;

import static mrtjp.projectred.fabrication.editor.ICEditorStateMachine.KEY_COMPILER_LOG_NODE_ADDED;
import static mrtjp.projectred.fabrication.editor.ICEditorStateMachine.KEY_COMPILER_LOG_NODE_EXECUTED;

public class ICCompilerLog implements ICStepThroughAssembler.EventReceiver {

    private final ICEditorStateMachine stateMachine;

    private final CompileTree compileTree = new CompileTree();

    private final List<Integer> currentPath = new LinkedList<>();
    private int completedSteps = 0; //TODO ideally this would be a feature in the compile tree object

    public ICCompilerLog(ICEditorStateMachine stateMachine) {
        this.stateMachine = stateMachine;
    }

    public void save(CompoundTag tag) {
        CompoundTag treeTag = new CompoundTag();
        compileTree.save(treeTag);
        tag.put("compile_tree", treeTag);
        tag.putInt("completed_steps", completedSteps);
        tag.putIntArray("current_path", currentPath);
    }

    public void load(CompoundTag tag) {
        compileTree.load(tag.getCompound("compile_tree"));
        completedSteps = tag.getInt("completed_steps");
        currentPath.clear();
        currentPath.addAll(Arrays.stream(tag.getIntArray("current_path")).boxed().collect(Collectors.toList()));
    }

    public void writeDesc(MCDataOutput out) {
        compileTree.writeDesc(out);
        out.writeInt(completedSteps);
        out.writeInt(currentPath.size());
        for (int i : currentPath) out.writeInt(i);
    }

    public void readDesc(MCDataInput in) {
        compileTree.readDesc(in);
        completedSteps = in.readInt();
        currentPath.clear();
        int size = in.readInt();
        for (int i = 0; i < size; i++) currentPath.add(in.readInt());
    }

    public void clear() {
        compileTree.clear();
        currentPath.clear();
        completedSteps = 0;
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

    //region Packet handling
    public void readLogStream(MCDataInput in, int key) {
        switch (key) {
            case KEY_COMPILER_LOG_NODE_ADDED:
                readNodeAdded(in);
                break;
            case KEY_COMPILER_LOG_NODE_EXECUTED:
                readNodeExecuted(in);
                break;
            default:
                throw new IllegalArgumentException("Unknown compiler stream key: " + key);
        }
    }
    //endregion

    //region Server-side utilities
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

        public ICStepThroughAssembler.AssemblerStepType step;
        public final List<TileCoord> tileCoords = new ArrayList<>();
        public final List<Integer> registerIds = new ArrayList<>();
        public final List<Integer> gateIds = new ArrayList<>();
        public final Map<Integer, Integer> registerRemaps = new HashMap<>();

        public void save(CompoundTag tag) {
            tag.putByte("step", (byte) step.ordinal());

            ListTag tileCoordsTag = new ListTag();
            for (TileCoord coord : tileCoords) {
                CompoundTag coordTag = new CompoundTag();
                coordTag.putInt("x", coord.x);
                coordTag.putInt("y", coord.y);
                coordTag.putInt("z", coord.z);
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

            ListTag tileCoordsTag = tag.getList("tileCoords", Tag.TAG_COMPOUND);
            for (Tag coordTag : tileCoordsTag) {
                CompoundTag coordTagCompound = (CompoundTag) coordTag;
                tileCoords.add(new TileCoord(
                        coordTagCompound.getInt("x"),
                        coordTagCompound.getInt("y"),
                        coordTagCompound.getInt("z")
                ));
            }

            registerIds.addAll(Arrays.stream(tag.getIntArray("registerIds")).boxed().collect(Collectors.toList()));
            gateIds.addAll(Arrays.stream(tag.getIntArray("gateIds")).boxed().collect(Collectors.toList()));

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
