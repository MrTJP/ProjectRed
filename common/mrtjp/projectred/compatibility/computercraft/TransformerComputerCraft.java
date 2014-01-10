package mrtjp.projectred.compatibility.computercraft;

import codechicken.lib.asm.*;
import codechicken.lib.asm.InstructionComparator.InsnListSection;
import com.google.common.collect.ImmutableMap;
import net.minecraft.launchwrapper.IClassTransformer;
import net.minecraft.launchwrapper.LaunchClassLoader;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.commons.RemappingClassAdapter;
import org.objectweb.asm.commons.SimpleRemapper;
import org.objectweb.asm.tree.*;

import java.io.IOException;
import java.util.Map;
import java.util.TreeMap;

public class TransformerComputerCraft implements IClassTransformer, Opcodes
{
    private static LaunchClassLoader cl = (LaunchClassLoader)ClassHeirachyManager.class.getClassLoader();
    private static byte[] getBytes(String name) {
        try {
            return cl.getClassBytes(name.replace('/', '.'));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private interface IClassPatcher
    {
        public byte[] apply(String name, byte[] bytes);
    }

    private class ClassReplacer implements IClassPatcher
    {
        private String replacement;
        public ClassReplacer(String replacement) {
            this.replacement = replacement;
        }

        @Override
        public byte[] apply(String name, byte[] bytes) {
            ClassReader reader = new ClassReader(getBytes(replacement));
            ClassWriter writer = new ClassWriter(0);
            RemappingClassAdapter remapAdapter = new RemappingClassAdapter(writer,
                    new SimpleRemapper(replacement.replace('.', '/'), name.replace('.', '/')));
            reader.accept(remapAdapter, ClassReader.EXPAND_FRAMES);
            return writer.toByteArray();
        }
    }

    private TreeMap<String, IClassPatcher> patches = new TreeMap<String, IClassPatcher>();
    private Map<String, ASMReader.ASMBlock> asmBlocks;

    private ASMReader.ASMBlock getASMBlock(String key) {
        if(asmBlocks == null)
            asmBlocks = ASMReader.loadResource("/assets/projectred/asm/computercraft.asm");

        return asmBlocks.get(key);
    }


    public TransformerComputerCraft() {
        patches.put("dan200.computer.shared.RedPowerTileEntityComputer", new ClassReplacer("mrtjp.projectred.compatibility.computercraft.ProjectRedTileEntityComputer"));
        patches.put("dan200.turtle.shared.RedPowerTileEntityTurtle", new ClassReplacer("mrtjp.projectred.compatibility.computercraft.ProjectRedTileEntityTurtle"));
        patches.put("dan200.computer.shared.RedPowerInterop", new IClassPatcher() {
            Map<String, String> remappings = new ImmutableMap.Builder<String, String>()
                    .put("com.eloraam.redpower.core.RedPowerLib", "mrtjp.projectred.ProjectRedCore")
                    .put("com.eloraam.redpower.RedPowerWorld", "mrtjp.projectred.ProjectRedExploration")
                    .put("itemPickaxeRuby", "itemRubyPickaxe")
                    .put("itemPickaxeGreenSapphire", "itemPeridotPickaxe")
                    .put("itemPickaxeSapphire", "itemSapphirePickaxe")
                    .put("itemShovelRuby", "itemRubyShovel")
                    .put("itemShovelGreenSapphire", "itemPeridotShovel")
                    .put("itemShovelSapphire", "itemSapphireShovel")
                    .put("itemAxeRuby", "itemRubyAxe")
                    .put("itemAxeGreenSapphire", "itemPeridotAxe")
                    .put("itemAxeSapphire", "itemSapphireAxe")
                    .put("itemSwordRuby", "itemRubySword")
                    .put("itemSwordGreenSapphire", "itemPeridotSword")
                    .put("itemSwordSapphire", "itemSapphireSword")
                    .put("itemHoeRuby", "itemRubyHoe")
                    .put("itemHoeGreenSapphire", "itemPeridotHoe")
                    .put("itemHoeSapphire", "itemSapphireHoe")
                    .build();

            @Override
            public byte[] apply(String name, byte[] bytes) {
                ClassNode cnode = ASMHelper.createClassNode(bytes);
                remapMethod(ASMHelper.findMethod(new ObfMapping("dan200/computer/shared/RedPowerInterop", "findRedPower", "()V"), cnode));
                remapMethod(ASMHelper.findMethod(new ObfMapping("dan200/computer/shared/RedPowerInterop", "findRedPowerWorld", "()V"), cnode));
                return ASMHelper.createBytes(cnode, 0);
            }

            private void remapMethod(MethodNode mnode) {
                for(AbstractInsnNode insn = mnode.instructions.getFirst(); insn != null; insn = insn.getNext())
                    if(insn instanceof LdcInsnNode) {
                        LdcInsnNode ldc = (LdcInsnNode)insn;
                        if(ldc.cst instanceof String && remappings.containsKey(ldc.cst))
                            ldc.cst = remappings.get(ldc.cst);
                    }
            }
        });
        patches.put("dan200.computer.shared.BlockComputerBase", new IClassPatcher() {
            @Override
            public byte[] apply(String name, byte[] bytes) {
                ClassNode cnode = ASMHelper.createClassNode(bytes);
                for(MethodNode mnode : cnode.methods)
                    if(mnode.name.equals("getBundledPowerOutput"))
                        mnode.instructions = getASMBlock("getBundledPowerOutput").insns;

                return ASMHelper.createBytes(cnode, 3);
            }
        });
        patches.put("dan200.computer.shared.NetworkedComputerHelper", new IClassPatcher() {
            @Override
            public byte[] apply(String name, byte[] bytes) {
                ClassNode cnode = ASMHelper.createClassNode(bytes);
                for(MethodNode mnode : cnode.methods)
                    if(mnode.name.equals("updateRedstone")) {
                        InsnList needle1 = new InsnList();
                        needle1.add(ObfMapping.fromDesc("dan200/computer/shared/MFRInterop.isMFRInstalled()Z").toInsn(INVOKESTATIC));
                        AbstractInsnNode rem_start = InstructionComparator.insnListFindStart(mnode.instructions, needle1).get(0);
                        ASMHelper.removeBlock(mnode.instructions, new InsnListSection(rem_start, rem_start.getNext()));

                        InsnList needle2 = new InsnList();
                        needle2.add(ObfMapping.fromDesc("dan200/computer/shared/BlockComputerBase.updateCable(Lnet/minecraft/world/World;III)V").toInsn(INVOKEVIRTUAL));

                        AbstractInsnNode marker = InstructionComparator.insnListFindStart(mnode.instructions, needle2).get(0);
                        mnode.instructions.insert(marker, getASMBlock("cableUpdate").insns);
                    }
                    else if(mnode.name.equals("update")) {
                        InsnList needle = new InsnList();
                        needle.add(ObfMapping.fromDesc("dan200/computer/shared/NetworkedComputerHelper.m_firstFrame:Z").toInsn(GETFIELD));

                        AbstractInsnNode marker = InstructionComparator.insnListFindStart(mnode.instructions, needle).get(0).getNext();
                        mnode.instructions.insert(marker, getASMBlock("firstFrame").insns);
                    }

                return ASMHelper.createBytes(cnode, 3);
            }
        });
    }

    @Override
    public byte[] transform(String name, String transformedName, byte[] bytes) {
        if(bytes == null) return null;
        IClassPatcher patch = patches.get(name);
        if(patch != null) {
            try {
                bytes = patch.apply(name, bytes);
            } catch (Exception e) {
                System.err.println("Failed to apply ProjectRed integration to ComputerCraft class: "+name+"\n please ensure you have ComputerCraft 1.58 installed");
                e.printStackTrace();
            }
        }

        return bytes;
    }
}
