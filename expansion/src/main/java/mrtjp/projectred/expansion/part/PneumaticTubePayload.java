package mrtjp.projectred.expansion.part;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.item.ItemStack;

public class PneumaticTubePayload {

    public static final int MAX_PROGRESS = 255;

    private int progress;
    private int speed;

    private int inputSide = -1;
    private int outputSide = -1;

    private ItemStack itemStack;

    public PneumaticTubePayload(ItemStack itemStack) {
        this.itemStack = itemStack;
    }

    public PneumaticTubePayload() {
        this(ItemStack.EMPTY);
    }

    public int getProgress() {
        return progress;
    }

    public int getSpeed() {
        return speed;
    }

    public void setProgress(int prog) {
        progress = prog;
    }

    public void setSpeed(int spd) {
        speed = spd;
    }

    public void incrementProgress() {
        progress += speed;
    }

    public void setInputSide(int s) {
        inputSide = s;
    }

    public void setOutputSide(int s) {
        outputSide = s;
    }

    public int getInputSide() {
        return inputSide;
    }

    public int getOutputSide() {
        return outputSide;
    }

    public boolean hasOutputSide() {
        return outputSide != -1;
    }

    public void resetOutput() {
        outputSide = -1;
    }

    public void resetProgress() {
        progress = Math.max(0, progress - MAX_PROGRESS);
    }

    public boolean isPassedHalfWay() {
        return progress > MAX_PROGRESS / 2;
    }

    public int getCurrentSide() {
        return progress < MAX_PROGRESS / 2 ? inputSide : outputSide;
    }

    public ItemStack getItemStack() {
        return itemStack;
    }

    public void save(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        tag.putInt("progress", progress); //TODO size
        tag.putInt("speed", speed);
        tag.putInt("input_dir", inputSide);
        tag.putInt("output_dir", outputSide);
        tag.put("item_stack", itemStack.save(lookupProvider));
    }

    public void load(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        progress = tag.getInt("progress");
        speed = tag.getInt("speed");
        inputSide = tag.getInt("input_dir");
        outputSide = tag.getInt("output_dir");
        itemStack = ItemStack.parseOptional(lookupProvider, tag.getCompound("item_stack"));
    }

    public void writeDesc(MCDataOutput output) {
        output.writeByte(progress);
        output.writeByte(speed);
        output.writeByte(inputSide);
        output.writeByte(outputSide);
        output.writeItemStack(itemStack);
    }

    public void readDesc(MCDataInput input) {
        progress = input.readByte();
        speed = input.readByte();
        inputSide = input.readByte();
        outputSide = input.readByte();
        itemStack = input.readItemStack();
    }
}
