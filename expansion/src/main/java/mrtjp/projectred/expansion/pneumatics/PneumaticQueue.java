package mrtjp.projectred.expansion.pneumatics;

import mrtjp.projectred.expansion.part.PneumaticTubePayload;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;

import javax.annotation.Nullable;
import java.util.Deque;
import java.util.LinkedList;

public class PneumaticQueue {

    private final Deque<PneumaticTubePayload> queue = new LinkedList<>();

    private boolean backStuffed = false;

    public boolean isEmpty() {
        return queue.isEmpty();
    }

    public void add(PneumaticTubePayload payload) {
        queue.addLast(payload);
    }

    public void addBackStuffed(PneumaticTubePayload payload) {
        queue.addFirst(payload);
        backStuffed = true;
    }

    public boolean isBackStuffed() {
        return backStuffed;
    }

    @Nullable
    public PneumaticTubePayload poll() {
        var out = queue.pollFirst();
        if (queue.isEmpty()) {
            backStuffed = false;
        }
        return out;
    }

    //region Save/Load
    public void save(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        var payloads = new ListTag();
        // save to list
        for (var payload : queue) {
            var payloadTag = new CompoundTag();
            payload.save(payloadTag, lookupProvider);
            payloads.add(payloadTag);
        }
        tag.put("payloads", payloads);
        tag.putBoolean("backstuffed", backStuffed);
    }

    public void load(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        queue.clear();
        // Load from list
        var payloads = tag.getList("payloads", Tag.TAG_COMPOUND);
        for (int i = 0; i < payloads.size(); i++) {
            var payloadTag = payloads.getCompound(i);
            var payload = new PneumaticTubePayload();
            payload.load(payloadTag, lookupProvider);
            queue.addLast(payload);
        }
        backStuffed = tag.getBoolean("backstuffed");
    }
    //endregion
}
