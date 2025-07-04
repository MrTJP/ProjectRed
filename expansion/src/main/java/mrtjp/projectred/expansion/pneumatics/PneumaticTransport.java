package mrtjp.projectred.expansion.pneumatics;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.projectred.expansion.part.PneumaticTubePayload;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;

import javax.annotation.Nullable;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Predicate;

import static mrtjp.projectred.expansion.part.PneumaticTubePayload.MAX_PROGRESS;

public class PneumaticTransport {

    private final PneumaticTransportContainer container;

    private final HashMap<Integer, PneumaticTubePayload> payloads = new HashMap<>();

    private int nextId = 0;

    public PneumaticTransport(PneumaticTransportContainer container) {
        this.container = container;
    }

    //region save/load
    public void save(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        ListTag payloadList = new ListTag();
        for (var e : payloads.entrySet()) {
            CompoundTag payloadTag = new CompoundTag();
            payloadTag.putInt("id", e.getKey());
            e.getValue().save(payloadTag, lookupProvider);
            payloadList.add(payloadTag);
        }
        tag.put("payloads", payloadList);
        tag.putInt("nextId", nextId);
    }

    public void load(CompoundTag tag, HolderLookup.Provider lookupProvider) {
        payloads.clear();
        ListTag payloadList = tag.getList("payloads", ListTag.TAG_COMPOUND);
        for (int i = 0; i < payloadList.size(); i++) {
            CompoundTag payloadTag = payloadList.getCompound(i);
            int id = payloadTag.getInt("id");
            PneumaticTubePayload payload = new PneumaticTubePayload();
            payload.load(payloadTag, lookupProvider);
            payloads.put(id, payload);
        }
        nextId = tag.getInt("nextId");
    }

    public void writeDesc(MCDataOutput packet) {
        packet.writeVarInt(payloads.size());
        for (var e : payloads.entrySet()) {
            packet.writeVarInt(e.getKey());
            e.getValue().writeDesc(packet);
        }
    }

    public void readDesc(MCDataInput packet) {
        payloads.clear();
        int size = packet.readVarInt();
        for (int i = 0; i < size; i++) {
            int id = packet.readVarInt();
            PneumaticTubePayload payload = new PneumaticTubePayload();
            payload.readDesc(packet);
            payloads.put(id, payload);
        }
    }
    //endregion

    @Nullable
    public PneumaticTubePayload removePayload(int id) {
        return payloads.remove(id);
    }

    @Nullable
    public PneumaticTubePayload getPayload(int id) {
        return payloads.get(id);
    }

    public void addPayload(int id, PneumaticTubePayload payload) {
        assert !payloads.containsValue(payload);
        payloads.put(id, payload);
    }

    public int addPayload(PneumaticTubePayload payload) {
        assert !payloads.containsValue(payload);
        int id = getNextId();
        payloads.put(id, payload);
        return id;
    }

    public Collection<PneumaticTubePayload> getPayloads() {
        return payloads.values();
    }

    public Collection<Integer> removePayloadsConditionally(Predicate<PneumaticTubePayload> condition) {
        List<Integer> toRemove = new LinkedList<>();
        for (var e : payloads.entrySet()) {
            if (condition.test(e.getValue())) {
                toRemove.add(e.getKey());
            }
        }
        for (int id : toRemove) {
            payloads.remove(id);
        }
        return toRemove;
    }

    public void tick() {
        List<Integer> toRemove = new LinkedList<>();

        // Move all payloads forward
        for (var e : payloads.entrySet()) {
            var payload = e.getValue();

            int p1 = payload.getProgress();
            payload.incrementProgress();
            int p2 = payload.getProgress();

            // Calc output direction right before reaching middle
            int outputCalcBoundary = MAX_PROGRESS / 2 - payload.getSpeed();
            if (p1 < outputCalcBoundary && p2 >= outputCalcBoundary) {
                container.setOutputDirection(payload);
                container.onPayloadChanged(e.getKey(), payload);
            }

            // If max progress was crossed
            if (p1 < MAX_PROGRESS && p2 >= MAX_PROGRESS) {
                if (container.onPayloadReachedOutput(e.getKey(), payload)) {
                    toRemove.add(e.getKey());
                }
            }

            if (p2 >= MAX_PROGRESS * 2) {
                // Insurance in case item is not passed when crossing MAX_PROGRESS
                toRemove.add(e.getKey());
            }
        }

        // Remove all payloads that reached the end
        for (int id : toRemove) {
            payloads.remove(id);
        }
    }

    private int getNextId() {
        int id = nextId++;
        while (payloads.containsKey(id)) {
            id = nextId++;
        }
        return id;
    }
}
