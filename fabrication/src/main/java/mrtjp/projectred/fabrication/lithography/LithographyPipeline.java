package mrtjp.projectred.fabrication.lithography;

import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

public enum LithographyPipeline {
    BASIC(new ProcessNode[] { ProcessNode.PROCESS_64NM },
            new WaferType[] { WaferType.ROUGH_WAFER },
            "basic"),
    ADVANCED(new ProcessNode[] { ProcessNode.PROCESS_64NM, ProcessNode.PROCESS_32NM, ProcessNode.PROCESS_16NM, ProcessNode.PROCESS_8NM },
            new WaferType[] { WaferType.ROUGH_WAFER, WaferType.PURIFIED_WAFER, WaferType.POLISHED_WAFER },
            "advanced");

    private final Set<ProcessNode> compatibleProcesses;
    private final Set<WaferType> compatibleWafers;
    private final String unlocalizedName;

    LithographyPipeline(ProcessNode[] compatibleProcesses, WaferType[] compatibleWafers, String unlocalizedName) {
        this.compatibleProcesses = Arrays.stream(compatibleProcesses).collect(Collectors.toSet());
        this.compatibleWafers = Arrays.stream(compatibleWafers).collect(Collectors.toSet());
        this.unlocalizedName = unlocalizedName;
    }

    public boolean isProcessNodeValid(ProcessNode process) {
        return compatibleProcesses.contains(process);
    }

    public boolean isWaferTypeValid(WaferType wafer) {
        return compatibleWafers.contains(wafer);
    }

    public String getUnlocalizedName() {
        return unlocalizedName;
    }
}
