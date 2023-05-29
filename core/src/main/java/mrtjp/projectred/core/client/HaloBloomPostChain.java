package mrtjp.projectred.core.client;

import com.mojang.blaze3d.pipeline.RenderTarget;

import java.io.IOException;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;

public class HaloBloomPostChain extends FixedScalePostChain {

    public HaloBloomPostChain() throws IOException {
        super(MOD_ID, "halo_bloom");

        // Post chain JSON description requires absolute target sizes. We want to use
        // sizes relative to the main target. Scale is manually set here. Superclass
        // implementation will take care of re-scaling on resize.
        addFixedTargetScale("ds1", 0.5);    // 1/2
        addFixedTargetScale("ds2", 0.25);   // 1/4
        addFixedTargetScale("ds3", 0.125);  // 1/8
        addFixedTargetScale("ds4", 0.0625); // 1/16
    }

    public RenderTarget getInputTarget() {
        return getTempTarget("bloom_in");
    }
}
