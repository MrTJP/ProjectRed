package mrtjp.projectred.api;

import javax.annotation.Nullable;

/**
 * Central API class for ProjectRed. APIs are split up by module. If a specific
 * module is installed, the appropriate field will contain an implementor of that
 * the module's API interface. <br>
 * <br>
 * It is recommended that mods access this class within a soft dependency class.
 */
public final class ProjectRedAPI
{
    /**
     * The mod IDs used for all modules of ProjectRed.
     */
    public static final String CORE_MOD_ID          = "projectred_core";
    public static final String EXPANSION_MOD_ID     = "projectred_expansion";
    public static final String EXPLORATION_MOD_ID   = "projectred_exploration";
    public static final String FABRICATION_MOD_ID   = "projectred_fabrication";
    public static final String ILLUMINATION_MOD_ID  = "projectred_illumination";
    public static final String INTEGRATION_MOD_ID   = "projectred_integration";
    public static final String TRANSMISSION_MOD_ID  = "projectred_transmission";

    /**
     * API used for Frame-based movement.
     */
    @Nullable
    public static IExpansionAPI expansionAPI;

    /**
     * API used for interacting with wires.
     */
    @Nullable
    public static ITransmissionAPI transmissionAPI;
}