package mrtjp.projectred.api;

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
    public static final String modIDCompatibility = "projectred-compatibility";
    public static final String modIDCore = "projectred-core";
    public static final String modIDExpansion = "projectred-expansion";
    public static final String modIDExploration = "projectred-exploration";
    public static final String modIDFabrication = "projectred-fabrication";
    public static final String modIDIllumination = "projectred-illumination";
    public static final String modIDIntegration = "projectred-integration";
    public static final String modIDRelocation = "projectred-relocation";
    public static final String modIDTransmission = "projectred-transmission";
    public static final String modIDTransportation = "projectred-transportation";

    /**
     * API used for Frame-based movement.
     */
    public static IRelocationAPI relocationAPI;

    /**
     * API used for interacting with wires.
     */
    public static ITransmissionAPI transmissionAPI;

    /**
     * API used for interacting with pipes
     */
    public static ITransportationAPI transportationAPI;
}