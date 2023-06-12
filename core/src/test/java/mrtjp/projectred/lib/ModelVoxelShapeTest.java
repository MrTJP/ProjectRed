package mrtjp.projectred.lib;

import codechicken.lib.math.MathHelper;
import codechicken.lib.vec.*;
import org.junit.jupiter.api.Test;

import java.util.LinkedList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class ModelVoxelShapeTest {

    @Test
    public void testTriRayHit() {
        for (TriTestCase testCase : createTestCases()) {

            Vector3 i1 = testCase.tri.getIntersection(testCase.above, testCase.slightlyAbove, false, false);
            Vector3 i2 = testCase.tri.getIntersection(testCase.above, testCase.below,         false, false);
            Vector3 i3 = testCase.tri.getIntersection(testCase.below, testCase.slightlyBelow, false, false);
            Vector3 i4 = testCase.tri.getIntersection(testCase.below, testCase.above,         false, false);

            assertEqualsT(testCase.on, i1);
            assertEqualsT(testCase.on, i2);
            assertEqualsT(testCase.on, i3);
            assertEqualsT(testCase.on, i4);
        }
    }

    @Test
    public void testTriRayHitWithCulling() {
        for (TriTestCase testCase : createTestCases()) {

            Vector3 i1 = testCase.tri.getIntersection(testCase.above, testCase.slightlyAbove, true, false);
            Vector3 i2 = testCase.tri.getIntersection(testCase.above, testCase.below,         true, false);
            Vector3 i3 = testCase.tri.getIntersection(testCase.below, testCase.slightlyBelow, true, false);
            Vector3 i4 = testCase.tri.getIntersection(testCase.below, testCase.above,         true, false);

            assertEqualsT(testCase.on, i1);
            assertEqualsT(testCase.on, i2);
            assertNull(i3);
            assertNull(i4);
        }
    }

    @Test
    public void testTriSegmentHit() {
        for (TriTestCase testCase : createTestCases()) {

            Vector3 i1 = testCase.tri.getIntersection(testCase.above, testCase.slightlyAbove, false, true);
            Vector3 i2 = testCase.tri.getIntersection(testCase.above, testCase.below,         false, true);
            Vector3 i3 = testCase.tri.getIntersection(testCase.below, testCase.slightlyBelow, false, true);
            Vector3 i4 = testCase.tri.getIntersection(testCase.below, testCase.above,         false, true);

            assertNull(i1);
            assertEqualsT(testCase.on, i2);
            assertNull(i3);
            assertEqualsT(testCase.on, i4);
        }
    }

    @Test
    public void testTriSegmentHitWithCulling() {
        for (TriTestCase testCase : createTestCases()) {

            Vector3 i1 = testCase.tri.getIntersection(testCase.above, testCase.slightlyAbove, true, true);
            Vector3 i2 = testCase.tri.getIntersection(testCase.above, testCase.below,         true, true);
            Vector3 i3 = testCase.tri.getIntersection(testCase.below, testCase.slightlyBelow, true, true);
            Vector3 i4 = testCase.tri.getIntersection(testCase.below, testCase.above,         true, true);

            assertNull(i1);
            assertEqualsT(testCase.on, i2);
            assertNull(i3);
            assertNull(i4);
        }
    }

    // Equal with some floating point tolerance
    public static void assertEqualsT(Vector3 a, Vector3 b) {
        assertTrue(a.equalsT(b));
    }

    public List<TriTestCase> createTestCases() {
        List<TriTestCase> testCases = new LinkedList<>();

        // Simple half-quad on the XZ plane with normal facing up
        Vector3 v0 = new Vector3(0, 0, 0);
        Vector3 v1 = new Vector3(0, 0, 1);
        Vector3 v2 = new Vector3(1, 0, 0);

        double xyo = 0.2; // makes the ray not perfectly vertical
        Vector3 above           = new Vector3(0.25 + xyo,    0.10, 0.25 + xyo);
        Vector3 slightlyAbove   = new Vector3(0.25 + xyo/2,  0.05, 0.25 + xyo/2);
        Vector3 on              = new Vector3(0.25,          0.00, 0.25);
        Vector3 slightlyBelow   = new Vector3(0.25 - xyo/2, -0.05, 0.25 - xyo/2);
        Vector3 below           = new Vector3(0.25 - xyo,   -0.10, 0.25 - xyo);

        int rsteps = 16;
        int tsteps = 5;
        double rotationStep = 360.0 / rsteps * MathHelper.torad;
        double translationStep = 2.0 / tsteps;
        for (int i = 0; i < rsteps; i++) for (int j = 0; j < rsteps; j++) for (int k = 0; k < rsteps; k++) {
            for (int x = -tsteps; x < tsteps; x++) for (int y = -tsteps; y < tsteps; y++) for (int z = -tsteps; z < tsteps; z++) {

                // Create xyz transformation
                Transformation t = new TransformationList();
                t.with(new Rotation(i * rotationStep, Vector3.X_POS).at(Vector3.CENTER));
                t.with(new Rotation(j * rotationStep, Vector3.Y_POS).at(Vector3.CENTER));
                t.with(new Rotation(k * rotationStep, Vector3.Z_POS).at(Vector3.CENTER));
                t.with(new Translation(translationStep * x, translationStep * y, translationStep * z));

                // Create test case with transformed vectors
                TriTestCase testCase = new TriTestCase(
                        new ModelVoxelShape.Tri(
                                v0.copy().apply(t),
                                v1.copy().apply(t),
                                v2.copy().apply(t)),
                        above.copy().apply(t),
                        slightlyAbove.copy().apply(t),
                        below.copy().apply(t),
                        slightlyBelow.copy().apply(t),
                        on.copy().apply(t));

                testCases.add(testCase);
            }
        }

        return testCases;
    }

    @Test
    public void testCaseA() {

        ModelVoxelShape.Tri tri1 = new ModelVoxelShape.Tri(
                new Vector3(0.8753, 0.1247, 1.0000),
                new Vector3(0.1247, 0.1247, 0.8753),
                new Vector3(0.1247, 0.1247, 1.0000));

        Vector3 start = new Vector3(0.7000, 2.0390, 1.3000);
        Vector3 end = new Vector3(0.07573, -2.855, 0.4915);

        Vector3 i1 = tri1.getIntersection(start, end, false, false);
        Vector3 i2 = tri1.getIntersection(start, end, true, false);

        assertNotNull(i1);
        assertNotNull(i2);
        assertEqualsT(i1, i2);
    }

    private record TriTestCase(ModelVoxelShape.Tri tri, Vector3 above, Vector3 slightlyAbove, Vector3 below, Vector3 slightlyBelow, Vector3 on) { }
}
