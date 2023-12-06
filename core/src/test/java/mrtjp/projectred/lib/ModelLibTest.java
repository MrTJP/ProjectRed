package mrtjp.projectred.lib;

import codechicken.lib.vec.Vertex5;
import org.junit.jupiter.api.Test;

import javax.annotation.Nullable;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ModelLibTest {

    @Test
    public void testVertJsonExportImport() {

        Map<String, Vertex5[]> vertMap = new HashMap<>();

        vertMap.put("groupA", new Vertex5[] {
                new Vertex5(1.1, 0, 0, 0, 0),
                new Vertex5(0, 1.2, 0, 0, 0),
                new Vertex5(0, 0, 1.3, 0, 0),
                new Vertex5(0, 0, 0, 1.4, 0),
                new Vertex5(0, 0, 0, 0, 1.5),
        });

        vertMap.put("groupB", new Vertex5[] {
                new Vertex5(1.1, 0, 0, 0, 0),
                new Vertex5(0, 1.2, 0, 0, 0),
                new Vertex5(0, 0, 1.3, 0, 0),
                new Vertex5(0, 0, 0, 1.4, 0),
                new Vertex5(0, 0, 0, 0, 1.5),
        });

        // Export and import
        ModelLib.exportVertsToJson("test.json", vertMap);
        Map<String, Vertex5[]> importedVertMap = ModelLib.importVertsFromJson("test.json");

        // Compare
        assertEquals(vertMap.size(), importedVertMap.size());
        assertVertArrayEquals(vertMap.get("groupA"), importedVertMap.get("groupA"));
        assertVertArrayEquals(vertMap.get("groupB"), importedVertMap.get("groupB"));
    }

    // For some reason, Vertex5 does not provide equals implementation
    private static void assertVertArrayEquals(@Nullable Vertex5[] expected, @Nullable Vertex5[] actual) {
        if (expected == null || actual == null) {
            assertEquals(expected, actual);
            return;
        }

        assertEquals(expected.length, actual.length);
        for (int i = 0; i < expected.length; i++) {
            assertEquals(expected[i].vec, actual[i].vec);
            assertEquals(expected[i].uv, actual[i].uv);
        }
    }
}
