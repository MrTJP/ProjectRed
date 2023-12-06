package mrtjp.projectred.lib;

import codechicken.lib.vec.Vertex5;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.packs.resources.Resource;
import net.minecraft.server.packs.resources.ResourceManager;

import java.io.*;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

public class ModelLib {

    /**
     * Exports a map of verts to a json file (i.e. groups of a CCModel for example)
     * @param filePath The path to the destination file (directories must exist)
     * @param vertGroups The map of vert groups to export
     */
    public static void exportVertsToJson(String filePath, Map<String, Vertex5[]> vertGroups) {
        try(PrintWriter writer = new PrintWriter(filePath)) {
            JsonWriter jsonWriter = new JsonWriter(writer);
            jsonWriter.setIndent("    ");

            jsonWriter.beginObject();
            for (var entry : vertGroups.entrySet()) {
                jsonWriter.name(entry.getKey());

                jsonWriter.beginArray();
                for (int i = 0; i < entry.getValue().length; i++) {
                    Vertex5 vertex = entry.getValue()[i];
                    jsonWriter.beginObject();
                    jsonWriter.name("x").value(vertex.vec.x);
                    jsonWriter.name("y").value(vertex.vec.y);
                    jsonWriter.name("z").value(vertex.vec.z);
                    jsonWriter.name("u").value(vertex.uv.u);
                    jsonWriter.name("v").value(vertex.uv.v);
                    jsonWriter.endObject();
                }
                jsonWriter.endArray();
            }

            jsonWriter.endObject();
            jsonWriter.flush();

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static Map<String, Vertex5[]> importVertsFromJson(ResourceManager resourceManager, ResourceLocation resource) {
        try (
                Resource res = resourceManager.getResource(resource);
                InputStream stream = res.getInputStream()) {

            return importVertsFromJson(new InputStreamReader(stream));

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static Map<String, Vertex5[]> importVertsFromJson(String filePath) {
        try (FileReader reader = new FileReader(filePath)) {
            return importVertsFromJson(reader);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

        /**
         * Imports verts from a json file. (See {@link #exportVertsToJson(String, Map)} for the format)
         */
    public static Map<String, Vertex5[]> importVertsFromJson(Reader reader) {

        try {
            Map<String, Vertex5[]> vertMap = new HashMap<>();
            JsonReader jsonReader = new JsonReader(reader);

            jsonReader.beginObject();
            while (jsonReader.hasNext()) {

                String name = jsonReader.nextName();
                LinkedList<Vertex5> verts = new LinkedList<>();

                jsonReader.beginArray();
                while (jsonReader.hasNext()) {
                    Vertex5 vertex = new Vertex5();

                    jsonReader.beginObject();
                    while (jsonReader.hasNext()) {
                        String key = jsonReader.nextName();
                        switch (key) {
                            case "x" -> vertex.vec.x = jsonReader.nextDouble();
                            case "y" -> vertex.vec.y = jsonReader.nextDouble();
                            case "z" -> vertex.vec.z = jsonReader.nextDouble();
                            case "u" -> vertex.uv.u = jsonReader.nextDouble();
                            case "v" -> vertex.uv.v = jsonReader.nextDouble();
                            default -> throw new RuntimeException("Invalid key: " + key);
                        }
                    }
                    jsonReader.endObject();

                    verts.add(vertex);
                }
                jsonReader.endArray();

                vertMap.put(name, verts.toArray(new Vertex5[0]));
            }
            jsonReader.endObject();

            return vertMap;

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
