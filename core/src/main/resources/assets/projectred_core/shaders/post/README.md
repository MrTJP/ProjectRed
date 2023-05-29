## PostChain definition format

PostChains are post-processing shader pipelines that can be defined in JSON

```json
{
    // Pipeline input FBOs. These will be drawn to either externally (typically, 
    // create a custom RenderType, with custom TargetShard that bindWrites one of these targets)
    // or internally by a pass.
    // 
    // These targets will be available below along with a 'minecraft:main' hard-coded
    // value, which represents the RenderTarget object given to PostChain constructor,
    // typically the main target
    //
    // Going forward, 'target' will refer to one of these FBOs
    "targets": [
        "target1", // Target with default w/h matching main target
        {
            "name": "target2", // Or dictionaries with specitic w/h
            "width": 40,  // Either missing width or height replaced with screen w/h
            "height": 60
        }
    ],

    // List of shaders passes to run, their input targets, and output targets
    "passes": [
        {
            "name": "modid:pass1shader", // Shader location (i.e. assets/<modid>/shaders/program/pass1shader.json)
            "intarget": "minecraft:main", // Input target (one of above, or minecraft:main)
            "outtarget": "target1",
            
            // Array of sampler2d inputs to pass shader. Can map any of the defined targets.
            "auxtargets": [
                {
                    "name": "Target1Sampler", // Sampler name (referenced by name inside 'pass1shader')
                    "id": "target1" // Target name
                },
                {
                    "name": "Target1DepthSampler",
                    "id": "target1:depth" // Special ':depth' suffix will use this target's depth buffer info only
                },
                {
                    "name": "TextureSampler",
                    "id": "example_texture", // If this target does not exist, it is treated as a texture resource location (i.e. assets/<modid>/textures/effects/<id>.png)
                    "width": 16, // w of texture
                    "height": 16, // h of texture
                    "bilinear": false // Bilinear sampling enable (NN otherwise)
                }
            ],
            // Array of uniforms to give to pass shader
            "uniforms": [
                {
                    "name": "Uniform1", // Uniform name from pass shader definition
                    "values": [ 0.1, 0.2, 0.3, 0.4 ] // Array of floats, with length 1 to 4
                }
            ]
        },
        {
            "name": "modid:pass2shader",
            "intarget": "target1",
            "outtarget": "target2"
        }
        {
            "name": "blit", // A useful built-in pass shader for simple blitting
            "intarget": "target2",
            "outtarget": "minecraft:main" // Final pass renders to main target
        }
    ]
}
```