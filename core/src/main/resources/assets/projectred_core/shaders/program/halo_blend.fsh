#version 150

uniform sampler2D DiffuseSampler;
uniform sampler2D HaloSampler;

in vec2 texCoord;

out vec4 fragColor;

// Blend function to overlay bloom layer
vec3 blend(vec3 dst, vec4 src) {
    return (dst * (1.0 - src.a)) + src.rgb;
}

void main() {

    vec4 background = texture(DiffuseSampler, texCoord);
    vec4 halo = texture(HaloSampler, texCoord);

    // Throw empty pixels away
    if (halo.a == 0) {
        fragColor = vec4(background.rgb, 1);
        return;
    }

    // Blend halo into background
    vec3 blended = blend(background.rgb, halo);

    // Output final colour as
    fragColor = vec4(blended, 1.0);
}
