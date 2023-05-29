// Based on article by Alexander Christensen:
// https://learnopengl.com/Guest-Articles/2022/Phys.-Based-Bloom

#version 150

// This shader performs upsampling on a texture,
// as taken from Call Of Duty method, presented at ACM Siggraph 2014.

// Remember to add bilinear minification filter for this texture!
// Remember to use a floating-point texture format (for HDR)!
// Remember to use edge clamping for this texture!
uniform sampler2D DiffuseSampler;
uniform float FilterRadius;

in vec2 texCoord;
in vec2 oneTexel;

out vec4 upsample;

void main()
{
    // The filter kernel is applied with a radius, specified in texture
    // coordinates, so that the radius will vary across mip resolutions.
    float x = oneTexel.x * FilterRadius;
    float y = oneTexel.y * FilterRadius;

    // Take 9 samples around current texel:
    // a - b - c
    // d - e - f
    // g - h - i
    // === ('e' is the current texel) ===
    vec4 a = texture(DiffuseSampler, vec2(texCoord.x - x, texCoord.y + y));
    vec4 b = texture(DiffuseSampler, vec2(texCoord.x,     texCoord.y + y));
    vec4 c = texture(DiffuseSampler, vec2(texCoord.x + x, texCoord.y + y));

    vec4 d = texture(DiffuseSampler, vec2(texCoord.x - x, texCoord.y));
    vec4 e = texture(DiffuseSampler, vec2(texCoord.x,     texCoord.y));
    vec4 f = texture(DiffuseSampler, vec2(texCoord.x + x, texCoord.y));

    vec4 g = texture(DiffuseSampler, vec2(texCoord.x - x, texCoord.y - y));
    vec4 h = texture(DiffuseSampler, vec2(texCoord.x,     texCoord.y - y));
    vec4 i = texture(DiffuseSampler, vec2(texCoord.x + x, texCoord.y - y));

    // Apply weighted distribution, by using a 3x3 tent filter:
    //  1   | 1 2 1 |
    // -- * | 2 4 2 |
    // 16   | 1 2 1 |
    upsample = e*4.0;
    upsample += (b+d+f+h)*2.0;
    upsample += (a+c+g+i);
    upsample *= 1.0 / 16.0;
}
