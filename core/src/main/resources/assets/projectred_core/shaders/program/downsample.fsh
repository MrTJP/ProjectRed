// Based on article by Alexander Christensen:
// https://learnopengl.com/Guest-Articles/2022/Phys.-Based-Bloom

#version 150

// This shader performs downsampling on a texture,
// as taken from Call Of Duty method, presented at ACM Siggraph 2014.
// This particular method was customly designed to eliminate
// "pulsating artifacts and temporal stability issues".

// Remember to add bilinear minification filter for this texture!
// Remember to use a floating-point texture format (for HDR)!
// Remember to use edge clamping for this texture!
uniform sampler2D DiffuseSampler;

in vec2 texCoord;
in vec2 oneTexel;

out vec4 downsample;

void main()
{
    float x = oneTexel.x;
    float y = oneTexel.y;

    // Take 13 samples around current texel:
    // a - b - c
    // - j - k -
    // d - e - f
    // - l - m -
    // g - h - i
    // === ('e' is the current texel) ===
    vec4 a = texture(DiffuseSampler, vec2(texCoord.x - 2*x, texCoord.y + 2*y));
    vec4 b = texture(DiffuseSampler, vec2(texCoord.x,       texCoord.y + 2*y));
    vec4 c = texture(DiffuseSampler, vec2(texCoord.x + 2*x, texCoord.y + 2*y));

    vec4 d = texture(DiffuseSampler, vec2(texCoord.x - 2*x, texCoord.y));
    vec4 e = texture(DiffuseSampler, vec2(texCoord.x,       texCoord.y));
    vec4 f = texture(DiffuseSampler, vec2(texCoord.x + 2*x, texCoord.y));

    vec4 g = texture(DiffuseSampler, vec2(texCoord.x - 2*x, texCoord.y - 2*y));
    vec4 h = texture(DiffuseSampler, vec2(texCoord.x,       texCoord.y - 2*y));
    vec4 i = texture(DiffuseSampler, vec2(texCoord.x + 2*x, texCoord.y - 2*y));

    vec4 j = texture(DiffuseSampler, vec2(texCoord.x - x, texCoord.y + y));
    vec4 k = texture(DiffuseSampler, vec2(texCoord.x + x, texCoord.y + y));
    vec4 l = texture(DiffuseSampler, vec2(texCoord.x - x, texCoord.y - y));
    vec4 m = texture(DiffuseSampler, vec2(texCoord.x + x, texCoord.y - y));

    // Apply weighted distribution:
    // 0.5 + 0.125 + 0.125 + 0.125 + 0.125 = 1
    // a,b,d,e * 0.125
    // b,c,e,f * 0.125
    // d,e,g,h * 0.125
    // e,f,h,i * 0.125
    // j,k,l,m * 0.5
    // This shows 5 square areas that are being sampled. But some of them overlap,
    // so to have an energy preserving downsample we need to make some adjustments.
    // The weights are the distributed, so that the sum of j,k,l,m (e.g.)
    // contribute 0.5 to the final color output. The code below is written
    // to effectively yield this sum. We get:
    // 0.125*5 + 0.03125*4 + 0.0625*4 = 1
    downsample = e*0.125;
    downsample += (a+c+g+i)*0.03125;
    downsample += (b+d+f+h)*0.0625;
    downsample += (j+k+l+m)*0.125;
}