#define LIGHT_MODE 0
// #define IBL

// language extensions
#define saturate(x) clamp(x, 0.0, 1.0)
float sqr(float x){ return x*x; }
float quad(float x){ return sqr(sqr(x)); }
float pow5(float x){ return x*sqr(sqr(x)); }
const float PI = 3.14159265359;
#define INV_PI 1.0 / PI

// Directional light, direction is TO the light source
#if LIGHT_MODE == 0 // directional
vec3 uLcd; // light.color * light.candelas
vec3 uLd; // -light.transform[2].xyz
#endif
#if LIGHT_MODE == 1 // point
uniform vec3 uLcd; // light.color * light.candelas
uniform vec3 uLp; // light.transform[3].xyz
#endif
#if LIGHT_MODE == 2 // spot
uniform vec3 uLcd; // light.color * light.candelas
uniform vec3 uLp; // light.transform[3].xyz
uniform float uLcosInnerAngle;
uniform float uLcosOuterAngle;
#endif
#if LIGHT_MODE == 3 // sphere
uniform vec3 uLcd; // light.color * light.candelas
uniform vec3 uLp; // light.transform[3].xyz
uniform float uLradius; // light.size.x
#endif
#if LIGHT_MODE == 4 // disc
uniform vec3 uLcd; // light.color * light.candelas
// Not implemented yet! Sphere-spot, should probably become a proper disc light implementation
#endif
#if LIGHT_MODE == 5 // tube
uniform vec3 uLcd; // light.color * light.candelas
uniform vec3 uLp; // light.transform[3].xyz
uniform vec3 uLd; // -light.transform[2].xyz
uniform float uLradius; // light.size.x
uniform float uLlength; // light.size.y
#endif
#if LIGHT_MODE == 6 // rect
uniform vec3 uLcd; // light.color * light.candelas
uniform vec3 uLp; // light.transform[3].xyz
uniform vec3 uLorientation; // mat3(light.transform[0].xyz,light.transform[1].xyz,light.transform[2].xyz)
uniform vec2 uLsize; // light.size
#endif

uniform mat4 uP;
uniform mat4 uC;
uniform float uFrustumTop;
uniform vec2 uResolution;
uniform sampler2D uDepthBuffer;

vec3 vWorldPos; // I thought about this too late when moving to deferred, it was a varying before and now a global variable :D


// LightData creation
uniform mat4 uV;
uniform sampler2D uAlbedoRoughness;
uniform sampler2D uF0;
uniform sampler2D uN;

struct Material
{
    vec3 albedo;
    float roughness;
    vec3 reflectance;
};

struct LightData
{
    Material mtl;
    vec3 N; // Normal can be normal mapped
    float NdotV; // Calc as saturate(dot(V, N));
    vec3 R; // Calc as reflect(-V, N);
    vec3 V; // Calc as normalize(V);
};

// BRDF from
// https://cdn2.unrealengine.com/Resources/files/2013SiggraphPresentationsNotes-26915738.pdf
// https://learnopengl.com/PBR
// F
// #ifdef IBL
// For image based specular lighting
vec3 schlickFresnelRoughness(vec3 F0, float LdotH, float roughness)
{
    return F0 + saturate(vec3(1.0 - roughness) - F0) * pow5(1.0 - LdotH);
}
// #endif

#ifdef LIGHT_MODE
// For regular lighting
vec3 schlickFresnel(vec3 F0, float LdotH)
{
    return F0 + (vec3(1.0) - F0) * pow5(1.0 - LdotH);
}
// D
float ggxNormalDistribution(float NdotH, float roughness)
{
    float a2 = quad(roughness);
    return a2 / (PI * sqr(sqr(NdotH) * (a2 - 1.0) + 1.0));
}
// G
float schlickGGX(float NdotV, float NdotL, float roughness)
{
    #if 0// This version is darker, not visibly different on extreme rough or smooth surfaces
    // TODO: I think is is completely broken by now
    // Remap roughness according to Disney's modification to reduce 'hotness'
    roughness = roughness * 0.5 + 0.5;
    // karis 2013: use schlick but with k = a/2
    // to match smith ggx better. Exact match if a=1
    float k = sqr(roughness + 1.0) * 0.125;
    #else
    // https://www.shadertoy.com/view/MlB3DV
    // Seems to be a copy from http://www.filmicworlds.com/2014/04/21/optimizing-ggx-shaders-with-dotlh/
    // or https://google.github.io/filament/Filament.html , 4.6, but then that is a bunch of copy paste as well
    // Visibility term (G) : Smith with Schlick's approximation
    float k = sqr(roughness) * 0.5;
    #endif
    float G1V = NdotV / (NdotV * (1.0 - k) + k);
    float G1L = NdotL / (NdotL * (1.0 - k) + k);
    return G1V * G1L;
}

vec3 cookTorranceBRDF(vec3 L, LightData info)
{
    vec3 H = normalize(L + info.V);
    float NdotH = saturate(dot(info.N, H));
    float LdotH = saturate(dot(L, H));
    float NdotL = saturate(dot(info.N, L));

    vec3 F0 = info.mtl.reflectance;
    float D = ggxNormalDistribution(NdotH, info.mtl.roughness);
    vec3 F = schlickFresnel(F0, LdotH);
    float G = schlickGGX(info.NdotV, NdotL, info.mtl.roughness);
    vec3 Ks = D * F * G;
    vec3 Kd = info.mtl.albedo * INV_PI * (1.0 - F);

    return (Kd + Ks) * NdotL;
}

// The only difference is we pass in a custom lambert term that was computed before the
// area light calculations adjusted 'L'
vec3 cookTorranceBRDFArea(vec3 L, LightData info, float lambert)
{
    vec3 H = normalize(L + info.V);
    float NdotH = saturate(dot(info.N, H));
    float LdotH = saturate(dot(L, H));
    float NdotL = saturate(dot(info.N, L));

    vec3 F0 = info.mtl.reflectance;
    float D = ggxNormalDistribution(NdotH, info.mtl.roughness);
    vec3 F = schlickFresnel(F0, LdotH);
    float G = schlickGGX(info.NdotV, NdotL, info.mtl.roughness);
    vec3 Ks = D * F * G;
    vec3 Kd = info.mtl.albedo * INV_PI * (1.0 - F);

    return Kd * saturate(lambert) + Ks * NdotL;
}

// Differently shaped lights
vec3 _lDirectional(vec3 L, vec3 color, LightData info, float lambert)
{
    return color * cookTorranceBRDFArea(L, info, lambert);
}

// Directional light, direction is TO the light source
vec3 lDirectional(vec3 L, vec3 color, LightData info)
{
    return color * cookTorranceBRDF(L, info);
}

// Reusable code
vec3 _lPoint(vec3 local, vec3 color, LightData info)
{
    float sqrDist = dot(local, local);
    vec3 L = local * inversesqrt(sqrDist);
    return lDirectional(L, color, info) / (1.0 + sqrDist);
}
vec3 _lPoint(vec3 local, vec3 color, LightData info, float lambert)
{
    float sqrDist = dot(local, local);
    vec3 L = local * inversesqrt(sqrDist);
    return _lDirectional(L, color, info, lambert) / (1.0 + sqrDist);
}
vec3 _lArea(vec3 local, vec3 color, float radius, LightData info, float lambert)
{
    // get the closest point on the reflection ray in light-space
    vec3 centerToRay = dot(local, info.R) * info.R - local;
    // update point
    local += centerToRay * saturate(radius / length(centerToRay));
    // the rest is just a point light
    return _lPoint(local, color, info, lambert);
}

// Point light, TODO: radius? (to change the falloff & intensity based on the light size but without the sphere math)
vec3 lPoint(vec3 point, vec3 color, LightData info)
{
    return _lPoint(point - vWorldPos, color, info);
}

// Super hacky spot light, it just applies a masking term based on NdotL, which we don't even reuse from the BRDF code.
// TODO: Cookie textures, I kind of want IES support
vec3 lSpot(vec3 point, vec3 color, float cosInnerAngle, float cosOuterAngle, LightData info)
{
    return lPoint(point, color, info) * (1.0 - smoothstep(cosInnerAngle, cosOuterAngle, dot(normalize(point - vWorldPos), info.N)));
}

// This is all based on Karis 2014 area light code, was designed for phong but I'm using it to feed GGX anyways
// The idea is to adjust L so that it matches the closest point on the light's surface to the reflection ray (infinite line)
// so that LdotR becomes maximized. GGX relies more on NdotH which is a different and more complicated problem it seems.

// This references how they solved sphere lights for GGX but I don't understand the math
// so given my limited time I opted to completely omit this, as we'd have to solve it for all the other shapes too.
// https://www.guerrilla-games.com/read/decima-engine-advances-in-lighting-and-aa

// Sphere area light
vec3 lSphere(vec3 point, vec3 color, float radius, LightData info)
{
    vec3 local = point - vWorldPos;
    float lambert = dot(normalize(local), info.N);
    return _lArea(local, color, radius, info, lambert);
}

// Tube area light
vec3 lTube(vec3 center, vec3 extent, float radius, vec3 color, LightData info)
{
    vec3 start = center - extent;
    vec3 end = center + extent;
    vec3 line = extent + extent;
    float RdotLine = dot(info.R, line);
    float sqrLen = dot(line, line);
    vec3 local = start + line * saturate((dot(info.R, start) * RdotLine - dot(start, line)) / (sqrLen - sqr(RdotLine)));
    float lineLength = sqrt(sqrLen);
    vec3 lineDirection = line / lineLength;
    float lambert = dot(info.N, normalize(clamp(dot(-start, lineDirection), 0.0, lineLength) * lineDirection + start));
    // this is where it becomes identical to the sphere light
    return _lArea(local, color, radius, info, lambert);
}

// Single sided rect area light
vec3 lRect(vec3 center, mat3 orientation, vec2 size, vec3 color, LightData info)
{
    // TODO: this function is a bit long to digest, can we reuse something?
    vec3 local = center - vWorldPos;
    // distance to plane
    float planeDist = dot(local, -orientation[2]);
    if (planeDist < 0.0)// we are behind the plane, ignore
        return vec3(0.0);
    // intersect reflection ray with rectangle light
    // With phong if the rflection ray is moving away from the plane we can assume the light to be invisible
    // but with ggx that is not the case, I don't think this abs() is correct but it fixes some weird light termination.
    float intersectionDist = abs(planeDist / -dot(info.R, orientation[2]));
    // local space point on surface as hit by reflection ray
    vec3 pointOnPlane = vWorldPos + info.R * intersectionDist - center;
    // project into texture space
    vec2 uv = vec2(dot(pointOnPlane, orientation[0]), dot(pointOnPlane, orientation[1]));
    // limit to size
    uv = clamp(uv, -size, size);

    // convert back to closest point on plane relative to the point we're shading
    vec3 closestPoint = local + uv.x * orientation[0] + uv.y * orientation[1];

    vec3 L = normalize(closestPoint);// update the light direction

    // For diffuse we get closest point on light and do NdotL from there...
    // get closest point on plane
    pointOnPlane = vWorldPos + orientation[2] * planeDist;
    // get uvs
    uv = vec2(dot(pointOnPlane, orientation[0]), dot(pointOnPlane, orientation[1]));
    // limit to size
    uv = clamp(uv, -size, size);
    // project back to 3D
    pointOnPlane = local + uv.x * orientation[0] + uv.y * orientation[1];
    float distL = length(pointOnPlane);// use this for attenuation

    // TODO: this is the least physics based thing in this file, and that includes the cone light penumbra being just a gradient multiply.
    // lambert from the closest point on the plane
    float lambert = dot(info.N, pointOnPlane / distL);
    // fade everything close to the light source
    return _lDirectional(L, color, info, lambert) * saturate(planeDist * planeDist * 0.25) / (1.0 + distL * distL);
}

vec3 applyLight(LightData info)
{
#if LIGHT_MODE == 0
    return lDirectional(uLd, uLcd, info)
    #ifdef SHADOW
    * shadow(info.N)
    #endif
    ;
#endif

#if LIGHT_MODE == 1
    return lPoint(uLp, uLcd, info);
#endif

#if LIGHT_MODE == 2
    return lSpot(uLp, uLcd, uLcosInnerAngle, uLcosOuterAngle, info);
#endif

#if LIGHT_MODE == 3
    return lSphere(uLp, uLcd, uLradius, info);
#endif

#if LIGHT_MODE == 4
    // Not implemented yet! Sphere-spot, should probably become a proper disc light implementation
#endif

#if LIGHT_MODE == 5
    return lTube(uLp, uLd * uLlength, uLradius, cd, info);
#endif

#if LIGHT_MODE == 6
    return lRect(uLp, uLorientation, uLsize, cd, info);
#endif
}
#endif

#ifdef IBL
// IBL
uniform samplerCube uIRRCube;
uniform samplerCube uSpecCube;
uniform sampler2D uBRDFLUT;
vec3 ambient(LightData info)
{
    // Image based lighting
    // Specular
    vec3 F = schlickFresnelRoughness(info.mtl.reflectance, info.NdotV, info.mtl.roughness);
    // sample both the pre-filter map and the BRDF lut and combine them together as per the Split-Sum approximation

    // TODO: The IBL bake hard codes the roughness steps to be equally spaced between cube maps.
    //  The max LOD is then hard coded to match the mip count (-1).
    //  I feel it should be easier to tweak though, maybe uniform based and read mip count off the cube map?
    const float MAX_REFLECTION_LOD = 4.0;

    vec3 prefilteredColor = textureLod(uSpecCube, info.R, min(sqrt(info.mtl.roughness) * MAX_REFLECTION_LOD, MAX_REFLECTION_LOD)).xyz;
    vec2 brdf = texture(uBRDFLUT, vec2(info.NdotV, info.mtl.roughness)).rg;
    vec3 Ks = prefilteredColor * (F * brdf.x + brdf.y);

    // Diffuse
    // For energy conservation, the diffuse and specular light can't
    // be above 1.0 (unless the surface emits light); to preserve this
    // relationship the diffuse component (kD) should equal 1.0 - reflectance.
    vec3 Kd = texture(uIRRCube, info.N).xyz * info.mtl.albedo * (1.0 - F);
    return Kd + Ks;
}
#else
vec3 ambient(LightData info)
{
    return vec3(.5) * info.mtl.albedo;
}
#endif

vec3 doLighting(vec3 pos, vec3 camPos, vec3 nor, vec3 albedo, float roughness, vec3 reflectance) {
    vWorldPos = pos;
    vec3 V = normalize(camPos - vWorldPos);
    vec3 N = nor;
    LightData info = LightData(Material(albedo, roughness, reflectance), N, saturate(dot(V, N)), reflect(-V, N), V);

    float vTint = 1.;
    vec3 oColor;

#ifdef LIGHT_MODE
    oColor = vTint * applyLight(info);
#endif

// #ifdef IBL
    // oColor += ambient(info);
// #endif

    return oColor;
}
