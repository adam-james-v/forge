/*
-------------------------------------

WebGL Forge Viewer
by Adam Vermeer aka adam-james - 2019

-------------------------------------

Shader source code taken and modified from:
https://www.shadertoy.com/view/wdGGz3

-------------------------------------

Original Header:

"ShaderToy Tutorial - Ray Marching Operators 2" 
by Martijn Steinrucken aka BigWings/CountFrolic - 2019
License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

This is the starting point for a YouTube tutorial:
https://youtu.be/Vmb7VGBVZJA

-------------------------------------

Modifications:
- fully annotated the code
- improved fn and var names for clarity
- adjusted camera pos and zoom for pseudo-axonometric appearance
- re-structured functions for better compilation from Forge lib


-------------------------------------
*/

// +----------------------------------+
// |                                  |
// |   BEGIN COMPILER INPUT SECTION   |
// |                                  |
// +----------------------------------+

// CSG operations
//
// The Forge compiler will emit valid CSG operations.
// ops defined here are for prototype purposes

float sdSmoothUnion( float a, float b, float k ) {
    float h = clamp(0.5 + 0.5*(b - a)/k, 0.0, 1.0);
    return mix(b, a, h) - k*h*(1.0 - h);
}

float sdUnion( float a, float b ) {
    return min(a, b);
}

float sdIntersection( float a, float b ) {
    return max(a, b);
}

// F-REP Solids
//
// The Forge compiler will emit valid F-REP functions.
// solids defined here are for prototype purposes

float sdTorus(vec3 pt, vec2 r) {
    float x = length(pt.xz) - r.x;
    return length(vec2(x, pt.y)) - r.y;
}

float sdBox(vec3 pt, vec3 s) {
    pt = abs(pt) - s;
    return length(max(pt, 0.0)) + min(max(pt.x, max(pt.y, pt.z)), 0.0);
}

// Scene
//
// the sdScene function takes a 3d pt and returns a single distance value
// distance is the min. dist. a pt is away from the surface of any object in the scene
//
// the scene function is emitted by the forge compiler
// and is a GLSL equivalent representation of the scene code

float sdScene(vec3 pt) {

    // objects in scene
    float box = sdBox(pt - vec3(0.0, 1.0, 0.0), vec3(1.0));
    float torus = sdTorus(pt - vec3(0.0, 1.1, 0.0), vec2(1.9, 0.3));
    
    // distance from point to nearest object in scene
    // 
    // the basic scene is the union of all objects
    float dist = sdUnion(box, torus);
    return dist;
}

// +----------------------------------+
// |                                  |
// |   END COMPILER INPUT SECTION     |
// |                                  |
// +----------------------------------+


// define limit values
// 
// MAX_STEPS: 
// maximum number of marches to take along a ray
// small values (<< 100) cause artefacts around edges
// large values (>> 1000) increase computation requirements
//
// MAX_DIST:
// maximum distance in world units to cast a ray to
// creates a sphere beyond which no objects are rendered
// low values cause clipping
// low values cause distortion due to pseudo-axonometric projection
// high values can increase computation when infinite objects exist in scene
//
// SURF_DIST:
// the distance at which a ray is considered to be hitting a surface
// large values (around 0.1) cause incorrect surface rendering
// small values (>> 0.00001) improve accuracy but increase computation requirements
//
// defaults:
// the given defaults work acceptably well for simple to medium complexity scenes
// where complexity is determined by manual inspection
// 
// TODO: determine a heuristic which can automatically adjust limit values to
// balance accuracy and usability of the scene based on code complexity

#define MAX_STEPS 200
#define MAX_DIST 20000.
#define SURF_DIST .0001

// declare needed uniforms
//
// mouse used to pass mouse position data
// resolution used to pass canvas resolution data
uniform vec2 resolution;
uniform vec2 mouse;





// utility functions
//
// Rot calculates a 2d rotation matrix given an angle in radians
// used for basic camera rotation implementation

mat2 rot(float a) {
    float s = sin(a);
    float c = cos(a);
    return mat2(c, -s, s, c);
}

// Ray Marching functions

float marchRay(vec3 rayOrigin, vec3 rayDirection) {
    
    float distMarched = 0.0;
    
    for (int i = 0; i < MAX_STEPS; i++) {
        vec3 pt = rayOrigin + rayDirection*distMarched;
        float distStep = sdScene(pt);
        distMarched += distStep;
        if (distMarched > MAX_DIST || distStep < SURF_DIST) break;
    }
    
    return distMarched;
}

vec3 estimateNormal(vec3 pt) {
    float dist = sdScene(pt);
    vec2 epsilon = vec2(0.0001, 0);
    
    vec3 n = dist - vec3(
        sdScene(pt - epsilon.xyy),
        sdScene(pt - epsilon.yxy),
        sdScene(pt - epsilon.yyx));
    
    return normalize(n);
}

vec3 rayDirection(vec2 uv, vec3 pt, vec3 lookAt, float zoom) {
    vec3 f = normalize(lookAt - pt) ,
         r = normalize(cross(vec3(0, 1, 0), f)) ,
         u = cross(f, r) ,
         c = pt + f*zoom ,
         i = c + uv.x*r + uv.y*u ,
         dir = normalize(i - pt);
    return dir;
}

// MAIN
//
// rendering occurs in the main function
// this is a basic viewer implementing a barebones ray marching algorithm
// it uses no lighting 

void main() {

    // uv is the pixel coordinate mapped onto [-1 1] range in x and y
    vec2 uv = (gl_FragCoord.xy-.5*resolution.xy)/resolution.y;
    vec2 m = mouse;

    // CAMERA
    // 
    // the camera is placed far back due to dependence on MAX_DIST
    // this is done to approximate axonometric projection
    // the ray direction vector is zoomed proportionally to compensate 

    vec3 ro = vec3(MAX_DIST/4.0, 0.0, -MAX_DIST/2.0);
    ro.yz *= rot(-m.y*3.14+1.);
    ro.xz *= rot(-m.x*6.2831);
    
    float zoom = 1.1;
    vec3 lookAt = vec3(0, 1, 0);
    vec3 rd = rayDirection(uv, ro, lookAt, MAX_DIST/20.*zoom);
    float d = marchRay(ro, rd);
    
    // when no ray intersection occurs, a default colour is used
    // in this case, col.rgb and alpha are all set to 0.0
    vec3 col = vec3(0.0);
    gl_FragColor = vec4(col, 0.0);

    if(d<MAX_DIST) {
        vec3 p = ro + rd * d;

        col = (estimateNormal(p)*0.5 + 0.5);
        col = pow(col, vec3(1.2)); // gamma 
        gl_FragColor = vec4(col, 1.0);
    }
}