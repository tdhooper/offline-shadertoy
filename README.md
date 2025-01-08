https://tdhooper.github.io/offline-shadertoy/

Nothing special, just a harness so I can live edit shaders with glslify, and use the Shadertoy uniforms.

Run with:

    $ npm run dev

Then open the given url in your browser, which shows a list of projects

To add a new project, just copy an existing project's directory

# Camera

Free-fly-camera, when used, has the controls

	W - forward
	S - back
	A - left
	D - right

	Q - down
	E - up

	R - tilt right
	F - tilt left

	Mouse (click and drag) - look

Reset camera and other state from your browser console with:

	resetCamera()

Use the camera in shaders with:

	in vec3 eye;
	in vec3 dir;

	vec3 rayOrigin = eye;
  	vec3 rayDirection = normalize(dir);

Camera fov and movement speed can be set in the config...

# Config

Various things can be configured, get an idea by grabing the current state from your browser console:
 
	exportState()

Make it the project default by saving it as `config.json` in a project's folder

After saving changes to `config.json`, reload it with:

 	resetState()

# Multipass

Passes can be defined by adding comments to sampler uniforms, this will load the result of `buffer-a.glsl` into `iChannel0`:

	uniform sampler2D iChannel0; // buffer-a.glsl, filter: linear, wrap: clamp

Textures can be similarly loaded:

	uniform sampler2D noiseTexture; // images/noise.png, filter: nearest, wrap: repeat

# Gizmos

Transform objects in SDFs with gizmos using the `gmTransform(p)` function, see the multi-gizmo project for an example https://tdhooper.github.io/offline-shadertoy/projects/multi-gizmo

You will need to add a GIZMO_MAP function that returns your SDF:

	vec3 GIZMO_MAP(vec3 p) {
 		return map(p);
	}
