Nothing special, just a harness so I can live edit shaders with glslify, and use the Shadertoy uniforms.

Run with:

    $ npm run dev

Then open the given url in your browser, which shows a list of projects

To add a new project, just copy an existing project's directory

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
	resetState()

Grab the current state, and make it the project default by sacing it as config.json in its project folder:

	exportState()
