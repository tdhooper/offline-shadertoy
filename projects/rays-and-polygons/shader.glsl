    #extension GL_EXT_frag_depth : enable
    precision mediump float;
    uniform mat4 projection;
    varying vec3 eye;
    varying vec3 dir;
    varying vec3 cameraForward;

    float fBox(vec3 p, vec3 s) {
      p = abs(p) - s;
      return max(p.x, max(p.y, p.z));
    }

    float map(vec3 p) {
      // 1 x 1 x 1 Box
      float d = fBox(p, vec3(.5));
      d = 1e12;

      d = -fBox(p, vec3(5.));

      p = mod(p, 1.) - .5;
      d = min(d, length(p) - .04);
      return d;
    }

    vec3 calcNormal(vec3 p) {
      vec3 eps = vec3(.001,0,0);
      vec3 n = vec3(
        map(p + eps.xyy) - map(p - eps.xyy),
        map(p + eps.yxy) - map(p - eps.yxy),
        map(p + eps.yyx) - map(p - eps.yyx)
      );
      return normalize(n);
    }

    const float ITER = 50.;

    void main() {

      vec3 rayOrigin = eye;
      vec3 rayDirection = normalize(dir);
      vec3 rayPosition = rayOrigin;
      float rayLength = 0.;

      float distance = 0.;
      vec3 color = vec3(0);
      for (float i = 0.; i < ITER; i++) {
        rayLength += distance;
        rayPosition = rayOrigin + rayDirection * rayLength;
        distance = map(rayPosition);
        color += .05;
        if (distance < .001) {
          color *= calcNormal(rayPosition) * .5 + .5;
          break;
        }
      }

      float eyeHitZ = -rayLength * dot(rayDirection, cameraForward);

      vec3 eyeSpace = vec3(0, 0, eyeHitZ);
      float zc = ( projection * vec4(eyeSpace, 1)).z;
      float wc = ( projection * vec4(eyeSpace, 1)).w;
      float depth = (zc/wc + 1.) / 2.;

      gl_FragColor = vec4(color, 1);
      gl_FragDepthEXT = depth;
    }