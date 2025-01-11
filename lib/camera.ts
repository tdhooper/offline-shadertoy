import { mat4, vec3, quat } from "gl-matrix";

export class Camera {

  public version = "2";

  public position: vec3 = vec3.create();
  public rotation: quat = quat.create();

  private _matrix: mat4 = mat4.create();
  private _view: mat4 = mat4.create();

  toState(): any {
    return {
      version: this.version,
      position: Array.from(this.position),
      rotation: Array.from(this.rotation),
    };
  }

  fromState(state: any): void {
    if (state.version === this.version) {
      quat.copy(this.rotation, state.rotation);
      vec3.copy(this.position, state.position);
    } else {
      if (state.rotation && state.position) {
        if (state.rotation.length == 3) {
          // Convert from old first-person-camera
          const out = mat4.create();
          mat4.translate(out, out, state.position);
          mat4.rotateX(out, out, state.rotation[0]);
          mat4.rotateY(out, out, state.rotation[1]);
          mat4.rotateZ(out, out, state.rotation[2] - Math.PI);
          mat4.getRotation(this.rotation, out);
          mat4.getTranslation(this.position, out);
        } else {
          quat.copy(this.rotation, state.rotation);
          vec3.copy(this.position, state.position);
        }
      } else if ('0' in state) {
        // Convert from old free-fly-camera
        mat4.getRotation(this.rotation, state);
        mat4.getTranslation(this.position, state);
      }
      // previous versions had rotation inverted
      quat.invert(this.rotation, this.rotation);
    }
  }

  matrix() {
    mat4.fromRotationTranslation(this._matrix, this.rotation, this.position);
    return this._matrix;
  };

  view() {
    mat4.invert(this._view, this.matrix());
    return this._view;
  };
}
