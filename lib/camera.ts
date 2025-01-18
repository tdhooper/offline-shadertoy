import { mat4, vec3, quat } from "gl-matrix";

export class Camera {

  public version = "3";

  public position: vec3 = vec3.create();
  public rotation: quat = quat.create();
  public aspect: number = 1;
  public fov: number = 1 / (Math.PI / 5);

  private _matrix: mat4 = mat4.create();
  private _view: mat4 = mat4.create();
  private _projection: mat4 = mat4.create();

  toState(): any {
    return {
      version: this.version,
      position: Array.from(this.position),
      rotation: Array.from(this.rotation),
      fov: this.fov,
    };
  }

  fromState(state: any): void {
      
    if (state.camera && state.camera.version === this.version) {
      quat.copy(this.rotation, state.camera.rotation);
      vec3.copy(this.position, state.camera.position);
      this.fov = state.camera.fov || this.fov;
      return;
    }

    if (state.camera && state.camera.version === "2") {
      quat.copy(this.rotation, state.camera.rotation);
      vec3.copy(this.position, state.camera.position);
      this.fov = state.fov || this.fov;
      return;
    }
    
    if (state.camera && state.camera.rotation && state.camera.position) {
      if (state.camera.rotation.length == 3) {
        // Convert from old first-person-camera
        const out = mat4.create();
        mat4.translate(out, out, state.camera.position);
        mat4.rotateX(out, out, state.camera.rotation[0]);
        mat4.rotateY(out, out, state.camera.rotation[1]);
        mat4.rotateZ(out, out, state.camera.rotation[2] - Math.PI);
        mat4.getRotation(this.rotation, out);
        mat4.getTranslation(this.position, out);
      } else {
        quat.copy(this.rotation, state.camera.rotation);
        vec3.copy(this.position, state.camera.position);
      }
      // previous versions had rotation inverted
      quat.invert(this.rotation, this.rotation);
      this.fov = state.fov || this.fov;
      return;
    }

    if (state.cameraMatrix) {
      // Convert from old free-fly-camera
      mat4.getRotation(this.rotation, state.cameraMatrix);
      mat4.getTranslation(this.position, state.cameraMatrix);
      this.fov = state.fov || this.fov;
      return;
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

  projection() {
    return mat4.perspective(
      this._projection,
      1 / this.fov,
      this.aspect,
      0.01,
      1000
    )
  }
}
