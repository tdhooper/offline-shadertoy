import { vec3, quat, mat4 } from "gl-matrix";
import { Camera } from "./camera";
import { InteractionObject } from "./interaction-manager";
import pressed from "key-pressed";


interface FreeFlyCameraOptions {
  positionSpeed?: number;
  tiltSpeed?: number;
  rotationSpeed?: number;
  shiftModifier?: number;
  simDt?: number;
}

export class FreeFlyCameraControl {
  private camera: Camera;
  private positionSpeed: number;
  private tiltSpeed: number;
  private rotationSpeed: number;
  private shiftModifier: number;

  private _dir: vec3 = vec3.create();
  private _rot: quat = quat.create();
  private _mat: mat4 = mat4.create();

  private velocity: vec3 = vec3.create();
  private velocityDt: vec3 = vec3.create();
  private acceleration: vec3 = vec3.create();
  private accelerationDt: vec3 = vec3.create();
  private drag: vec3 = vec3.create();

  private simPosition: vec3 = vec3.create();
  private prevPosition: vec3 = vec3.create();

  private simDt: number;
  private currentTime: number = performance.now();
  private accumulator: number = 0;

  private lastMouseX: number | null = null;
  private lastMouseY: number | null = null;
  private dragging: boolean = false;

  private lastTime?: number;

  constructor(camera: Camera, mouse: InteractionObject, options: FreeFlyCameraOptions = {}) {
    this.camera = camera;
    this.positionSpeed = options.positionSpeed || 5;
    this.tiltSpeed = options.tiltSpeed || 10;
    this.rotationSpeed = options.rotationSpeed || 0.5;
    this.shiftModifier = options.shiftModifier || 10;
    this.simDt = options.simDt || 1;

    vec3.copy(this.simPosition, this.camera.position);
    vec3.copy(this.prevPosition, this.camera.position);

    this.registerMouseEvents(mouse);
  }

  private registerMouseEvents(mouse: InteractionObject): void {
    mouse.addEventListener("pointerDown", (e: CustomEvent) => {
      this.lastMouseX = e.detail.x;
      this.lastMouseY = e.detail.y;
      this.dragging = true;
    });

    mouse.addEventListener("pointerUp", () => {
      this.dragging = false;
      this.lastMouseX = null;
      this.lastMouseY = null;
    });

    mouse.addEventListener("pointerDrag", (e: CustomEvent) => {
      if (this.dragging) {
        const { x, y } = e.detail;
        if (this.lastMouseX !== null && this.lastMouseY !== null) {
          const dx = x - this.lastMouseX;
          const dy = y - this.lastMouseY;
          this.pointer(dx, dy, 0);
        }
        this.lastMouseX = e.detail.x;
        this.lastMouseY = e.detail.y;
      }
    });
  }

  toState(): any {
    return {
        positionSpeed: this.positionSpeed,
        tiltSpeed: this.tiltSpeed,
        rotationSpeed: this.rotationSpeed,
        shiftModifier: this.shiftModifier,
    };
  }

  fromState(state: any): void {
    this.positionSpeed = state.positionSpeed || this.positionSpeed;
    this.tiltSpeed = state.tiltSpeed || this.tiltSpeed;
    this.rotationSpeed = state.rotationSpeed || this.rotationSpeed;
    this.shiftModifier = state.shiftModifier || this.shiftModifier;
  }

  tick(): void {
    const realTime = performance.now();
    const elapsed = this.lastTime ? realTime - this.lastTime : 0;
    this.lastTime = realTime;

    this.control(
      elapsed,
      [
        pressed("W"),
        pressed("S"),
        pressed("A"),
        pressed("D"),
        pressed("E"),
        pressed("Q"),
        pressed("F"),
        pressed("R"),
      ],
      pressed("<shift>")
    );

    this.simulate();
  }

  private control(dt: number, move: boolean[], shift: boolean): void {
    let positionSpeed = this.positionSpeed / 1000000;
    let tiltSpeed = this.tiltSpeed / 1000;

    if (shift) {
      positionSpeed *= this.shiftModifier;
      tiltSpeed *= this.shiftModifier;
    }

    const [forward, back, left, right, up, down, rollRight, rollLeft] = move;

    vec3.set(this._dir, right - left, up - down, back - forward);
    if (forward || back || left || right || up || down) {
      vec3.normalize(this._dir, this._dir);
      vec3.scale(this._dir, this._dir, positionSpeed);
    }

    const tilt = (rollLeft - rollRight) * tiltSpeed * 100;
    this.move(this._dir);
    this.pointer(0, 0, tilt);
  }

  move(dir: vec3): void {
    vec3.transformQuat(this.acceleration, dir, this.camera.rotation);
    vec3.scale(this.drag, this.velocity, -0.0016 * Math.sqrt(this.positionSpeed));
    vec3.add(this.acceleration, this.acceleration, this.drag);

    const lastSpeed = vec3.sqrLen(this.velocity);
    this.simulate();
    const speed = vec3.sqrLen(this.velocity);

    if (speed < lastSpeed && speed < 0.00000001) {
      vec3.set(this.velocity, 0, 0, 0);
    }
  }

  private pointer(dx: number, dy: number, tilt: number): void {
    const x = dx * this.rotationSpeed;
    const y = dy * this.rotationSpeed;
    const z = tilt * this.rotationSpeed;

    quat.fromEuler(this._rot, -y, -x, -z);
    quat.multiply(this.camera.rotation, this.camera.rotation, this._rot);
  }

  simulate(): void {
    const newTime = performance.now();
    let frameTime = newTime - this.currentTime;
    if (frameTime > 250) {
      frameTime = 250;
    }
    this.currentTime = newTime;
    this.accumulator += frameTime;

    while (this.accumulator >= this.simDt) {
      vec3.copy(this.prevPosition, this.simPosition);
      this.integrate(this.simDt);
      this.accumulator -= this.simDt;
    }

    const alpha = this.accumulator / this.simDt;
    vec3.lerp(this.camera.position, this.prevPosition, this.simPosition, alpha);
  }

  private integrate(dt: number): void {
    vec3.scale(this.velocityDt, this.velocity, dt);
    vec3.add(this.simPosition, this.simPosition, this.velocityDt);

    vec3.scale(this.accelerationDt, this.acceleration, dt * dt * 0.5);
    vec3.add(this.simPosition, this.simPosition, this.accelerationDt);

    vec3.scale(this.accelerationDt, this.acceleration, dt);
    vec3.add(this.velocity, this.velocity, this.accelerationDt);
  }

  resetSimulation(): void {
    vec3.copy(this.prevPosition, this.camera.position);
    vec3.copy(this.simPosition, this.camera.position);
    vec3.set(this.velocity, 0, 0, 0);
  }

  lookAtOrigin(): void {
    const target = vec3.create();
    const up = vec3.fromValues(0, 1, 0);
    mat4.lookAt(this._mat, this.camera.position, target, up);
    mat4.getRotation(this.camera.rotation, this._mat);
  }
}
