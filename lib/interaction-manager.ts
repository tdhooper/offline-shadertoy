export class InteractionObject extends EventTarget {
  public hitTest: (x: number, y: number) => number;

  constructor(hitTest: (x: number, y: number) => number) {
    super();
    this.hitTest = hitTest;
  }
}

export class InteractionManager {
  private objects: InteractionObject[] = [];
  private selectedObject: InteractionObject | null = null;
  private isDragging: boolean = false;

  add(object: InteractionObject): void {
    this.objects.push(object);
  }

  remove(object: InteractionObject): void {
    const index = this.objects.indexOf(object);
    if (index >= 0) {
      this.objects.splice(index, 1);
    }
  }

  handlePointerEvent(event: PointerEvent): void {
    const { clientX, clientY, type } = event;

    let highestLayer = -1;
    let topObject: InteractionObject | null = null;

    // Determine the top-most object at the pointer location
    for (const obj of this.objects) {
      const layer = obj.hitTest(clientX, clientY);
      if (layer > highestLayer) {
        highestLayer = layer;
        topObject = obj;
      }
    }

    if (type === "pointerdown" && topObject) {
      this.isDragging = true;
      this.selectedObject = topObject;
      topObject.dispatchEvent(new CustomEvent("pointerDown", { detail: { x: clientX, y: clientY } }));
    }

    if (type === "pointerup" && this.selectedObject) {
      this.isDragging = false;
      this.selectedObject.dispatchEvent(new CustomEvent("pointerUp", { detail: { x: clientX, y: clientY } }));
    }

    if (type === "pointermove" && this.isDragging && this.selectedObject) {
      this.selectedObject.dispatchEvent(new CustomEvent("pointerDrag", { detail: { x: clientX, y: clientY } }));
    }

    if (topObject !== this.selectedObject && !this.isDragging) {
      // Emit pointerLeave on the previously selected object
      if (this.selectedObject) {
        this.selectedObject.dispatchEvent(new CustomEvent("pointerLeave"));
      }

      // Emit pointerEnter on the newly selected object
      if (topObject) {
        topObject.dispatchEvent(new CustomEvent("pointerEnter"));
      }

      // Update the selected object
      this.selectedObject = topObject;
    }
  }

  attachToDOMElement(element: HTMLElement): void {
    element.addEventListener("pointermove", (event) => this.handlePointerEvent(event));
    element.addEventListener("pointerdown", (event) => this.handlePointerEvent(event));
    element.addEventListener("pointerup", (event) => this.handlePointerEvent(event));
  }
}
