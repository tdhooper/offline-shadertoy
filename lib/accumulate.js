class AccumulateControl {

    constructor(container) {
        this.accumulate = false;
        this.drawIndex = 0;

        this.toggle = document.createElement('input');
        this.toggle.setAttribute('type', 'checkbox');
        this.toggle.checked = this.accumulate;
        container.appendChild(this.toggle);

        this.toggle.addEventListener('change', () => {
            this.accumulate = this.toggle.checked;
        });
    }

    drawState(stateChanged) {
        let isAccumulationDraw = this.accumulate && ! stateChanged;
        this.drawIndex = isAccumulationDraw ? this.drawIndex + 1 : 0;
        return {
            accumulate: {
                isAccumulationDraw: isAccumulationDraw,
                drawIndex: this.drawIndex,
            }
        };
    }

    toState() {
        return {
            accumulate: {
                enabled: this.accumulate,
                isAccumulationDraw: false,
                drawIndex: 0,
            }
        };
    }

    fromState(obj) {
        // old
        if (obj.accumulateControl) {
            this.accumulate = obj.accumulateControl.accumulate;
            this.toggle.checked = obj.accumulateControl.accumulate;
        }
        // new
        if (obj.accumulate) {
            this.accumulate = obj.accumulate.enabled;
            this.toggle.checked = obj.accumulate.enabled;
        }
    }
}

export default AccumulateControl;
