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

    drawState(stateChanged, force) {
        let isAccumulationDraw = ! (stateChanged || force);
        this.drawIndex = isAccumulationDraw ? this.drawIndex + 1 : 0;
        return {
            isAccumulationDraw: isAccumulationDraw,
            drawIndex: this.drawIndex
        };
    }

    serialize() {
        return {
            accumulate: this.accumulate
        };
    }

    fromObject(obj) {
        this.accumulate = obj.accumulate;
        this.toggle.checked = this.accumulate;
    }
}

module.exports = AccumulateControl;
