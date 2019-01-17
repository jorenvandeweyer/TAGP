const h = 70, w = 100, dist = 0;

function addClickEvent(id, fun) {
    document.querySelector(id).addEventListener("click", fun);
}

class EventListener {
    constructor() {
        this.listeners = new Map();
    }

    emit(event, ...opts) {
        if (!this.listeners.has(event)) return;
        const fun = this.listeners.get(event);
        fun(...opts, this);
    }

    on(event, fun) {
        this.listeners.set(event, fun);
    }

    clear(event) {
        this.listeners.delete(event);
    }
}

class Instance {
    constructor(inst) {
        this.pid = inst.pid;
        this.type = inst.type;
        // this.getInfo();
    }

    async getInfo() {
        this.info = await request(`/api/instance/${this.pid}`, "GET");
        return this.info;
    }

    async on() {
        if (this.type !== "pumpInst") return;
        const data = await request(`/api/instance/${this.pid}`, "POST", {
            on: true
        });
        return data;
    }

    async off() {
        if (this.type !== "pumpInst") return;
        const data = await request(`/api/instance/${this.pid}`, "POST", {
            on: false
        });
        return data
    }
}

class System extends EventListener {
    constructor() {
        super();
        this.resources = [];
        this.selected = null;
    }

    async select(index) {
        this.selected = this.resources[index];
        if (this.selected) {
            await this.selected.getInfo();
            this.emit("selected", this.selected);
        } else {
            this.emit("unselected");
        }
        this.visualise();
    }

    async selectedOn() {
        if (!this.selected) return;
        this.selected.on();
    }

    async selectedOff() {
        if (!this.selected) return;
        this.selected.off();
    }

    async updateSystem() {
        const data = await request("/api/system", "GET");

        this.resources = data.map(inst => new Instance(inst));

        this.visualise();
        return this.resources;
    }

    async addInstance(type) {
        const result = await request("/api/system", "POST", {
            type,
        });
        await this.updateSystem();
        return result;
    }

    addPipe() {
        return this.addInstance("pipe");
    }

    addFlowMeter() {
        return this.addInstance("flowMeter"); 
    }

    addHeatExch() {
        return this.addInstance("heatExchanger");
    }

    addPump() {
        return this.addInstance("pump");
    }

    removeLast() {
        return this.addInstance("removeLast");
    }

    visualise() {
        drawPipes(this);
    }
}

async function request(path, method="GET", data) {
    return fetch(path, {
        method,
        cache: "no-cache",
        headers: {
            "Content-Type": "application/json",

        },
        body: data ? JSON.stringify(data) : null,
    }).then(data => data.json());
}

async function init() {
    const system = new System();

    await system.updateSystem();

    addClickEvent("#addPipe", () => system.addPipe());
    addClickEvent("#addFlowMeter", () => system.addFlowMeter());
    addClickEvent("#addHeatExch", () => system.addHeatExch());
    addClickEvent("#addPump", () => system.addPump());
    addClickEvent("#removeLast", () => system.removeLast());
    addClickEvent("#turn_on", () => system.selectedOn());
    addClickEvent("#turn_off", () => system.selectedOff());
    addClickEvent("#observer", () => {
        fetch("/api/observer", {method: "POST"});
    });

    addClickEvent("#test", () => {
        fetch("/api/test", {method: "POST"});
        system.updateSystem();
    });

    addClickEvent("#canvas", (event) => {
        system.emit("click", event.offsetX, event.offsetY);
    });

    system.on("click", selectInstance);
    system.on("selected", (selected) => {
        document.querySelector("#info_title").innerText = `Info Window Pipe`;
        document.querySelector("#info_line1").innerText = `Flow Influence ${selected.info.flow_influence}`;
        document.querySelector("#info_line2").innerText = `Pid: ${selected.pid}`;
        document.querySelector("#info_line3").innerText = `Type: ${selected.type}`;
    });

    return system;
}

function selectInstance(x_canvas, y_canvas, system) {
    const resources = system.resources;
    const no_pipes = resources.length;

    let top_side = 0, bottom_side = 0, left_side = 0, right_side = 0;

    if (no_pipes < 4) {
        top_side = no_pipes;
    } else {
        if (no_pipes < 8) {
            top_side = 1;
            bottom_side = 1;
            right_side = 1;
            left_side = 1;
            switch (no_pipes - 4) {
                case 0:
                    bottom_side--;
                    top_side++;
                    break;
                case 1:
                    top_side++;
                    break;
                case 2:
                    top_side++;
                    bottom_side++;
                    break;
                case 3:
                    top_side += 2;
                    bottom_side++;
                    break;
            }
        } else {
            let sides = Math.floor((no_pipes - 4) / 4);
            top_side = sides;
            bottom_side = sides;
            left_side = sides;
            right_side = sides;
            if (no_pipes % 4 == 0) {
                top_side += 2;
                bottom_side += 2;
            } else {
                let rem = (no_pipes - 4) % 4;
                switch (rem) {
                    case 1:
                        left_side++;
                        right_side++;
                        top_side += 2;
                        bottom_side++;
                        break;
                    case 2:
                        left_side++;
                        right_side++;
                        top_side += 2;
                        bottom_side += 2;
                        break;
                    case 3:
                        left_side++;
                        right_side++;
                        top_side += 3;
                        bottom_side += 2;
                        break;
                }
            }
        }
    }

    for (let j = 0; j < no_pipes; j++) {
        if (j < top_side) {
            const i = j;
            const x = 10 + i * (w + dist);
            const y = 10;
            if (x_canvas >= x && x_canvas <= x+w &&
                y_canvas >= y && y_canvas <= y+h) {
                return system.select(j);
            }
        } else if (j < top_side + right_side) {
            const i = j - top_side;
            const x = 10 + (w - h) + (top_side - 1) * (w + dist);
            const y = 10 + dist + h + i * (dist + w);
            if (x_canvas >= x && x_canvas <= x+h &&
                y_canvas >= y && y_canvas <= y+w) {
                return system.select(j);
            } 
        } else if (j < top_side + right_side + bottom_side) {
            const i = j - top_side - right_side;
            const x = 10 + i * (w + dist);
            const y = 10 + dist + h + (left_side) * (w + dist);
            if (x_canvas >= x && x_canvas <= x+w &&
                y_canvas >= y && y_canvas <= y+h) {
                return system.select(j);
            }
        } else {
            const i = j - top_side - right_side - bottom_side;
            const x = 10;
            const y = 10 + dist + h + i * (dist + w);
            if (x_canvas >= x && x_canvas <= x+h &&
                y_canvas >= y && y_canvas <= y+w) {
                return system.select(j);
            }
        }
    }

    system.select(null);
}

async function drawPipes(system) {
    const resources = system.resources;
    console.log(resources);

    const c = document.querySelector("#canvas");
    const ctx = c.getContext("2d");

    const no_pipes = resources.length;
    let top_side = 0, bottom_side = 0, left_side = 0, right_side = 0;

    if (no_pipes < 4)
        top_side = no_pipes;
    else {
        if (no_pipes < 8) {
            top_side = 1;
            bottom_side = 1;
            right_side = 1;
            left_side = 1;
            switch (no_pipes - 4) {
                case 0:
                    bottom_side--;
                    top_side++;
                    break;
                case 1:
                    top_side++;
                    break;
                case 2:
                    top_side++;
                    bottom_side++;
                    break;
                case 3:
                    top_side += 2;
                    bottom_side++;
                    break;
            }
        } else {
            let sides = Math.floor((no_pipes - 4) / 4);
            top_side = sides;
            bottom_side = sides;
            left_side = sides;
            right_side = sides;
            if (no_pipes % 4 == 0) {
                top_side += 2;
                bottom_side += 2;
            } else {
                let rem = (no_pipes - 4) % 4;
                switch (rem) {
                    case 1:
                        left_side++;
                        right_side++;
                        top_side += 2;
                        bottom_side++;
                        break;
                    case 2:
                        left_side++;
                        right_side++;
                        top_side += 2;
                        bottom_side += 2;
                        break;
                    case 3:
                        left_side++;
                        right_side++;
                        top_side += 3;
                        bottom_side += 2;
                        break;
                }
            }
        }
    }

    let dpi = window.devicePixelRatio;
    let width = (20 + top_side * (dist + w));
    let height = (20 + 2 * (dist + h) + left_side * (dist + w));
    c.style.width = width + "px";
    c.style.height = height + "px";
    c.width = width * dpi;
    c.height = height * dpi;
    ctx.scale(dpi, dpi);

    for (let j = 0; j < no_pipes; j++) {
        if (system.resources[j] == system.selected) {
            ctx.save();
            ctx.lineWidth = 5;
            ctx.strokeStyle = "#7D7D7D";
        }
        let rotation = false;
        let x, y;
        if (j < top_side) {
            let i = j;
            x = 10 + i * (w + dist);
            y = 10;
            ctx.strokeRect(x, y, w, h);
        }
        else if (j < top_side + right_side) {
            rotation = true;
            let i = j - top_side;
            x = 10 + (w - h) + (top_side - 1) * (w + dist);
            y = 10 + dist + h + i * (dist + w);
            ctx.strokeRect(x, y, h, w);
        }
        else if (j < top_side + right_side + bottom_side) {
            let i = j - top_side - right_side;
            x = 10 + i * (w + dist);
            y = 10 + dist + h + (left_side) * (w + dist);
            ctx.strokeRect(x, y, w, h);
        }
        else {
            rotation = true;
            let i = j - top_side - right_side - bottom_side;
            x = 10;
            y = 10 + dist + h + i * (dist + w);
            ctx.strokeRect(x, y, h, w);
        }

        let font_size = 15;
        ctx.font = font_size + "px Arial";
        if (!rotation) {
            ctx.textAlign = "center"
            ctx.fillText(resources[j].type, x+w/2, y+h/2);
        }
        else {
            //ctx.rotate(Math.PI/2);
            ctx.textAlign = "center"
            ctx.fillText(resources[j].type, x+h/2, y+w/2);
        }
        ctx.restore();
    }
}

const system = init();
