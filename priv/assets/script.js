function addClickEvent(id, fun) {
    document.querySelector(id).addEventListener("click", fun);
}

class Instance {
    constructor(inst) {
        this.pid = inst.pid;
        this.type = inst.type;
        // this.getInfo();
    }

    async getInfo() {
        const data = await request(`/api/instance/${this.pid}`, "GET");
        console.log(data);
    }
}

class System {
    constructor() {
        this.resources = [];
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

async function request(path, method="get", data) {
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

    addClickEvent("#observer", () => {
        fetch("/api/observer", {method: "POST"});
    });

    addClickEvent("#test", () => {
        fetch("/api/test", {method: "POST"});
        system.updateSystem();
    });

    return system;
}

const system = init();

async function drawPipes(system) {
    const resources = system.resources;
    console.log(resources);

    let h = 70, w = 100, dist = 0;

    let c = document.querySelector("#canvas");
    let ctx = c.getContext("2d");

    let no_pipes = resources.length;
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

    for (let j = 0; j < (top_side + bottom_side + left_side + right_side); j++) {
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

    }
}
/**
 for (let i = 0; i<top_side; i++) {
        ctx.strokeRect(10 + i*(w+dist), 10, w, h);
      }
 for (let i = 0; i<bottom_side; i++) {
        if (bottom_side==1) {
          ctx.strokeRect(10+dist + w + i*(w+dist), 10+dist + h + (left_side)*(w+dist), w, h);
        } else {
          ctx.strokeRect(10 + i*(w+dist), 10+dist + h + (left_side)*(w+dist), w, h);
        }
      }
 for (let i = 0; i<left_side; i++) {
        ctx.strokeRect(10, 10+dist + h + i*(dist+w), h, w);
      }
 for (let i = 0; i<right_side; i++) {
        ctx.strokeRect(10 + (w-h) + (top_side-1)*(w+dist), 10+dist + h + i*(dist+w), h, w);
      }*/

      
