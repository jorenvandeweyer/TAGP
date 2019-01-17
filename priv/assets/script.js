drawPipes();

function drawPipes() {
    async () => {
        const data = await fetch("/api/data").then(data => data.json()).then(console.log);
    }
    console.log(data);
    let resources = data.resource_data;
    console.log(resources);
    let h = 50, w = 80, dist = 0;

    let c = document.getElementById("canvas");
    let ctx = c.getContext("2d");

    let result = null,
        tmp = [];
    location.search
        .substr(1)
        .split("&")
        .forEach(function (item) {
            tmp = item.split("=");
            if (tmp[0] === "no_pipes") result = decodeURIComponent(tmp[1]);
        });

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
        /**if (!rotation) ctx.fillText(resources[j].type, x+10, y+font_size);
         else {
            ctx.rotate(Math.PI/2);
            ctx.fillText(resources[j].type, x+font_size, y+10);
        }*/

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