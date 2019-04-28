open Reprocessing;
open Common;

let darken = (color: colorT, percentage): colorT => {
  let p = float_of_int(percentage) /. 100.;
  {
    r: color.r *. (1. -. p),
    g: color.g *. (1. -. p),
    b: color.b *. (1. -. p),
    a: color.a,
  };
};

let niceRect = (~pos, ~color, ~width, ~height, env) => {
  Draw.fill(color, env);
  Draw.stroke(darken(color, 20), env);
  Draw.strokeWeight(10, env);
  Draw.rectf(
    ~pos=Point.toPair(pos),
    ~height=height +. 10.,
    ~width=width +. 10.,
    env,
  );
  Draw.fill(color, env);
  Draw.stroke(color, env);
  Draw.rectf(
    ~pos=Point.(toPair(pos + create(5., 5.))),
    ~height,
    ~width,
    env,
  );
};

let actionLabel = (actionName, pos, font, env) => {
  let w = Draw.textWidth(~font, ~body=actionName, env);
  Draw.pushStyle(env);
  let c = Constants.green;
  Draw.fill(c, env);
  Draw.stroke(darken(c, 20), env);
  Draw.strokeWeight(10, env);
  Draw.rectf(
    ~pos=Point.(toPair(pos + create(0., -35.))),
    ~height=40.,
    ~width=float_of_int(w + 15),
    env,
  );
  Draw.fill(c, env);
  Draw.stroke(c, env);
  Draw.rectf(
    ~pos=Point.(toPair(pos + create(5., -30.))),
    ~height=30.,
    ~width=float_of_int(w + 5),
    env,
  );
  Draw.tint(Constants.black, env);
  Draw.text(
    ~font,
    ~pos=(int_of_float(pos.x) + 10, int_of_float(pos.y)),
    ~body=actionName,
    env,
  );
  Draw.popStyle(env);
};

let placeholderUncentered = (name, ~pos, ~width, ~height, env) => {
  Draw.fill(Constants.red, env);
  Draw.rectf(~pos=Point.(toPair(pos)), ~width, ~height, env);
  Draw.text(
    ~body=name,
    ~pos=(int_of_float(pos.x), int_of_float(pos.y)),
    env,
  );
};

let sprite = (t, name, ~pos, ~flipped=false, ~scale=1.0, env) => {
  Reprocessing.(
    switch (StringMap.find(name, t.Sprite.map)) {
    | {x, y, w, h} =>
      let width = float_of_int(w) *. scale *. (flipped ? (-1.) : 1.);
      let height = float_of_int(h) *. scale;
      Draw.subImagef(
        t.sheet,
        ~pos=Point.(toPair(pos - create(width /. 2., height /. 2.))),
        ~width,
        ~height,
        ~texPos=(x, y),
        ~texWidth=w,
        ~texHeight=h,
        env,
      );
    | exception Not_found =>
      placeholderUncentered(
        name,
        ~pos=Point.(pos - create(50., 50.)),
        ~width=50.,
        ~height=50.,
        env,
      )
    }
  );
};

module Dialog = {
  let height = 150.;
  let padding = 40.;
  let windowFix = 10.;

  let box = env => {
    niceRect(
      ~pos=
        Point.create(
          padding,
          float_of_int(Env.height(env)) -. height -. padding -. windowFix,
        ),
      ~color=Utils.color(~r=100, ~g=100, ~b=255, ~a=255),
      ~height,
      ~width=float_of_int(Env.width(env)) -. 2. *. padding -. windowFix,
      env,
    );
  };

  let face = (~x, assetName, label, sprites, font, env) => {
    let top = float_of_int(Env.height(env)) -. height +. 10.;
    sprite(sprites, assetName, ~scale=0.3, ~pos=Point.create(x, top), env);
    let tw = Draw.textWidth(~body=label, ~font, env) / 2;
    Draw.text(
      ~body=label,
      ~font,
      ~pos=(int_of_float(x) - tw, int_of_float(top +. 80.)),
      env,
    );
  };

  let text = (~x, content, percentScrolled, font, env) => {
    let lineHeight = 40.;
    let top =
      float_of_int(Env.height(env))
      -. height
      -. windowFix
      +. height
      /. 2.
      -. float_of_int(List.length(content))
      *. (lineHeight /. 2.);
    let numChars =
      if (percentScrolled >= 1.0) {
        10000;
      } else {
        List.fold_left((acc, s) => acc + String.length(s), 0, content);
      };
    let rec drawSomeText = (row, charsLeft, content) =>
      switch (charsLeft, content) {
      | (_, []) => ()
      | (n, _) when n <= 0 => ()
      | (n, [s, ...rest]) =>
        let len = String.length(s);
        /* Printf.printf("%i %i\n%!", n, len); */
        let body = String.sub(s, 0, min(n, len));
        /* let body = s; */
        Draw.text(
          ~body,
          ~font,
          ~pos=(
            int_of_float(x),
            int_of_float(top +. float_of_int(row) *. lineHeight),
          ),
          env,
        );
        drawSomeText(row + 1, n - len, rest);
      };
    drawSomeText(
      0,
      int_of_float(float_of_int(numChars) *. percentScrolled),
      content,
    );
  };

  let next = (time, font, env) => {
    let top = Env.height(env) - int_of_float(padding -. windowFix) - 15;
    Draw.pushStyle(env);
    Draw.tint({...Constants.white, a: min(1.0, time /. 1.0)}, env);
    Draw.text(
      ~body="Click to continue",
      ~font,
      ~pos=(Env.width(env) / 2 - 50, top),
      env,
    );
    Draw.popStyle(env);
  };
};
