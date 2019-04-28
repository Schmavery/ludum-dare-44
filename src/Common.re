open Reprocessing;

module StringMap = Map.Make(String);

type character = | Player | NPC;

type dialogItem = {
  assetName: string,
  label: string,
  character,
  content: list(string),
};

module Point = {
  type t = {
    x: float,
    y: float,
  };

  let zero = {x: 0., y: 0.};
  let create = (x, y) => {x, y};
  let toPair = ({x, y}) => (x, y);
  let fromPair = ((x, y)) => {x, y};
  let fromIntPair = ((x, y)) => {x: float_of_int(x), y: float_of_int(y)};
  let add = ({x: x1, y: y1}, {x: x2, y: y2}) => {x: x1 +. x2, y: y1 +. y2};
  let sub = ({x: x1, y: y1}, {x: x2, y: y2}) => {x: x1 -. x2, y: y1 -. y2};
  let moveTo = (src, ~dest, ~speed) => {
    let dist = Utils.distf(~p1=toPair(src), ~p2=toPair(dest));
    if (dist <= speed) {
      dest;
    } else {
      let {x: srcX, y: srcY} = src;
      let {x: destX, y: destY} = dest;
      let mult = speed /. dist;
      let d = create((destX -. srcX) *. mult, (destY -. srcY) *. mult);
      add(src, d);
    };
  };
  let print = t => Printf.printf("{x:%f,y:%f}\n%!", t.x, t.y);
  let (+) = add;
  let (-) = sub;
};

module Sprite = {
  type sheetEntry = {
    x: int,
    y: int,
    w: int,
    h: int,
  };

  type t = {
    sheet: Reprocessing.imageT,
    map: StringMap.t(sheetEntry),
  };

  let create = (sheet, map) => {sheet, map};
};
