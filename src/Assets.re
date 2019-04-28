open Common;

let maybeInt = (f) => switch (int_of_float(f)) {
  | i => Some(i)
  | exception Failure(_) => None
};

let loadSpriteSheet = (filename, cb) =>
  Reasongl.Gl.File.readFile(
    ~filename,
    ~cb=jsonString => {
      open Json.Infix;
      let json = Json.parse(jsonString);
      let things = Json.get("frames", json) |?> Json.array |! "Expected field `frames` to be an array";
      let assets =
        List.fold_left(
          (assets, thing) => {
            let frame = Json.get("frame", thing) |! "Expected field `frame` in `frames` array";
            let x = Json.get("x", frame) |?> Json.number |?> maybeInt |! "Invalid field `x`";
            let y = Json.get("y", frame) |?> Json.number |?> maybeInt |! "Invalid field `y`";
            let w = Json.get("w", frame) |?> Json.number |?> maybeInt |! "Invalid field `w`";
            let h = Json.get("h", frame) |?> Json.number |?> maybeInt |! "Invalid field `h`";
            let name = Json.get("filename", thing) |?> Json.string |! "Invalid field `filename`";
            let name = List.hd(Reprocessing.Utils.split(name, ~sep='.'));
            StringMap.add(name, Sprite.({x, y, w, h}), assets);
          },
          StringMap.empty,
          things,
        );
      cb(assets)
    },
  );

