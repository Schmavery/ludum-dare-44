open Reprocessing;
open Common;
let debug = false;
/* let debug = true; */

type mouse = {
  down: bool,
  up: bool,
  pressed: bool,
  pos: Point.t,
};

type roomKind =
  | Lobby
  | Tattoo
  | Cellar;

type state = {
  position: Point.t,
  destination: Point.t,
  facingRight: bool,
  destinationReachedTrigger: option(state => state),
  sprites: Sprite.t,
  font: fontT,
  currentRoom: roomKind,
  mouse,
  rooms,
  elapsedTime: float,
  /* holding: option(item), */
  /* inventory: list(item), */
  dialog: list(dialogItem),
  dialogTimer: float,
  dialogOverTrigger: option(state => state),
}
and item = {
  pos: Point.t,
  label: string,
  width: float,
  height: float,
  img: option(string),
  action: state => state,
}
and room = list(item)
and rooms = {
  lobby: room,
  tattoo: room,
  cellar: room,
};

let floorHeight = 450.;
let dialogScrollTime = 0.7;

module Action = {
  let empty = s => s;
  let print = (text, s) => {
    print_endline(text);
    s;
  };
  let walkTo = (destination, cb, state) => {
    ...state,
    destination,
    destinationReachedTrigger: Some(cb),
  };
  let walkToMouse = (cb, state) => {
    let destination =
      Point.{x: state.mouse.pos.x, y: max(floorHeight, state.mouse.pos.y)};
    {...state, destination, destinationReachedTrigger: Some(cb)};
  };
  let dialog = (dialog, cb, state) => {
    ...state,
    dialog,
    dialogTimer: 0.,
    dialogOverTrigger: Some(cb),
  };
  let changeRoom = (~pos=?, room, cb, state) =>
    cb(
      switch (pos) {
      | None => {...state, currentRoom: room}
      | Some(p) => {...state, position: p, destination: p, currentRoom: room}
      },
    );
};

let createRooms = () => {
  lobby: [
    {
      pos: Point.create(150., 170.),
      label: "open",
      width: 140.,
      height: 320.,
      img: None,
      action: Action.walkToMouse(Action.changeRoom(Tattoo, Action.empty)),
    },
    {
      pos: Point.create(630., 260.),
      label: "open",
      width: 100.,
      height: 180.,
      img: None,
      action:
        Action.walkToMouse(Action.dialog(Dialog.doorTest, Action.empty)),
    },
  ],
  tattoo: [
    {
      pos: Point.create(150., 170.),
      label: "open",
      width: 140.,
      height: 320.,
      img: None,
      action: Action.walkToMouse(Action.changeRoom(Lobby, Action.empty)),
    },
    {
      pos: Point.create(630., 260.),
      label: "open",
      width: 100.,
      height: 180.,
      img: None,
      action:
        Action.(walkToMouse(Action.dialog(Dialog.doorLocked, Action.empty))),
    },
  ],
  cellar: [],
};

let getRoom = (kind, rooms) =>
  switch (kind) {
  | Lobby => rooms.lobby
  | Tattoo => rooms.tattoo
  | Cellar => rooms.cellar
  };

let getRoomAssetName = kind =>
  switch (kind) {
  | Lobby => "lobby"
  | Tattoo => "lobby3"
  | Cellar => "large_thumbnail"
  };

let bgScale = 1.;
let screenW = int_of_float(float_of_int(1205) *. bgScale);
let screenH = int_of_float(float_of_int(667) *. bgScale);

let setup = (spritesheetData, env) => {
  let fontPath = "assets/tragicmarker_2x.fnt";
  let spritesheetPath = "assets/spritesheet/sprites.png";
  Env.size(~width=screenW, ~height=screenH, env);
  let pos = Point.create(400., 400.);
  {
    position: pos,
    destination: pos,
    facingRight: false,
    destinationReachedTrigger: None,
    dialogOverTrigger: None,
    currentRoom: Lobby,
    rooms: createRooms(),
    dialog: [],
    dialogTimer: 0.,
    sprites:
      Sprite.create(
        Draw.loadImage(~filename=spritesheetPath, env),
        spritesheetData,
      ),
    font: Draw.loadFont(~filename=fontPath, env),
    mouse: {
      down: false,
      up: false,
      pressed: false,
      pos: Point.fromIntPair(Env.mouse(env)),
    },
    elapsedTime: 0.,
  };
};

let handleWorldMouse = (state, env) => {
  let mouse = state.mouse.pos;

  let rec processItems = (items, state) =>
    switch (items) {
    | [] => (false, state)
    | [{label, action, pos: {x, y}, width, height}, ...tl] =>
      if (mouse.x >= x
          && mouse.y >= y
          && mouse.x <= x
          +. width
          && mouse.y <= y
          +. height) {
        // In rect
        DrawStuff.actionLabel(label, mouse, state.font, env);
        if (state.mouse.up) {
          (true, action(state));
        } else {
          (true, state);
        };
      } else {
        // outside rect
        processItems(tl, state);
      }
    };

  let (hoveredItem, state) =
    processItems(getRoom(state.currentRoom, state.rooms), state);

  if (mouse.y > floorHeight
      && state.destination == state.position
      && !hoveredItem) {
    DrawStuff.actionLabel("walk", mouse, state.font, env);
  };

  if (state.mouse.up && mouse.y > floorHeight && !hoveredItem) {
    Action.walkToMouse(Action.print("Reached destination"), state);
  } else {
    state;
  };
};

let draw = (state, env) => {
  let state =
    if (Env.keyPressed(R, env)) {
      {...state, rooms: createRooms()};
    } else {
      state;
    };
  let state =
    if (Env.keyPressed(T, env)) {
      {
        ...state,
        currentRoom:
          switch (state.currentRoom) {
          | Lobby => Tattoo
          | Tattoo => Lobby
          | a => a
          },
      };
    } else {
      state;
    };
  Draw.background(Utils.color(~r=255, ~g=217, ~b=229, ~a=255), env);
  DrawStuff.sprite(
    state.sprites,
    getRoomAssetName(state.currentRoom),
    ~scale=bgScale,
    ~pos=
      Point.create(
        float_of_int(screenW) /. 2.,
        float_of_int(screenH) /. 2.,
      ),
    env,
  );

  if (debug) {
    List.iter(
      ({label, pos, width, height}) =>
        DrawStuff.placeholderUncentered(label, ~pos, ~width, ~height, env),
      getRoom(state.currentRoom, state.rooms),
    );
  };

  let spriteBody =
    if (state.position == state.destination) {
      "body_one_stand";
    } else {
      switch (int_of_float(state.elapsedTime /. 0.1) mod 4) {
      | 0 => "body_one_stand"
      | 1 => "body_one_walk_one"
      | 2 => "body_one_stand"
      | _ => "body_one_walk_two"
      };
    };
  DrawStuff.sprite(
    state.sprites,
    spriteBody,
    ~scale=0.4,
    ~flipped=state.facingRight,
    ~pos=state.position,
    env,
  );
  DrawStuff.sprite(
    state.sprites,
    "dude_head",
    ~scale=0.3,
    ~flipped=state.facingRight,
    ~pos=Point.(state.position + create(0., -100.)),
    env,
  );

  let state =
    switch (
      state.position == state.destination,
      state.destinationReachedTrigger,
    ) {
    | (true, Some(cb)) => cb({...state, destinationReachedTrigger: None})
    | (false, _) => state
    | _ => state
    };

  let dialogClick = (state, tl) =>
    if (state.mouse.up && state.dialogTimer <= dialogScrollTime) {
      {...state, dialogTimer: dialogScrollTime};
    } else if (state.mouse.up && tl == []) {
      let newState = {
        ...state,
        dialog: [],
        dialogTimer: 0.,
        dialogOverTrigger: None,
      };
      switch (state.dialogOverTrigger) {
      | None => newState
      | Some(action) => action(newState)
      };
    } else if (state.mouse.up) {
      {...state, dialog: tl, dialogTimer: 0.};
    } else {
      {...state, dialogTimer: state.dialogTimer +. Env.deltaTime(env)};
    };

  let state =
    switch (state.dialog) {
    | [] => handleWorldMouse(state, env)
    | [{assetName, label, character: Player, content}, ...tl] =>
      DrawStuff.Dialog.box(env);
      DrawStuff.Dialog.face(
        ~x=120.,
        assetName,
        label,
        state.sprites,
        state.font,
        env,
      );
      DrawStuff.Dialog.text(
        ~x=200.,
        content,
        state.dialogTimer /. dialogScrollTime,
        state.font,
        env,
      );
      if (state.dialogTimer > dialogScrollTime +. 1.) {
        DrawStuff.Dialog.next(
          state.dialogTimer -. dialogScrollTime -. 1.5,
          state.font,
          env,
        );
      };
      dialogClick(state, tl);
    | [{assetName, label, character: NPC, content}, ...tl] =>
      DrawStuff.Dialog.box(env);
      DrawStuff.Dialog.face(
        ~x=float_of_int(Env.width(env) - 120 - 10),
        assetName,
        label,
        state.sprites,
        state.font,
        env,
      );
      DrawStuff.Dialog.text(
        ~x=120.,
        content,
        state.dialogTimer /. dialogScrollTime,
        state.font,
        env,
      );
      if (state.dialogTimer > dialogScrollTime +. 1.) {
        DrawStuff.Dialog.next(
          state.dialogTimer -. dialogScrollTime -. 1.5,
          state.font,
          env,
        );
      };
      dialogClick(state, tl);
    };

  {
    ...state,
    facingRight:
      state.position.x < state.destination.x
      || state.position.x == state.destination.x
      && state.facingRight,
    position:
      Point.moveTo(
        state.position,
        ~dest=state.destination,
        ~speed=300. *. Env.deltaTime(env),
      ),
    mouse: {
      ...state.mouse,
      down: false,
      up: false,
    },
    elapsedTime: state.elapsedTime +. Env.deltaTime(env),
  };
};

let mouseDown = (state, _) => {
  ...state,
  mouse: {
    ...state.mouse,
    down: true,
    up: false,
    pressed: true,
  },
};
let mouseUp = (state, _) => {
  ...state,
  mouse: {
    ...state.mouse,
    down: false,
    up: true,
    pressed: false,
  },
};

let mouseMove = (state, env) => {
  ...state,
  mouse: {
    ...state.mouse,
    pos: Point.fromIntPair(Env.mouse(env)),
  },
};

/* let basedirname = Filename.dirname(Sys.argv[0]) ++ "/"; */
Assets.loadSpriteSheet("assets/spritesheet/sprites.json", assets =>
  run(~setup=setup(assets), ~draw, ~mouseDown, ~mouseUp, ~mouseMove, ())
);
