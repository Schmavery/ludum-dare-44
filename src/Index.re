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
  | Office
  | Lobby
  | Hallway
  | Cellar;

type itemKind =
  | President
  | Bottles
  | Bernard
  | Bob
  | Person
  | Keys
  | Champagne
  | Device
  | Door
  | Other
  | Phone;

type state = {
  position: Point.t,
  destination: Point.t,
  facingRight: bool,
  destinationReachedTrigger: option(state => state),
  flags: StringMap.t(unit),
  isBob: bool,
  sprites: Sprite.t,
  font: fontT,
  currentRoom: roomKind,
  mouse,
  rooms,
  elapsedTime: float,
  holding: option(inventoryItem),
  inventory: list(inventoryItem),
  altInventory: list(inventoryItem),
  dialog: list(dialogItem),
  dialogTimer: float,
  dialogOverTrigger: option(state => state),
}
and inventoryItem = {
  kind: itemKind,
  label: string,
  img: string,
  mutable useWith: (itemKind, state) => option((string, state => state)),
}
and worldItem = {
  kind: itemKind,
  pos: Point.t,
  width: float,
  height: float,
  actionLabel: string,
  img: option(string),
  action: state => state,
}
and room = list(worldItem)
and rooms = {
  office: room,
  lobby: room,
  cellar: room,
  hallway: room,
};

let floorHeight = 300.;
let inventoryPanelHeight = 140.;
let dialogScrollTime = 0.7;

let bgScale = 0.75;
let bgWidth = int_of_float(float_of_int(1205) *. bgScale);
let bgHeight = int_of_float(float_of_int(667) *. bgScale);

let getRoom = (kind, rooms) =>
  switch (kind) {
  | Office => rooms.office
  | Lobby => rooms.lobby
  | Cellar => rooms.cellar
  | Hallway => rooms.hallway
  };

let updateRoomList = (kind, update, rooms) => {
  switch (kind) {
  | Office => {...rooms, office: update(rooms.office)}
  | Lobby => {...rooms, lobby: update(rooms.lobby)}
  | Cellar => {...rooms, cellar: update(rooms.cellar)}
  | Hallway => {...rooms, hallway: update(rooms.hallway)}
  };
};

let removeItemFromRoom = (kind, itemKind, rooms) => {
  updateRoomList(
    kind,
    List.filter((i: worldItem) => i.kind != itemKind),
    rooms,
  );
};

let addItemToRoom = (kind, item, rooms) => {
  updateRoomList(kind, l => [item, ...l], rooms);
};

let getRoomAssetName = kind =>
  switch (kind) {
  | Office => "office"
  | Lobby => "lobby"
  | Cellar => "cellar"
  | Hallway => "hallway"
  };

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
      Point.{
        x: state.mouse.pos.x,
        y: min(max(floorHeight, state.mouse.pos.y), float_of_int(bgHeight)),
      };
    {...state, destination, destinationReachedTrigger: Some(cb)};
  };
  let dialog = (dialog, cb, state) => {
    ...state,
    dialog,
    dialogTimer: 0.,
    dialogOverTrigger: Some(cb),
  };
  let charDialog = (dialogItem, cb, state) => {
    dialog(
      dialogItem(state.isBob ? Dialog.People.bob : Dialog.People.bernard),
      cb,
      state,
    );
  };
  /* TODO: Support from "from" pos to have the person walk onto the next scene */
  let changeRoom = (~pos=?, room, cb, state) =>
    cb(
      switch (pos) {
      | None => {...state, currentRoom: room}
      | Some(p) => {...state, position: p, destination: p, currentRoom: room}
      },
    );
  let addToInventory = (item, cb, state) =>
    cb({...state, inventory: [item, ...state.inventory]});
  let removeFromInventory = (item, cb, state) =>
    cb({
      ...state,
      inventory:
        List.filter((i: inventoryItem) => i.kind != item, state.inventory),
    });
  let removeFromRoom = (kind, cb, state) =>
    cb({
      ...state,
      rooms: removeItemFromRoom(state.currentRoom, kind, state.rooms),
    });
  let addToRoom = (item, cb, state) =>
    cb({
      ...state,
      rooms: addItemToRoom(state.currentRoom, item, state.rooms),
    });
  let ifHaveItem = (itemKind, first, second, state) =>
    if (List.exists(
          (i: inventoryItem) => i.kind == itemKind,
          state.inventory,
        )) {
      first(state);
    } else {
      second(state);
    };
  let ifFlag = (flag, first, second, state) =>
    switch (StringMap.find(flag, state.flags)) {
    | () => first(state)
    | exception Not_found => second(state)
    };
  let switchFlag = (lst, ~default, state) => {
    switch (
      List.find(((flag, _)) => StringMap.mem(flag, state.flags), lst)
    ) {
    | (_, action) => action(state)
    | exception Not_found => default(state)
    };
  };
  let ifBob = (first, second, state) =>
    state.isBob ? first(state) : second(state);
  let flag = (flag, cb, state) =>
    cb({...state, flags: StringMap.add(flag, (), state.flags)});
  let swap = (cb, state) => {
    let (inventory, altInventory) = (state.altInventory, state.inventory);
    cb({...state, isBob: !state.isBob, inventory, altInventory});
  };
};

module Items = {
  open Action;
  let houseKeys: inventoryItem = {
    kind: Keys,
    label: "House keys",
    img: "keys_png",
    useWith: (itemKind, state) =>
      switch (itemKind, state.currentRoom) {
      | (Door, Office) =>
        Some((
          "unlock",
          walkToMouse @@
          dialog(Dialog.timeToGoToWork) @@
          changeRoom(Lobby) @@
          empty,
        ))
      | (Door, _) =>
        Some(("unlock", walkToMouse @@ dialog(Dialog.keyDoesntFit) @@ empty))
      | _ => None
      },
  };
  /* TODO: Hack for recursion? */
  let device: inventoryItem = {
    kind: Device,
    label: "Swapping device",
    img: "device",
    useWith: (_, _) => None,
  };
  let bernard = {
    kind: Bernard,
    pos: Point.create(730., 200.),
    width: 100.,
    height: 220.,
    actionLabel: "talk",
    img: Some("dude_two_full_body"),
    action:
      walkToMouse @@
      switchFlag(
        [
          (
            "finished_game",
            dialog(Dialog.bernardTalkToBobAfterSwap) @@ empty,
          ),
          ("got_device", dialog(Dialog.bernardBeforeSwap) @@ empty),
          (
            "tried_cellar",
            dialog(Dialog.bernardSwapExplanation1) @@
            addToInventory(device) @@
            dialog(Dialog.bernardSwapExplanation2) @@
            flag("got_device") @@
            empty,
          ),
        ],
        ~default=
          dialog(Dialog.bernardGoGetChampagne) @@
          flag("spoke_to_bernard") @@
          empty,
      ),
  };
  let bob = {
    kind: Bob,
    pos: Point.create(730., 200.),
    width: 100.,
    height: 220.,
    actionLabel: "talk",
    img: Some("main_dude_full_body"),
    action:
      Action.(
        walkToMouse @@
        ifFlag(
          "got_champagne",
          dialog(Dialog.bernardTalkToBobBeforeSwap) @@ empty,
          dialog(Dialog.hurryUp) @@ empty,
        )
      ),
  };
  device.useWith = (
    (itemKind, state) =>
      switch (itemKind) {
      | President =>
        Some(("swap", charDialog(Dialog.swapPresident) @@ empty))
      | Bernard =>
        Some((
          "swap",
          walkToMouse @@
          swap @@
          removeFromRoom(Bernard) @@
          addToRoom(bob) @@
          empty,
        ))
      | Bob =>
        Some((
          "swap",
          walkToMouse @@
          swap @@
          removeFromInventory(Device) @@
          flag("finished_game") @@
          removeFromRoom(Bob) @@
          addToRoom(bernard) @@
          dialog(Dialog.bernardSwapWithBob) @@
          empty,
        ))
      | _ => None
      }
  );
  let champagne: inventoryItem = {
    kind: Champagne,
    label: "Champagne",
    img: "champagne_png",
    useWith: (itemKind, state) =>
      switch (itemKind, state.currentRoom) {
      | (President, Lobby) =>
        Some((
          "give",
          walkToMouse @@
          dialog(Dialog.giveChampagneToPresident) @@
          removeFromInventory(Champagne) @@
          empty,
        ))
      | _ => None
      },
  };
};

let createRooms = () => {
  Action.{
    office: [
      {
        kind: Door,
        pos: Point.create(90., 230.),
        width: 100.,
        height: 220.,
        actionLabel: "open",
        img: None,
        action:
          walkToMouse(
            ifHaveItem(
              Keys,
              dialog(Dialog.doorLocked, empty),
              dialog(Dialog.keysUnderPhone, empty),
            ),
          ),
      },
      {
        kind: Phone,
        pos: Point.create(450., 265.),
        width: 70.,
        height: 70.,
        actionLabel: "answer",
        img: None,
        action:
          walkToMouse @@
          ifFlag(
            "talked_to_boss",
            dialog(Dialog.noPhone, empty),
            dialog(Dialog.bossPhoneCall) @@
            addToInventory(Items.houseKeys) @@
            flag("talked_to_boss") @@
            empty,
          ),
      },
    ],
    lobby: [
      /* Yeah yeah it's not an item I know */
      Items.bernard,
      {
        kind: President,
        pos: Point.create(340., 170.),
        width: 100.,
        height: 220.,
        actionLabel: "talk",
        img: Some("old_man_full_body"),
        action:
          walkToMouse @@
          ifBob(
            dialog(Dialog.bobHiToPresident) @@ empty,
            dialog(Dialog.bernardHiToPresident) @@ empty,
          ),
      },
      /* TODO: Fix for bernard */
      {
        kind: Door,
        pos: Point.create(120., 150.),
        width: 100.,
        height: 220.,
        actionLabel: "open",
        img: None,
        action:
          walkToMouse @@
          ifBob(
            dialog(Dialog.bannedDoor) @@ empty,
            dialog(Dialog.bannedDoorBernard) @@ empty,
          ),
      },
      {
        kind: Door,
        pos: Point.create(600., 100.),
        width: 130.,
        height: 240.,
        actionLabel: "enter",
        img: None,
        action:
          ifFlag(
            "spoke_to_bernard",
            walkToMouse @@
            changeRoom(~pos=Point.create(520., 250.), Hallway) @@
            empty,
            dialog(Dialog.shouldTalkToSomePeople) @@ empty,
          ),
      },
      {
        kind: Door,
        pos: Point.create(475., 195.),
        width: 70.,
        height: 140.,
        actionLabel: "open",
        img: None,
        action: walkToMouse @@ charDialog(Dialog.broomCloset) @@ empty,
      },
    ],
    hallway: [
      {
        kind: Other,
        pos: Point.create(450., 130.),
        width: 90.,
        height: 120.,
        actionLabel: "return",
        img: None,
        action:
          walkToMouse @@
          changeRoom(~pos=Point.create(620., 300.), Lobby) @@
          empty,
      },
      {
        kind: Other,
        pos: Point.create(285., 60.),
        width: 40.,
        height: 220.,
        actionLabel: "go down",
        img: None,
        action:
          walkToMouse @@
          ifBob(
            dialog(Dialog.cellarDoorNo) @@ flag("tried_cellar") @@ empty,
            changeRoom(~pos=Point.create(150., 400.), Cellar) @@ empty,
          ),
      },
      {
        kind: Other,
        pos: Point.create(355., 135.),
        width: 40.,
        height: 120.,
        actionLabel: "open",
        img: None,
        action: walkToMouse @@ charDialog(Dialog.paintingDoor) @@ empty,
      },
      {
        kind: Door,
        pos: Point.create(120., 50.),
        width: 100.,
        height: 290.,
        actionLabel: "open",
        img: None,
        action: walkToMouse @@ charDialog(Dialog.lockedDoor) @@ empty,
      },
    ],
    cellar: [
      {
        kind: Door,
        pos: Point.create(80., 190.),
        width: 100.,
        height: 260.,
        actionLabel: "return",
        img: None,
        action:
          walkToMouse @@
          changeRoom(~pos=Point.create(320., 270.), Hallway) @@
          empty,
      },
      {
        kind: Door,
        pos: Point.create(760., 290.),
        width: 70.,
        height: 170.,
        actionLabel: "open",
        img: None,
        action: walkToMouse @@ charDialog(Dialog.lockedDoor) @@ empty,
      },
      {
        kind: Champagne,
        pos: Point.create(430., 175.),
        width: 50.,
        height: 110.,
        actionLabel: "get",
        img: Some("champagne_png"),
        action:
          walkToMouse @@
          dialog(Dialog.Cellar.champagne) @@
          flag("got_champagne") @@
          addToInventory(Items.champagne) @@
          removeFromRoom(Champagne) @@
          empty,
      },
      {
        kind: Bottles,
        pos: Point.create(560., 160.),
        width: 70.,
        height: 200.,
        actionLabel: "inspect",
        img: None,
        action: dialog(Dialog.Cellar.bottle1) @@ empty,
      },
      {
        kind: Bottles,
        pos: Point.create(290., 170.),
        width: 60.,
        height: 190.,
        actionLabel: "inspect",
        img: None,
        action: dialog(Dialog.Cellar.bottle4) @@ empty,
      },
      {
        kind: Bottles,
        pos: Point.create(375., 165.),
        width: 50.,
        height: 105.,
        actionLabel: "inspect",
        img: None,
        action: dialog(Dialog.Cellar.bottle5) @@ empty,
      },
      {
        kind: Bottles,
        pos: Point.create(485., 165.),
        width: 50.,
        height: 105.,
        actionLabel: "inspect",
        img: None,
        action: dialog(Dialog.Cellar.bottle6) @@ empty,
      },
      {
        kind: Bottles,
        pos: Point.create(370., 290.),
        width: 170.,
        height: 70.,
        actionLabel: "inspect",
        img: None,
        action: dialog(Dialog.Cellar.bottle7) @@ empty,
      },
    ],
  };
};

let setup = (spritesheetData, env) => {
  let fontPath = "assets/tragicmarker_2x.fnt";
  let spritesheetPath = "assets/spritesheet/sprites.png";
  Env.size(
    ~width=bgWidth,
    ~height=bgHeight + int_of_float(inventoryPanelHeight),
    env,
  );
  let pos = Point.create(400., 400.);
  {
    position: pos,
    destination: pos,
    facingRight: false,
    destinationReachedTrigger: None,
    flags: StringMap.empty,
    isBob: true,
    dialogOverTrigger: None,
    currentRoom: Office,
    rooms: createRooms(),
    inventory: [],
    altInventory: [Items.device],
    holding: None,
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

  let rec processItems = (items, held, state) =>
    switch (items) {
    | [] => (false, state)
    | [{actionLabel, kind, action, pos: {x, y}, width, height}, ...tl] =>
      if (mouse.x >= x
          && mouse.y >= y
          && mouse.x <= x
          +. width
          && mouse.y <= y
          +. height) {
        // In rect
        switch (held) {
        | Some(item) =>
          switch (item.useWith(kind, state)) {
          | None => (true, state)
          | Some((label, action)) =>
            DrawStuff.actionLabel(label, mouse, state.font, env);
            (true, state.mouse.up ? action(state) : state);
          }
        | None =>
          DrawStuff.actionLabel(actionLabel, mouse, state.font, env);
          if (state.mouse.up) {
            (true, action(state));
          } else {
            (true, state);
          };
        };
      } else {
        // outside rect
        processItems(tl, held, state);
      }
    };

  let (hoveredItem, state) =
    processItems(
      getRoom(state.currentRoom, state.rooms),
      state.holding,
      state,
    );

  let state =
    if (mouse.y > floorHeight && !hoveredItem && state.holding == None) {
      if (state.destination == state.position) {
        DrawStuff.actionLabel("walk", mouse, state.font, env);
      };
      state.mouse.up ? Action.walkToMouse(Action.empty, state) : state;
    } else {
      state;
    };

  state.mouse.up ? {...state, holding: None} : state;
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
      {...state, currentRoom: Cellar, isBob: false};
    } else {
      state;
    };
  Draw.background(Constants.white, env);
  DrawStuff.sprite(
    state.sprites,
    getRoomAssetName(state.currentRoom),
    ~scale=bgScale,
    ~pos=
      Point.create(
        float_of_int(bgWidth) /. 2.,
        float_of_int(bgHeight) /. 2.,
      ),
    env,
  );

  List.iter(
    ({actionLabel, img, pos, width, height}) => {
      if (debug) {
        DrawStuff.placeholderUncentered(
          actionLabel,
          ~pos,
          ~width,
          ~height,
          env,
        );
      };
      switch (img) {
      | Some(assetName) =>
        DrawStuff.spriteUncentered(
          state.sprites,
          ~scale=0.7,
          assetName,
          ~pos,
          env,
        )
      | None => ()
      };
    },
    getRoom(state.currentRoom, state.rooms),
  );

  let num = state.isBob ? "one" : "two";
  let spriteBody =
    if (state.position == state.destination) {
      "body_" ++ num ++ "_stand";
    } else {
      switch (int_of_float(state.elapsedTime /. 0.1) mod 4) {
      | 0 => "body_" ++ num ++ "_stand"
      | 1 => "body_" ++ num ++ "_walk_one"
      | 2 => "body_" ++ num ++ "_stand"
      | _ => "body_" ++ num ++ "_walk_two"
      };
    };
  DrawStuff.sprite(
    state.sprites,
    spriteBody,
    ~scale=0.8,
    ~flipped=state.facingRight,
    ~pos=state.position,
    env,
  );
  DrawStuff.sprite(
    state.sprites,
    state.isBob ? "dude" : "dude_two_png",
    ~scale=0.6,
    ~flipped=state.facingRight,
    ~pos=Point.(state.position + create(0., -100.)),
    env,
  );

  // Draw inventory
  let invSquareSize = 80.;
  let invSlots = 8;
  let invSlotsf = float_of_int(invSlots);
  let invPadding =
    (float_of_int(Env.width(env)) -. invSquareSize *. invSlotsf)
    /. (invSlotsf +. 1.);
  let invTop = float_of_int(Env.height(env)) -. inventoryPanelHeight;
  let invItemTop = invTop +. invPadding;
  Draw.pushStyle(env);
  Draw.fill(Constants.white, env);
  Draw.stroke(Constants.black, env);
  Draw.strokeWeight(4, env);
  Draw.rectf(
    ~pos=((-5.), invTop),
    ~height=inventoryPanelHeight,
    ~width=float_of_int(Env.width(env)) +. 10.,
    env,
  );
  for (i in 0 to invSlots - 1) {
    Draw.rectf(
      ~pos=(
        float_of_int(i) *. (invSquareSize +. invPadding) +. invPadding,
        invItemTop,
      ),
      ~height=invSquareSize,
      ~width=invSquareSize,
      env,
    );
  };
  Draw.popStyle(env);

  List.iteri(
    (i, {img}) => {
      let pos =
        Point.create(
          float_of_int(i)
          *. (invSquareSize +. invPadding)
          +. invPadding
          +. invSquareSize
          /. 2.,
          invItemTop +. invSquareSize /. 2.,
        );
      DrawStuff.sprite(state.sprites, ~scale=0.5, img, ~pos, env);
    },
    state.inventory,
  );

  switch (state.holding) {
  | None => ()
  | Some({img}) =>
    DrawStuff.sprite(
      state.sprites,
      ~scale=0.5,
      img,
      ~pos=state.mouse.pos,
      env,
    )
  };

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

  let rec hoveringInventoryItem =
          (mouse: Point.t, i, items): option(inventoryItem) =>
    switch (items) {
    | [] => None
    | [item, ...rest] =>
      let rectPos =
        Point.create(
          float_of_int(i) *. (invSquareSize +. invPadding) +. invPadding,
          invItemTop,
        );
      if (mouse.x >= rectPos.x
          && mouse.y >= rectPos.y
          && mouse.x <= rectPos.x
          +. invSquareSize
          && mouse.y <= rectPos.y
          +. invSquareSize) {
        Some(item);
      } else {
        hoveringInventoryItem(mouse, i + 1, rest);
      };
    };

  let state =
    switch (
      state.mouse.pos.y > float_of_int(Env.height(env))
      -. inventoryPanelHeight,
      state.dialog,
    ) {
    | (false, []) => handleWorldMouse(state, env)
    | (true, []) =>
      // hovering inventory
      switch (hoveringInventoryItem(state.mouse.pos, 0, state.inventory)) {
      | Some(item) =>
        DrawStuff.actionLabel(item.label, state.mouse.pos, state.font, env);
        state.mouse.up ? {...state, holding: Some(item)} : state;
      | None => state
      }
    | (_, [{assetName, label, character: Player, content}, ...tl]) =>
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
    | (_, [{assetName, label, character: NPC, content}, ...tl]) =>
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
