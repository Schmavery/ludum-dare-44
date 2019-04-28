module Helpers = {
  open Common;
  let character = (character, label, assetName, content) => {
    assetName,
    label,
    character,
    content: [content],
  };

  let character' = (character, label, assetName, content) => {
    assetName,
    label,
    character,
    content,
  };

  let player = character(Player);
  let player' = character'(Player);
  let npc = character(NPC);
  let npc' = character'(NPC);
};
open Helpers;

let doorTest = {
  let you = player("Bob", "dude_head");
  let you' = player'("Bob", "dude_head");
  let door = npc("Big Door", "dude_head");
  [
    you("Hey door."),
    door("Hi!"),
    you'([
      "Wow, I wasn't expecting you to actually respond...",
      "Is that normal?",
    ]),
    door("..."),
    you("Also, uh.. why do you have my face?"),
    door("..."),
    you("..."),
  ];
};

let doorLocked = [player("Bob", "dude_head", "Looks like it's locked.")];
