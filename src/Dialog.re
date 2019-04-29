module Helpers = {
  open Common;
  let character = (character, (label, assetName), content) => {
    assetName,
    label,
    character,
    content: [content],
  };

  let character' = (character, (label, assetName), content) => {
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
module People = {
  let president = ("President", "old_man_png");
  let boss = ("Boss", "boss_head");
  let bob = ("Bob", "dude");
  let bernard = ("Bernard", "dude_two_png");
  let lady = ("Cindy", "lady");
};
open Helpers;

let doorTest = {
  let you = player(People.bob);
  let you' = player'(People.bob);
  let door = npc(("Big Door", "dude"));
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

let doorLocked = [player(People.bob, "Looks like it's locked.")];
let shouldTalkToSomePeople = [
  player'(
    People.bob,
    ["I should probably talk to some", "people before exploring."],
  ),
];

let bannedDoor = [
  npc(People.bernard, "Don't go in there, please!"),
  player(People.bob, "Oops, sorry."),
];

let bannedDoorBernard = [
  npc(People.bernard, "We're not supposed to go in there."),
];

let broomCloset = char => [player(char, "That's just a broom closet.")];

let noPhone = [
  player'(People.bob, ["It's not ringing, and", "I already have my keys."]),
];
let timeToGoToWork = [player(People.bob, "Time to go to work!")];
let keyDoesntFit = [player(People.bob, "The key doesn't fit this lock.")];

let keysUnderPhone = [
  player(People.bob, "I'm going to need my keys."),
  player(People.bob, "I think I left them under the phone."),
];

let bossPhoneCall = {
  let you = player(People.bob);
  let boss = npc(People.boss);
  let boss' = npc'(People.boss);
  [
    you("Huh, I guess this was ringing this whole time."),
    you("Hello?"),
    boss'([
      "Hello Bob, It's your boss.",
      "I hope your day has been good so far",
    ]),
    you("Thanks sir, I've been catching up on some tiding up -- "),
    boss("... because something terrible has happened."),
    you("Oh no! What happened?!"),
    boss'([
      "The president has gone missing.",
      "All our best agents are on holiday so we need",
      "you to head over and check it out.",
    ]),
    you("Sure thing, boss, you can count on me!"),
  ];
};

let bernardHiToPresident = {
  let you = player(People.bernard);
  let president = npc(People.president);
  [you("Hi Mr. President!"), president("Hello Bernard.")];
};

let bobHiToPresident = {
  let you = player(People.bob);
  let you' = player'(People.bob);
  let president = npc(People.president);
  let president' = npc'(People.president);
  [
    you("Uh, hello Mr. President, how are you?"),
    president("Never better!"),
    president'([
      "Though now that you mention it",
      "I am feeling a little thirsty.",
    ]),
    you'([
      "Excuse me for asking, but aren't you",
      "supposed to be kidnapped or something?",
    ]),
    you("I'm here to investigate your disappearance..."),
    president'([
      "Ah yes! It's a little embarrassing",
      "but actually I fell asleep watching TV",
      "and missed a few important meetings.",
    ]),
    you("That's understandable."),
    president'([
      "Now that you're here, can you go ask",
      "Bernard to get some champagne to celebrate",
      "my 'reappearance'?",
    ]),
    you("That's not really my job..."),
    president("Thanks!"),
  ];
};

let giveChampagneToPresident = {
  let you = player(People.bernard);
  /* let you' = player'(People.bob); */
  /* let president = npc(People.president); */
  /* let president' = npc'(People.president); */
  [you("Uh, hello Mr. President, here's some champage?")];
};

let bernardGoGetChampagne = {
  let you = player(People.bob);
  let you' = player'(People.bob);
  let bernard = npc(People.bernard);
  let bernard' = npc'(People.bernard);
  [
    bernard("Thank goodness you're here!"),
    you("Is the president ok?"),
    bernard("Ah yes, we found him."),
    you'([
      "Ok, well it seems like there's not much",
      "for me to investigate here, so I'll be heading back.",
    ]),
    bernard("Yes, so sorry about the inconvenience."),
    bernard'([
      "If you'll excuse me, I have to go",
      "get the fancy champagne.",
    ]),
    you("..."),
    you("Did you say fancy champagne?"),
    bernard("Yes, we're celebrating the president's return."),
    you("I'm a bit of a champagne enthusiast actually..."),
    bernard("*sigh*"),
    bernard'([
      "I guess you can have some if you help",
      "me out by getting it from the cellar.",
    ]),
    you("Great! Be back in a second!"),
  ];
};

let (bernardSwapExplanation1, bernardSwapExplanation2, bernardBeforeSwap) = {
  let you = player(People.bob);
  let bernard = npc(People.bernard);
  let bernard' = npc'(People.bernard);
  (
    [
      you("Looks like I'm not allowed in the cellar."),
      bernard("Oh right, sorry."),
      bernard'(["I'm very busy, so we'll have to swap."]),
      you("... Swap?"),
      bernard("Ah, I forgot you're a private detective."),
      bernard'([
        "We have top-secret technology that lets us",
        "swap bodies.",
      ]),
      you("You... what?"),
      bernard'([
        "Yeah, I know, right?",
        "It comes in super handy for",
        "last-minute presidential speeches.",
      ]),
      you("..."),
      bernard("Anyway, here, take this one for now."),
    ],
    [
      you("Thanks!"),
      bernard'([
        "Now just use it on me and you can",
        "go down to the cellar to get the",
        "champagne.",
      ]),
      you("Ok!"),
    ],
    [
      bernard'(["Just err.. 'click' the device", "then 'click' me."]),
      bernard("Whatever that means."),
    ],
  );
};

let cellarDoorNo = [
  player'(
    People.bob,
    ["Only government employees are allowed", "down to the cellar."],
  ),
  player(People.bob, "Bernard didn't warn me about this!"),
  player(People.bob, "I should go ask him about it."),
];

let paintingDoor = char => [
  player(char, "I think this is just a painting of a door."),
];

let lockedDoor = char => [player(char, "It's locked.")];

module Cellar = {
  let bottle1 = [
    player'(
      People.bernard,
      [
        "Ooh! Look at all these Cabernet Sauvignons!",
        "Last year sure was a good year!",
      ],
    ),
  ];

  let bottle2 = [
    player(People.bernard, "The president really seems to like rose."),
  ];

  let bottle3 = [
    player'(People.bernard, ["Is that ... ?", "Nope, that's just rose."]),
  ];

  let bottle4 = [
    player'(
      People.bernard,
      [
        "Mmmm that's prosecco, not champagne.",
        "Although who even knows what the difference is.",
      ],
    ),
  ];

  let bottle5 = [
    player(People.bernard, "My mom really likes this Pinot Grigio!"),
  ];

  let bottle6 = [player(People.bernard, "A ... huge case of tequila?")];

  let bottle7 = [player(People.bernard, "That's just grape juice.")];

  let bottle8 = [
    player(People.bernard, "Looks like a few dozen bottles of Jagermeister."),
  ];

  let bottle9 = [
    player(People.bernard, "What a beautiful assortment of Spanish Riojas!"),
  ];

  let bottle10 = [
    player(
      People.bernard,
      "Wouldn't mind grabbing a quick sip of this Syrah!",
    ),
  ];

  let bottle11 = [
    player'(
      People.bernard,
      ["This Merlot looks almost as old", "as the president himself."],
    ),
  ];

  let champagne = [
    player'(
      People.bernard,
      [
        "Aha! Champagne! Aged 400 years, amazing.",
        "I'll take this one with me.",
      ],
    ),
  ];
};

let bobHiToLady = [
  npc'(People.lady, ["Sorry, I can't talk.", "I'm too busy celebrating."]),
  player(People.bob, "Busy celebrating?"),
  npc(People.lady, "Can't talk."),
];

let bernardHiToLady = {
  let lady = npc(People.lady);
  let lady' = npc'(People.lady);
  let you = player(People.bernard);
  [
    lady("Oh, Bernard!"),
    lady("I've been looking for you!"),
    you("...You have?"),
    lady'([
      "Yes, I asked you to get that report",
      "on my desk by last Friday",
      "and I haven't seen anything.",
    ]),
    you("..."),
    lady("What's the status of that?"),
    you("Coming right up?"),
  ];
};

let shouldSwapBack = [
  player(People.bernard, "I should probably swap back first."),
];

let swapPresident = char => [player(char, "That seems pretty risky.")];

let stillHere = [npc(People.bernard, "You're still here?")];

let presidentBye = {
  let bob = player(People.bob);
  let president = npc(People.president);
  let president' = npc'(People.president);
  [
    president("Hi Bob. Thanks for all your help today."),
    bob("Anytime, Mr. President!"),
    president'([
      "I'm sorry we couldn't make more use",
      "of all your talents.",
    ]),
    president("We just ran out of time in the end!"),
    president("Maybe next time!"),
  ];
};

let hurryUp = [
  npc'(People.bob, ["What are you waiting for?", "Head to the cellar!"]),
];

let bernardSwapBackDidntTalkAboutTattoo = [
  player'(
    People.bernard,
    ["I've secured the champagne!", "Ready to swap back?"],
  ),
  npc'(People.bob, ["Absolutely!", "What do you think of my new tattoo?"]),
  player(People.bernard, "..."),
  npc(People.bob, "You dont like it?"),
  player(People.bernard, "No, no, it's ... really something."),
  npc(People.bob, "Thanks! I thought you might like it!"),
  player(People.bernard, "I'd like my body back, now."),
  npc(People.bob, "Alright, alright."),
];

let bernardTalkBeforeSwapBack = [
  npc'(
    People.bob,
    ["Great! You found the champagne!", "Ready to swap back?"],
  ),
  player'(
    People.bernard,
    ["Absolutely!", "What did you get up to while I was gone?"],
  ),
  npc'(
    People.bob,
    [
      "Oh I've been thinking about getting",
      "a tattoo, but I wasn't sure so",
      "I was finally able to test it out.",
    ],
  ),
  player(People.bernard, "..."),
  npc(People.bob, "You dont like it?"),
  player(People.bernard, "No, no, it's ... really something."),
  npc(People.bob, "Thanks! I thought you might like it!"),
  player(People.bernard, "Can we swap back, now?"),
  npc(People.bob, "Of course, of course."),
];

let bernardAfterSwap = [
  npc(People.bernard, "Great, thanks for swapping back."),
  npc(People.bernard, "*hands you a glass of champagne*"),
  npc(People.bernard, "Thanks for getting that for me!"),
  player'(
    People.bob,
    ["I'm just a phone call away", "next time you need anything!"],
  ),
  npc(People.bernard, "Mhm."),
  player(People.bob, "Here's my card in case you ..."),
  npc(People.bernard, "We'll call you if we need you."),
  player(People.bob, "Bye!"),
];
