A Little Case of Presidential Disappearance
---
This is a game made for Ludum Dare 44. The theme was "Your life is currency".

https://ldjam.com/events/ludum-dare/44/a-little-case-of-presidential-disappearance

Play now!
---

Click here to play: 
https://schmavery.github.io/ludum-dare-44/

More Info
---
This game was build using [Reprocessing](https://github.com/schmavery/reprocessing), a 2D graphics library we wrote in ReasonML/OCaml, with an api inspired by Processing.

See [here](https://github.com/schmavery/reprocessing#projects-using-reprocessing) for more examples of games and demos built in Reprocessing.


### Build
```
git clone https://github.com/Schmavery/ludum-dare-44.git
npm install
npm run build
```

### Start
```
npm start
```

To build to JS run `npm run build:web` and then run a static server (like `python -m SimpleHTTPServer`) and go to `localhost:8000`. If you're using safari you can simply open the `index.html` and tick `Develop > Disable Cross-Origin Restrictions`.

To build to native run `npm run build:native` and run `npm run start:native`

The build system used is [bsb-native](https://github.com/bsansouci/bucklescript).
