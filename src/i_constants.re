type gameStateT =
  | Running
  | Gameover
  | Restart
  | Pause;

type windowT = {
  h: int,
  w: int
};

type stateT = {
  gameTime: float,
  gameState: gameStateT,
  life: int,
  score: int,
  py: float,
  px: float,
  pvy: float,
  pvx: float,
  enemies: list((float,float)),
  pbullets: list((float,float)),
  ebullets: list((float,float)),
  enemySpawnTime: float,
  /* font: fontT */
};

let window = {
  h: 500,
  w: 700
};

let epoints = 100;
let bspd = 600.;
let ebspd = 500.;
let pspd = 400.;
let espd = 200.;
let eSpawnRate = 0.75;
let bSpawnRate = 0.005;