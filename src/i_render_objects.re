open Reprocessing;

let draw_player = ((x,y), env) => {
  Draw.fill(Utils.color(~r=218, ~g=218, ~b=218, ~a=255), env);
  Draw.ellipsef(~center = (x, y),
                  ~radx = 25.,
                  ~rady = 25.,
                  env);
};

let draw_enemies = (enemies, env) => {
  Draw.fill(Utils.color(~r=192, ~g=6, ~b=6, ~a=255), env);
  List.iter(
      ((x,y)) => {
      Draw.rectf(~pos = (x,y),
                  ~width = 60.,
                  ~height = 25.,
                  env);
      },
      enemies
  );
};

let draw_pbullet = (bullets, env) => {
  Draw.fill(Utils.color(~r=218, ~g=218, ~b=218, ~a=255), env);
  List.iter(
      ((x,y)) => {
      Draw.rectf(~pos=(x -. 5.,y), ~width=10., ~height=15., env);
      },
      bullets
  );
};

let draw_ebullet = (bullets, env) => {
  Draw.fill(Utils.color(~r=192, ~g=6, ~b=6, ~a=255), env);
  List.iter(
      ((x,y)) => {
      Draw.rectf(~pos=(x +. 25., y), ~width=10., ~height=15., env);
      },
      bullets
  );
};
