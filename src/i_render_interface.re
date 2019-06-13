open Reprocessing;
open I_constants;

let draw_score = (score, env) => {
  Draw.fill(Utils.color(~r=218, ~g=218, ~b=218, ~a=255), env);
  Draw.rectf(~pos=(0.,10.), ~width=150., ~height=40., env);
  Draw.text(~body=string_of_int(score), ~pos=(3, 15), env);
};
  
let draw_pause = env => {
  Draw.fill(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
  Draw.rectf(~pos=(float_of_int(window.w) *. 0.5 -. 75., float_of_int(window.h) *. 0.2), ~width=150., ~height=50., env);
  Draw.text(~body="PAUSED", ~pos=(int_of_float(float_of_int(window.w) *. 0.5 -. 62.), int_of_float(float_of_int(window.h) *. 0.2 +. 10.)), env);
}; 

let draw_gameover = env => {
  Draw.fill(Utils.color(~r=229, ~g=46, ~b=46, ~a=255), env);
  Draw.rectf(~pos=(float_of_int(window.w) *. 0.5 -. 125., float_of_int(window.h) *. 0.4), ~width=250., ~height=50., env);
  Draw.text(~body="GAME OVER", ~pos=(int_of_float(float_of_int(window.w) *. 0.5 -. 98.), int_of_float(float_of_int(window.h) *. 0.4 +. 10.)), env);
};

let draw_heart = ((x,y), env) => {
  Draw.fill(Utils.color(~r=229, ~g=46, ~b=46, ~a=255), env);
  Draw.ellipsef(~center = (x, y),
                ~radx = 10.,
                ~rady = 10.,
                env);
  /* Draw.fill(Utils.color(~r=255, ~g=0, ~b=0, ~a=255), env); */
  Draw.ellipsef(~center = (x +. 19., y),
                ~radx = 10.,
                ~rady = 10.,
                env);
  /* Draw.fill(Utils.color(~r=0, ~g=255, ~b=0, ~a=255), env); */
  Draw.ellipsef(~center = (x +. 10., y +. 5.),
                ~radx = 5.,
                ~rady = 5.,
                env);
  /* Draw.fill(Utils.color(~r=0, ~g=0, ~b=255, ~a=255), env); */
  Draw.trianglef(~p1 = (x +. 10., y +. 23.),
                  ~p2 = (x -. 7., y +. 7.),
                  ~p3 = (x +. 26., y +. 7.),
                  env);
};

let draw_lives = (life, env) => {
  let x = float_of_int(window.w) -. 40.;
  let y = 20.;
  for(i in 0 to life-1) {
    draw_heart((x -. (50. *. float_of_int(i)),y), env);
  }
}
