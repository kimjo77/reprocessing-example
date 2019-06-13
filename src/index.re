open Reprocessing;
open I_constants;
open I_render_objects;
open I_render_interface;

let add_enemy = enemies => {
  /* print_endline("Spawned Enemy!"); */
  /* print_endline(string_of_int(List.length(enemies))); */
  let newEnemy = [(
                    Random.float(float_of_int(window.w)), 
                    Random.float(float_of_int(-window.h / 2))
                 )];
  enemies @ newEnemy;
};

let rec spawnBullet = enemies => {
  switch(enemies) {
  | [] => []
  | [head, ...tail] => {
      ((Random.float(1.) <= bSpawnRate)? [head] : []) @ spawnBullet(tail)
    }
  }
};

let shoot = (x,y,bullets) => {
  let newBullet = [(x,y)];
  bullets @ newBullet;
};

let remove_enemy_out_of_bounds = enemies => {
  List.filter(
    ((_,y)) => {
      (y < float_of_int(window.h + 100))? true: false;
    },
    enemies
  );
};

let remove_pbullet_out_of_bounds = bullets => {
  List.filter(
    ((_,y)) => {
      (y > -100.)? true: false;
    },
    bullets
  );
};

let remove_ebullet_out_of_bounds = bullets => {
  List.filter(
    ((_,y)) => {
      (y < float_of_int(window.h + 100))? true: false;
    },
    bullets
  );
};

let collision_pbullet_and_enemy = ((bx,by), (ex,ey)) => {
  Utils.intersectRectRect(
    ~rect1Pos = (bx -. 5., by),
    ~rect1W = 10.,
    ~rect1H = 15.,
    ~rect2Pos = (ex, ey),
    ~rect2W = 60.,
    ~rect2H = 25.
  );
};

let collision_enemy_and_pbullet = ((ex,ey), (bx,by)) => {
  Utils.intersectRectRect(
    ~rect1Pos = (bx -. 5., by),
    ~rect1W = 10.,
    ~rect1H = 15.,
    ~rect2Pos = (ex, ey),
    ~rect2W = 60.,
    ~rect2H = 25.
  );
};

let collision_player_ebullet = ((bx,by),(px,py)) => {
  Utils.intersectRectCircle(
    ~rectPos = (bx +. 25., by),
    ~rectW = 10.,
    ~rectH = 15.,
    ~circlePos = (px, py),
    ~circleRad = 25.
  );
};

let remove_pbullet_enemy_collisions = (bullets, enemies) => {
  bullets |> List.filter(bullet => !List.exists(collision_pbullet_and_enemy(bullet), enemies));
};

let remove_enemy_pbullet_collisions = (enemies, pbullets) => {
  enemies |> List.filter(enemy => !List.exists(collision_enemy_and_pbullet(enemy), pbullets));
};

let remove_ebullet_player_collisions = (ebullets, player) => {
  List.filter(ebullet => !collision_player_ebullet(ebullet, player), ebullets);
};

let reduce_life_on_collision = (ebullets, player, life) => {
  life - List.length(List.filter(ebullet => collision_player_ebullet(ebullet, player), ebullets));
};

let increase_score_on_collision = (pbullets, enemies, score) => {
  score + (List.length(pbullets |> List.filter(bullet => List.exists(collision_pbullet_and_enemy(bullet), enemies))) * 100);
};


let setup = (env) => {
  Env.size(~width=window.w, ~height=window.h, env);
  { 
    gameTime: 0.,
    gameState: Running,
    life: 3,
    score: 0,
    py: 400., 
    px: 350., 
    pvy: 0., 
    pvx: 0.,
    enemies: [],
    pbullets: [],
    ebullets: [],
    enemySpawnTime: 0. +. eSpawnRate
  }
};

let draw = (state, env) => {
  Draw.background(Utils.color(~r=0, ~g=0, ~b=0, ~a=255), env);
  draw_enemies(state.enemies, env);
  draw_player((state.px, state.py), env);
  draw_pbullet(state.pbullets, env);
  draw_ebullet(state.ebullets, env);
  draw_score(state.score, env);
  draw_lives(state.life,env);
  (state.gameState == Pause)? draw_pause(env) : ();
  (state.gameState == Gameover)? draw_gameover(env) : ();

  let isSpawnEnemy = (state.gameTime >= state.enemySpawnTime)? true : false;
  let isShoot = Env.keyPressed(Space, env);
  /* print_endline(isSpawnEnemy? "True" : "False"); */
  /* print_float(state.gameTime); */
  /* print_endline(string_of_int(state.score)); */

  switch(state.gameState) {
    | Running => {
        ...state,
        gameTime: state.gameTime +. Env.deltaTime(env),
        gameState: 
                  ( if(state.life <= 0) {
                    print_endline("Gameover");
                    Gameover;
                  } else if (Env.keyPressed(R, env)) {
                    print_endline("Restart");
                    Restart;
                  } else if(Env.keyPressed(Enter, env)) {
                    print_endline("Paused");
                    Pause;
                  } else {
                    Running;
                  }),
        life: reduce_life_on_collision(state.ebullets, (state.px, state.py), state.life),
        score: increase_score_on_collision(state.pbullets, state.enemies, state.score),
        py: state.py +. state.pvy *. Env.deltaTime(env),
        px: state.px +. state.pvx *. Env.deltaTime(env),
        pvy:  
            (if(Env.key(Up, env)) {
              -.pspd;
            } else if(Env.key(Down, env)) {
              pspd;
            } else {
              0.;
            }),
        pvx:  
            (if(Env.key(Left, env)) {
              -.pspd;
            } else if(Env.key(Right, env)) {
              pspd;
            } else {
              0.;
            }),
        enemies:
                List.map(
                  ((x,y)) => {
                    (x, y +. espd *. Env.deltaTime(env));
                  },
                  remove_enemy_out_of_bounds((isSpawnEnemy)? add_enemy(state.enemies) : state.enemies)
                  -> remove_enemy_pbullet_collisions(state.pbullets)
                ),
        pbullets:
                List.map(
                  ((x,y)) => {
                    (x, y -. bspd *. Env.deltaTime(env));  
                  },
                  remove_pbullet_out_of_bounds(isShoot? shoot(state.px, state.py, state.pbullets) : state.pbullets)
                  -> remove_pbullet_enemy_collisions(state.enemies)
                ),
        ebullets: 
                  List.map(
                    ((x,y)) => {
                      (x, y +. ebspd *. Env.deltaTime(env));
                    },
                    (remove_ebullet_out_of_bounds(state.ebullets) @ spawnBullet(state.enemies))
                    -> remove_ebullet_player_collisions((state.px,state.py))
                  ),
        enemySpawnTime: 
                        if(state.gameTime >= state.enemySpawnTime) {
                          state.enemySpawnTime +. eSpawnRate;
                        } else {
                          state.enemySpawnTime;
                        }
      }
    | Gameover => {
      ...state,
      gameState: (if(Env.keyPressed(Enter, env) || Env.keyPressed(R, env)) {
                    print_endline("Restart");
                    Restart;
                  } else {
                    Gameover;
                  })
    }
    | Restart => {
      ...state,
      gameTime: 0.,
      gameState: Running,
      life: 3,
      score: 0,
      py: 400., 
      px: 350., 
      pvy: 0., 
      pvx: 0.,
      enemies: [],
      pbullets: [],
      ebullets: [],
      enemySpawnTime: 0. +. eSpawnRate
    }
    | Pause => {
      ...state,
      gameTime: Env.deltaTime(env),
      gameState: 
                (if(Env.keyPressed(R, env)) {
                  print_endline("Restart");
                  Restart;
                } else if(Env.keyPressed(Enter, env)) {
                  print_endline("Resumed");
                  Running
                } else {
                  Pause
                })
    }
  };
};

run(~setup, ~draw, ());