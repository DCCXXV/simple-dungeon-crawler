type pos = { x: int; y: int }

type direction = North | South | East | West

type tile =
    | Empty
    | Wall
    | Spike
    | Lava

type enemy_type =
    | Goblin
    | Archer
    | Brute

type enemy = {
    id: int;
    pos: pos;
    enemy_type: enemy_type;
    hp: int;
}

type projectile_type =
    | Arrow

type projectile_effect =
    | Normal
    | Fire

type projectile = {
    pos: pos;
    direction: direction;
    typ: projectile_type;
    effec: projectile_effect;
    owner_id: int;
    owner_type: enemy_type;
}

type player = {
    pos: pos;
    hp: int;
    max_hp: int;
}

type player_action =
    | Move of direction
    | Wait

type grid = {
    width: int;
    height: int;
    tiles: tile array array;
}

type msg_type = Good | Bad | Neutral

type message = {
    text: string;
    msg_type: msg_type;
}

type game_state = {
    player: player;
    enemies: enemy list;
    projectiles: projectile list;
    grid: grid;
    floor: int;
    turn: int;
    messages: message list;
}

type action_result =
    | Continue of game_state
    | PlayerDeath
    | FloorComplete
