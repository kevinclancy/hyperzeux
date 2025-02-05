open Common
open Agent
open AgentState
open BoardInterface

let walk_north (puppet : Puppet.t) =
  Puppet.set_texture puppet (TextureMap.get "person2_north.png");
  Actions.weak_walk_north ()

let walk_east (puppet : Puppet.t) =
  Puppet.set_texture puppet (TextureMap.get "person2_east.png");
  Actions.weak_walk_east ()

let walk_south (puppet : Puppet.t) =
  Puppet.set_texture puppet (TextureMap.get "person2_south.png");
  Actions.weak_walk_south ()

let walk_west (puppet : Puppet.t) =
  Puppet.set_texture puppet (TextureMap.get "person2_west.png");
  Actions.weak_walk_west ()

type direction =
  | North
  | East
  | South
  | West
  | Idle

type walk_state_private = {
  step_queue : direction Queue.t ;
  (** The player must now respond to unhandled key presses by taking steps in these directions. *)

  direction : direction ref ;
  (** The direction associated with this state, which we repeatedly walk in when [step_queue] is empty *)

  up_down : bool ref ;
  (** Is the up arrow key held down? *)

  right_down : bool ref ;
  (** Is the right arrow key held down? *)

  down_down : bool ref ;
  (** Is the down arrow key held down? *)

  left_down : bool ref ;
  (** Is the left arrow key held down? *)
}

let state_walking : walk_state_private AgentState.blueprint =
  (** After initial idle state, player transfers here to respond to arrow key presses *)

  let walk (me : Puppet.t) (d : direction) : unit =
    match d with
    | North ->
      walk_north me
    | East ->
      walk_east me
    | South ->
      walk_south me
    | West ->
      walk_west me
    | Idle ->
      Actions.wait 1
  in

  let update_direction (me : Puppet.t) (s : walk_state_private) (key_dir : direction) : unit =
    (** If a single arrow key is held down after pressing or releasing an arrow key, change [direction] field to the held key's direction *)
    begin match !(s.up_down), !(s.left_down), !(s.right_down), !(s.down_down) with
    | (true, false, false, false) ->
      s.direction := North
    | (false, true, false, false) ->
      s.direction := West
    | (false, false, true, false) ->
      s.direction := East
    | (false, false, false, true) ->
      s.direction := South
    | (false, false, false, false) ->
      s.direction := Idle
    | _ ->
      ()
    end
  in

  {
    state_functions = {
      AgentState.empty_state_functions with
        (* Initially start patrolling in a 2x2 square *)
        script = Some (fun (board : board_interface) (me : Puppet.t) (s : walk_state_private) ->
          while true do
            while not (Queue.is_empty s.step_queue) do
              walk me (Queue.pop s.step_queue)
            done;
            walk me !(s.direction)
          done;
        ) ;
        key_left_pressed = Some (fun (board : board_interface) (me : Puppet.t) (s : walk_state_private) ->
          Queue.push West s.step_queue;
          s.left_down := true;
          update_direction me s West;
          None
        );
        key_right_pressed = Some (fun (board : board_interface) (me : Puppet.t) (s : walk_state_private) ->
          Queue.push East s.step_queue;
          s.right_down := true;
          update_direction me s East;
          None
        );
        key_up_pressed = Some (fun (board : board_interface) (me : Puppet.t) (s : walk_state_private) ->
          Queue.push North s.step_queue;
          s.up_down := true;
          update_direction me s North;
          None
        );
        key_down_pressed = Some (fun (board : board_interface) (me : Puppet.t) (s : walk_state_private) ->
          Queue.push South s.step_queue;
          s.down_down := true;
          update_direction me s South;
          None
        );
        key_up_released = Some (fun (board : board_interface) (me : Puppet.t) (s : walk_state_private) ->
          s.up_down := false;
          update_direction me s North;
          None
        );
        key_right_released = Some (fun (board : board_interface) (me : Puppet.t) (s : walk_state_private) ->
          s.right_down := false;
          update_direction me s East;
          None
        );
        key_down_released = Some (fun (board : board_interface) (me : Puppet.t) (s : walk_state_private) ->
          s.down_down := false;
          update_direction me s South;
          None
        );
        key_left_released = Some (fun (board : board_interface) (me : Puppet.t) (s : walk_state_private) ->
          s.left_down := false;
          update_direction me s West;
          None
        );
    };

    props = {
      region_name = None;
      name = "Idle"
    }
  }

module Player : AgentClass = struct
  let states = StringMap.of_list [
    ("Walking", state_walking.props)
  ]

  let initial_state =
    let init_private = {
      step_queue = Queue.create ();
      direction = ref Idle;
      up_down = ref false ;
      right_down = ref false ;
      down_down = ref false ;
      left_down = ref false
    } in

    AgentState.create state_walking init_private

  let preview_texture_name = "person2_south.png"

  let preview_color = Raylib.Color.skyblue

  let speed = 0.8

  let name = "player"
end

include Player
