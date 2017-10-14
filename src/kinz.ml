(* Kinz: Kinz is not Zork *)

type dir = N|S|E|W

let str_of_dir d =
  match d with 
  | N -> "north"
  | S -> "south"
  | E -> "east"
  | W -> "west"

type room = {
    name : string;
    desc : string;
    mutable  exits : exit list;
  }

and exit = {
    d : dir;
    s : string;
    r : room;
  }

let make_room name desc : room = {
    name; desc; exits = []
  }

let remove_exit room dir =
  room.exits <- List.filter (fun e -> not (e.d = dir)) room.exits

let add_exit start dir s finish =
  let _ = remove_exit start dir in
  start.exits <- { d = dir; s; r = finish;} :: start.exits 

let find_exit rm dir : room option =
  try
    Some (List.find (fun e -> e.d = dir) rm.exits).r
  with Not_found -> None


let print_exit { d; s} = 
  Printf.printf "To the %s you see %s.\n" (str_of_dir d) s


let print_room room = 
  Printf.printf "%s\n\n" room.name;
  Printf.printf "%s\n\n" room.desc;
  List.iter print_exit room.exits

(*-------------------------------------------------------------------------------------*)

type state = {
    mutable loc: room;
  }

let init_rm = make_room "INIT" "overwrite"

let st = { loc = init_rm} 

let move_to rm = 
  st.loc <- rm;
  print_room st.loc


let go d =
  match find_exit st.loc d with
  | Some rm -> move_to rm
  | None -> Printf.printf "There is no exit to the %s.\n" (str_of_dir d)

(*-------------------------------------------------------------------------------------*)
let rm1 = make_room "Front Yard" "You are standing in front of a small wooden log cabin in the midle of a large oak forest."


let rm2 = make_room "Sitting Room" "A small ordinary sitting room with 2 small chairs and piles of dusty books on the coffee table and floor."


let rm3 = make_room "Dining Room" "a small 4-seat table is in the middle of the room, and hundreds of colorful tapestrys hang over the walls."


;; add_exit rm1 N "a narrow doorway into the house"  rm2 

;; add_exit rm2 S "a narrow doorway outside"  rm1 

;; add_exit rm2 N "a wide passageway"  rm3 

;; add_exit rm3 N "a wide passageway"  rm2 

;; move_to rm1
