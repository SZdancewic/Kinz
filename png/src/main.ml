;; open ImageLib
;; open Image      
;; open ImagePNG
;; open Arg

let worklist = ref []

let verbose = ref false 

let red = ref 0
let grn = ref 0
let blu = ref 0
let rng = ref 5
        
let verb s = if !verbose then Printf.printf "%s" s else ()

let rgb_image_fold (img:image) (f:'a -> int -> int -> int -> 'a) (b:'a) : 'a =
  let acc = ref b in
  for i = 0 to (img.width - 1) do
    for j = 0 to (img.height - 1) do
      (*      read_rgb img i j (fun r g b -> Printf.printf "r=%d g=%d b=%d\t" r g b); *)
      acc := (read_rgb img i j (f !acc))
    done
  done;
  !acc

let matches r g b : bool =
  ((!red - !rng) <= r && r <= (!red + !rng)) 
  && ((!grn - !rng) <= g && g <= (!grn + !rng))
  && ((!blu - !rng) <= b && b <= (!blu + !rng))
  

let filter img =
  rgb_image_fold img (fun x r g b -> x || matches r g b) false 
  
let do_one_file fn =
  let _ = verb (Printf.sprintf "Processing: %s\n" fn) in
  let img = ReadPNG.openfile fn in
  let _ = verb (Printf.sprintf "size = %d x %d %s\n" img.width img.height
                  (match img.pixels with
                   | RGB _ -> "RGB"
                   | RGBA _ -> "RGBA"
                   | Grey _ -> "Grey"
                   | GreyA _ -> "GreyA")) in
  if filter img then Printf.printf "%s\n" fn
              
                               
                               

(* Use the --test option to run unit tests and the quit the program. *)
let argspec = [
     ("-v", Set verbose, "enable verbose output")
   ; ("-r", Set_int red, "filter near this red value [0-255]")
   ; ("-g", Set_int grn, "filter near this green value [0-255]")
   ; ("-b", Set_int blu, "filter near this blue value [0-255]")
   ; ("-rng", Set_int rng, "search within -/+ rng of values [default 5]")
] 

let main () =
  Arg.parse argspec (fun f -> worklist := f :: !worklist) "";
  match !worklist with
  | [] -> ()
  | _ -> List.iter do_one_file !worklist

      
let () = if not !Sys.interactive then main ()
