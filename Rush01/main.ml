let win_x = 1060
let win_y = 850

let dark_color = (33, 33, 9)
let yellow = (254, 236, 53)

let load_image file = 
  try
    Sdlloader.load_image file
  with
    | _ -> print_endline @@ file ^ " failed to load"; exit 0

let load_sound file = 
  try
    Sdlmixer.loadWAV file
  with
    | _ -> print_endline @@ file ^ " failed to load"; exit 0

let load_music file = 
  try
    Sdlmixer.load_music file
  with
    | _ -> print_endline @@ file ^ " failed to load"; exit 0

let background = load_image "tama-bg.jpg"
let standard_pet = load_image "tama_standard.png"
let dead_pet = load_image "tama_dead.png"
let bath_pet = load_image "tama_bath.png"
let eaten_pet = load_image "tama_eaten.png"
let thunder_pet = load_image "tama_thunder.png"
let kill_pet = load_image "tama_kill.png"

let tama = ref ((new Tama.tama)#load)

let sdl_init () =
    Sdl.init [`EVERYTHING];
    at_exit Sdl.quit;
    Sdlttf.init ();
    at_exit Sdlttf.quit;
    Sdlmixer.open_audio ();
    at_exit Sdlmixer.close_audio;
    Sdlevent.enable_events Sdlevent.all_events_mask

let draw_background display =
  let position_of_image = Sdlvideo.rect 0 0 win_x win_y in
  Sdlvideo.blit_surface ~dst_rect:position_of_image ~src_rect:position_of_image ~src:background ~dst:display ();
  Sdlvideo.flip display

let draw_score screen =
  let score = !tama#get_score * 10 in
  let font_size = 25 in
  let font = Sdlttf.open_font "font.ttf" font_size in
  let str = string_of_int score in
  let text = Sdlttf.render_text_blended font str ~fg:yellow in
  let draw_box =
    let size_x = 50 + ((String.length str) * 10) in
    let size_y = 70 in
    let rect  = Sdlvideo.rect (960 - ((String.length str) * 10)) 120 size_x size_y in
    Sdlvideo.fill_rect ~rect:rect screen (Sdlvideo.map_RGB screen dark_color)
  in
  draw_box;
  Sdlvideo.blit_surface ~src:text ~dst:screen ~dst_rect:(Sdlvideo.rect (980 - ((String.length str) * 10)) 140 win_x win_y) ()

let draw_game_over screen = 
  let font_size = 100 in
  let font = Sdlttf.open_font "font.ttf" font_size in
  let str = "Game over" in
  let text = Sdlttf.render_text_blended font str ~fg:Sdlvideo.white in
  Sdlvideo.blit_surface ~src:text ~dst:screen ~dst_rect:(Sdlvideo.rect ((win_x / 2) - ((String.length str) * 28)) ((win_y / 2) - 100) win_x win_y) ();
  Sdlvideo.flip screen

let draw_pet ?(status = "standard") display =
  let image = match status with
    | "standard" -> standard_pet
    | "dead" -> dead_pet
    | "bath" -> bath_pet
    | "eaten" -> eaten_pet
    | "thunder" -> thunder_pet
    | "kill" -> kill_pet
    | _ -> standard_pet
  in
  let position_of_image = Sdlvideo.rect ((win_x / 2) - 170) ((win_y / 4) - 100) 50 50 in
  Sdlvideo.blit_surface ~dst_rect:position_of_image ~src:image ~dst:display ();
  Sdlvideo.flip display

let buttons = [
  (50, (win_y - 100),"Eat"); 
  (300, (win_y - 100), "Thunder");
  (550, (win_y - 100), "Bath");
  (800, (win_y - 100), "Kill")
  ]

let draw_text screen s dst_rect color =
  let font = Sdlttf.open_font "font.ttf" 24 in
  let s = match s with
    | "" -> " " (* render_text functions don't like empty strings *)
    | _  -> s in
  let text = Sdlttf.render_text_blended font s ~fg:color in
  Sdlvideo.blit_surface ~src:text ~dst:screen ~dst_rect:dst_rect ()

let draw_button ?(color = dark_color) ?(textColor = yellow) x y label display =
  let draw_box =
    let size_x = 210 in
    let size_y = 60 in
    let rect  = Sdlvideo.rect x y size_x size_y in
    Sdlvideo.fill_rect ~rect:rect display (Sdlvideo.map_RGB display color)
  in
  let draw_label =
    draw_text display label (Sdlvideo.rect (x + 105 - ((String.length label) * 7)) (y + 14) 200 60) textColor
  in
  draw_box;
  draw_label

let rec draw_buttons display buttons = match buttons with
  | (x, y, label)::tail when !tama#get_dead = false -> 
    draw_button x y label display ~color:dark_color;
    draw_buttons display tail
  | _ -> ()

let draw_restart_button display =
  draw_button ((win_x / 2) - 105) ((win_y / 2) + 50) "Try again" display ~color:dark_color

let rec check_hit_box x y hit_boxes display = match hit_boxes with
  | (x_start, y_start, x_end, y_end, label, _)::tail when (label <> "Try again") ->
    if((x_start < x) && (x_end > x) && (y_start < y) && (y_end > y) && (!tama#get_dead = false)) then
      (draw_button x_start y_start label display ~color:(255, 210, 86) ~textColor:(27, 56, 50);
      check_hit_box x y tail display)
    else
      (draw_button x_start y_start label display;
      check_hit_box x y tail display)
  | _ -> ()

let rec draw_info_bars display =
  let bars = [
    (50, 70, "Health", (!tama)#get_health);
    (300, 70, "Energy", (!tama)#get_energy);
    (550, 70, "Hygiene", (!tama)#get_hygiene);
    (800, 70, "Happy", (!tama)#get_happiness)]
  in
  let rec draw_info_bars_rec bars =
    match bars with
    | (x, y, label, value)::tail ->
      let size_y = 40 in
      let size_x = 210 in
      let rect  = Sdlvideo.rect x y size_x size_y in
      let draw_bar_inner () =
        let size_x = 200 in
        let size_y = 30 in
        let rect  = Sdlvideo.rect (x + 5) (y + 5) size_x size_y in
        Sdlvideo.fill_rect ~rect:rect display (Sdlvideo.map_RGB display dark_color);
      in
      let draw_value value =
        let size_x = value * 2 in
        let size_y = 30 in
        let rect  = Sdlvideo.rect (x + 5) (y + 5) size_x size_y in
        Sdlvideo.fill_rect ~rect:rect display (Sdlvideo.map_RGB display yellow)
      in
      let draw_label label =
        draw_text display label (Sdlvideo.rect (x + 20) (y - 40) size_x size_y)  dark_color
      in
      Sdlvideo.fill_rect ~rect:rect display (Sdlvideo.map_RGB display  dark_color);
      draw_bar_inner ();
      draw_value value;
      draw_label label;
      draw_info_bars_rec tail
    | _ -> ()
  in draw_info_bars_rec bars

let draw_scene ?(status = "standard") display =
  draw_background display;
  draw_pet display ~status:status;
  draw_info_bars display;
  draw_buttons display buttons;
  draw_score display;
  Sdlvideo.flip display

let rec handle_click x y hit_boxes display action_time = match hit_boxes with
  | (x_start, y_start, x_end, y_end, label, callback)::tail ->
    if((x_start < x) && (x_end > x) && (y_start < y) && (y_end > y)) then
      begin
        callback ();
        draw_info_bars display;
        2000
      end
    else
      handle_click x y tail display action_time
  | _ -> action_time

let rec handle_event display hit_boxes timeAtLastSec action_time last_tick_time =
  let time_delta = (Sdltimer.get_ticks ()) - last_tick_time in
  let last_tick_time = (Sdltimer.get_ticks ()) in
  let (action_time) = begin
    match Sdlevent.poll () with
    |	Some QUIT ->
      !tama#save;
      exit 0
    | Some Sdlevent.MOUSEBUTTONUP {Sdlevent.mbe_button = BUTTON_LEFT; Sdlevent.mbe_state = Sdlevent.RELEASED; Sdlevent.mbe_x = x; Sdlevent.mbe_y = y} ->
      (handle_click x y hit_boxes display action_time)
    | Some Sdlevent.MOUSEMOTION {Sdlevent.mme_x = x; Sdlevent.mme_y = y} ->
      check_hit_box x y hit_boxes display;
      (action_time)
    | Some Sdlevent.KEYDOWN {Sdlevent.keysym = Sdlkey.KEY_ESCAPE} -> exit 0
    | Some event ->
        (* print_endline (Sdlevent.string_of_event event); *)
        (action_time)
    | None -> (action_time)
  end in
  Sdlvideo.flip display;
  let timeAtLastSec = 
    if ((Sdltimer.get_ticks ()) - timeAtLastSec) > 1000 then
      ((tama := (!tama#undergoTime));
      draw_info_bars display;
      (* draw_scene ~status:"standard" display; *)
      Sdltimer.get_ticks ())
    else
      timeAtLastSec
  in
  let action_time = match action_time with
    | x when x > 0 && x - time_delta <= 0 ->
      (draw_scene ~status:"standard" display;
      0)
    | x when x > 0 -> (action_time - time_delta)
    | x -> 0
  in
  if(!tama#get_dead)
    then 
      begin
        draw_scene display ~status:"dead";
        draw_restart_button display;
        draw_game_over display;
      end;
  draw_score display;
  handle_event display hit_boxes timeAtLastSec action_time last_tick_time

let music = load_music "hydrogen.ogg"
let deadMusic = load_music "dead.ogg"

let play_sound sound = 
  try
    (if not !tama#get_dead then
      Sdlmixer.play_sound sound)
  with
    | _ -> ()

let () =
  at_exit (fun () -> !tama#save);
  sdl_init ();
  let (eatSound, thunderSound, bathSound, killSound) = (load_sound "eat.wav", load_sound "thunder.wav", load_sound "bath.wav", load_sound "kill.wav") in
  at_exit (fun () ->
    Sdlmixer.free_chunk eatSound;
    Sdlmixer.free_chunk thunderSound;
    Sdlmixer.free_chunk bathSound;
    Sdlmixer.free_chunk killSound
  );
  at_exit (fun () -> (
    Sdlmixer.halt_music (); 
    Sdlmixer.free_music music;
    Sdlmixer.free_music deadMusic
  ));
  tama := !tama#set_onDead (fun () -> 
    Sdlmixer.halt_music ();
    Sdlmixer.fadein_music deadMusic 1.0
  );
  let display = Sdlvideo.set_video_mode win_x win_y [`DOUBLEBUF] in
  begin
    let hit_boxes = [
      (50, (win_y - 100), 250, (win_y - 40), "Eat", (fun () ->
          print_endline "eat";
          tama := (!tama)#eat;
          draw_scene display ~status:"eaten";
          play_sound eatSound
        )
      );
      (300, (win_y - 100), 500, (win_y - 40), "Thunder", (fun () ->
          print_endline "thunder";
          draw_scene display ~status:"thunder";
          tama := (!tama)#thunder;
          play_sound thunderSound
        )
      );
      (550, (win_y - 100), 750, (win_y - 40), "Bath", (fun () ->
          print_endline "bath";
          draw_scene display ~status:"bath";
          tama := (!tama)#bath;
          play_sound bathSound
        )
      );
      (800, (win_y - 100), 1000, (win_y - 40), "Kill", (fun () ->
          print_endline "kill";
          draw_scene display ~status:"kill";
          tama := (!tama)#kill;
          play_sound killSound
        )
      );
      (((win_x / 2) - 105), ((win_y / 2) + 50), ((win_x / 2) + 105), ((win_y / 2) + 110), "Try again", (fun () ->
          if(!tama#get_dead)
            then
              begin
                tama := (!tama)#toDefault;
                draw_scene display;
                Sdlmixer.halt_music ();
                Sdlmixer.fadein_music music 1.0
              end
        )
      )
    ] in
    draw_scene ~status:"standard" display;
    Sdlmixer.fadein_music music 1.0;
    ignore @@ handle_event display hit_boxes (Sdltimer.get_ticks ()) 0 (Sdltimer.get_ticks ());
    ignore @@ exit 0
  end;
  ()