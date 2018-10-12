module type App =
  sig
    type project = string * string * int
    val zero : project
    val combine : project -> project -> project
    val fail : project -> project
    val success : project -> project
  end

module App =
  struct
    type project = string * string * int
    let zero = ("", "", 0)
    let combine (name1,status1,grade1) (name2,status2,grade2) =
      let av_grade = (grade1 + grade2) / 2
      in
      let new_status = if(av_grade >= 80) then "succeed" else "failed"
      in
      (name1 ^ name2, new_status, av_grade)
    let fail (name,status,grade) = (name, "failed", 0)
    let success (name,status,grade) = (name, "succeed", 80)
    let print_proj (name,status,grade) = print_endline ("name: " ^ name ^ "; status: " ^ status ^ "; grade: " ^ string_of_int grade)
  end

let () =
    let fail_proj = ("fail_proj", "failed", 0)
    in
    let succeed_proj = ("succeed_proj", "succeed", 80)
    in
    App.print_proj fail_proj;
    App.print_proj succeed_proj;
    App.print_proj (App.fail succeed_proj);
    App.print_proj (App.success succeed_proj);
    App.print_proj (App.success fail_proj);
    App.print_proj (App.fail fail_proj);
    App.print_proj (App.combine fail_proj succeed_proj);
    App.print_proj (App.combine succeed_proj succeed_proj)