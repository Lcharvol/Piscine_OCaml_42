(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   filemonoid.ml                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jblondea <jblondea@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/10/13 18:23:40 by jblondea          #+#    #+#             *)
(*   Updated: 2018/10/13 18:23:57 by jblondea         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type file = (string * in_channel * out_channel * string)

let zero : file = ("", stdin, stdout, "")

let fileName (file: file) : string = match file with (x, _, _, _) -> x
let inChannel (file: file) : in_channel = match file with (_, x, _, _) -> x
let outChannel (file: file) : out_channel = match file with (_, _, x, _) -> x
let line (file: file) : string = match file with (_, _, _, x) -> x

let setName (name: string) (file: file) : file = (name, inChannel file, outChannel file, line file)

let openOut (file: file) : file = (fileName file, inChannel file, open_out (fileName file), line file)
let write (content: string) (file: file) : file = output_string (outChannel file) content; file
let closeOut (file: file) : file = close_out_noerr (outChannel file); (fileName file, inChannel file, outChannel zero, line file)

let openIn (file: file) : file = (fileName file, open_in (fileName file), outChannel file, line file)
let readLine (file: file) : file = let content = input_line (inChannel file) in (fileName file, inChannel file, outChannel file, content)
let closeIn (file: file) : file = close_in_noerr (inChannel file); (fileName file, inChannel zero, outChannel file, line file)