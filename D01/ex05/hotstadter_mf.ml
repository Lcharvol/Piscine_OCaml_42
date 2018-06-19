let rec hfs_m n =
  if n < 0 then -1
  else if n = 0 then 0
  else n - hfs_s (hfs_m (n - 1))
and hfs_s n =
  if n < 0 then -1
  else if n = 0 then 1
  else n - hfs_m (hfs_s (n - 1))

let () =
  print_int (hfs_m 0);
  print_char '\n';
  print_int (hfs_s 0);
  print_char '\n';
  print_int (hfs_m 4);
  print_char '\n';
  print_int (hfs_s 4);
  print_char '\n'
