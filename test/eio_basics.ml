let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080)

let server soc =
  Eio.Net.run_server ~on_error:(Eio.traceln "Err handling conn: %a" Fmt.exn) soc
  @@ fun flow _src_addr ->
  let msg = Eio.Buf_read.of_flow flow ~max_size:100 in
  Eio.traceln "%S" (Eio.Buf_read.line msg);
  Eio.Flow.copy_string "Hello World" flow

let client ~net ~addr =
  Eio.Switch.run @@ fun sw ->
  let client_soc = Eio.Net.connect ~sw net addr in
  Eio.Buf_write.with_flow client_soc @@ fun buf ->
  Eio.Buf_write.string buf "hello server";
  let res = Eio.Buf_read.(parse_exn take_all) ~max_size:100 client_soc in
  Eio.traceln "res: %s" res

let main ~net =
  Eio.Switch.run @@ fun sw ->
  let listening_soc = Eio.Net.listen ~reuse_addr:true ~sw ~backlog:5 net addr in
  Eio.Fiber.fork_daemon ~sw (fun () -> server listening_soc);
  client ~net ~addr

let%expect_test "" = Eio_main.run @@ fun env -> main ~net:(Eio.Stdenv.net env)
