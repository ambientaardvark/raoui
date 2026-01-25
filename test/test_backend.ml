open Raoui

(* Unit tests for backend functions that don't require kernel *)

let test_random_hex_token () =
  let token = Backend.random_hex_token 16 in
  Alcotest.(check int) "token length" 32 (String.length token);
  (* Check all chars are hex *)
  let is_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') in
  String.iter (fun c ->
    Alcotest.(check bool) "is hex char" true (is_hex c)
  ) token

let test_sign () =
  let parts = ["header"; "parent"; "metadata"; "content"] in
  let key = "testkey" in
  let sig1 = Backend.sign parts key in
  let sig2 = Backend.sign parts key in
  (* Same input should produce same signature *)
  Alcotest.(check string) "deterministic" sig1 sig2;
  (* Signature should be hex encoded (64 chars for sha256) *)
  Alcotest.(check int) "signature length" 64 (String.length sig1)

let test_sign_different_keys () =
  let parts = ["header"; "parent"; "metadata"; "content"] in
  let sig1 = Backend.sign parts "key1" in
  let sig2 = Backend.sign parts "key2" in
  Alcotest.(check bool) "different keys different sigs" true (sig1 <> sig2)

let test_sign_different_parts () =
  let key = "testkey" in
  let sig1 = Backend.sign ["a"; "b"] key in
  let sig2 = Backend.sign ["a"; "c"] key in
  Alcotest.(check bool) "different parts different sigs" true (sig1 <> sig2)

let () =
  let open Alcotest in
  run "Backend" [
    "crypto", [
      test_case "Random hex token" `Quick test_random_hex_token;
      test_case "Sign deterministic" `Quick test_sign;
      test_case "Sign different keys" `Quick test_sign_different_keys;
      test_case "Sign different parts" `Quick test_sign_different_parts;
    ];
  ]
