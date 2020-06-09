(lang dune 2.5)

(name txtshooter)

(generate_opam_files true)

(package
 (name txtshooter)
 (depends dune websocket-wt-unix base stdio lwt_ppx ppx_yojson hmap))
