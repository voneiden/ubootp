(lang dune 3.11)

(name ubootp)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Matti Eiden")

(maintainers "Matti Eiden")

(license MIT)

(documentation https://github.com/voneiden/ubootp)

(package
 (name ubootp)
 (synopsis "Micro bootp like protocol for dynamic IP address assignment")
 (description "Right on")
 (depends ocaml dune)
 (tags
  (topics bootp dhcp ip iot)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
