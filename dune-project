(lang dune 3.17)

(name payments-core)

(generate_opam_files true)

(license MIT)

(maintainers "winterchill-trading")

(authors "winterchill-trading")

(source
 (github winterchill-trading/payments-core))

(package
 (name payments-core)
 (synopsis "Provider-neutral payment rails, transfer state machines and ledger booking")
 (depends
  (ocaml
   (>= 5.2))
  dune
  core
  async_kernel
  ppx_jane
  money
  ledger))
