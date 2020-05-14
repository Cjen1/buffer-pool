(lang dune 2.5)
(name buffer_pool)
(source (github cjen1/buffer_pool))
(authors "Chris Jensen")

(generate_opam_files true)

(package
  (name buffer_pool)
  (synopsis "A pool of buffers which automatically increases in size as required")
  (depends 
    (cstruct :with-test)
    (crowbar :with-test)
  )
)
