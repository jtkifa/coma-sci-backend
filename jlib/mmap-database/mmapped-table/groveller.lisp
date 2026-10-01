(in-package :mmapped-table)

(include "fcntl.h" "sys/mman.h" "sys/stat.h")
(include "sys/file.h")

(constant (o-rdonly "O_RDONLY"))
(constant (o-rdwr "O_RDWR"))
(constant (o-creat "O_CREAT"))
(constant (prot-read "PROT_READ"))
(constant (prot-write "PROT_WRITE"))
(constant (map-shared "MAP_SHARED"))

;; for flock'ing loaded database
(constant (+lock-sh+ "LOCK_SH"))
(constant (+lock-ex+ "LOCK_EX"))
(constant (+lock-nb+ "LOCK_NB"))
(constant (+lock-un+ "LOCK_UN"))
