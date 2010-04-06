(define tmk:nbarriers (foreign-procedure () int "Tmk_get_NBARRIERS"))
(define tmk:barrier (foreign-procedure (unsigned) void "Tmk_barrier"))
(define tmk:nprocs (foreign-procedure () int "Tmk_get_NPROCS"))
(define tmk:proc-id (foreign-procedure () int "Tmk_get_proc_id"))
(define tmk:nlocks (foreign-procedure () int "Tmk_get_NLOCKS"))
(define tmk:lock-acquire
 (foreign-procedure (unsigned) void "Tmk_lock_acquire"))
(define tmk:lock-release
 (foreign-procedure (unsigned) void "Tmk_lock_release"))
