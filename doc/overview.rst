Overview of eldapo
==================



Supervision tree
----------------

::
    
    eldapo_sup (gen_supervisor)
        |
        |-------- eldapo_tcp_acceptor (gen_tcp_listener)
        |
        |-------- eldapo_handler_sup (gen_supervisor)
                      |
                      |---------- eldapo_handler (gen_fsm)
                      |
                     ... ( one per connection )
                      |
                      |---------- eldapo_hanlder (gen_fsm)
