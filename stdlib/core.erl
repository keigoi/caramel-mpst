% Source code generated with Caramel.
-module(core).
-export_type([result/2]).


-type result(Ok, Err) :: {ok, Ok}
                       | {error, Err}
                       .


