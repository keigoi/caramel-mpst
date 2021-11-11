% Source code generated with Caramel.
-module(binary).
-export_type([endianness/0]).
-export_type([replace_opt/0]).
-export_type([split_opt/0]).


-type endianness() :: big
                    | little
                    .

-type replace_opt() :: global
                     | {scope, {integer(), integer()}}
                     | {insert_replaced, list(integer())}
                     .

-type split_opt() :: global
                   | trim_all
                   .


