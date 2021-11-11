% Source code generated with Caramel.
-module(ets).
-export_type([access/0]).
-export_type([concurrency/0]).
-export_type([make_opt/0]).
-export_type([t/2]).
-export_type([table_type/0]).


-opaque t(K, V) :: reference().

-type table_type() :: set
                    | ordered_set
                    | bag
                    | duplicate_bag
                    .

-type access() :: public
                | protected
                | private
                .

-type concurrency() :: {write_concurrency, boolean()}
                     | {read_concurrency, boolean()}
                     .

-type make_opt() :: access()
                  | concurrency()
                  | table_type()
                  | named_table
                  | {decentralized_counters, boolean()}
                  | compressed
                  .


