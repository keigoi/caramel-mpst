% Source code generated with Caramel.
-module(calendar).
-export_type([date/0]).
-export_type([datetime/0]).
-export_type([day/0]).
-export_type([hour/0]).
-export_type([minute/0]).
-export_type([month/0]).
-export_type([second/0]).
-export_type([time/0]).
-export_type([year/0]).


-type year() :: integer().

-type month() :: integer().

-type day() :: integer().

-type date() :: {year(), month(), day()}.

-type hour() :: integer().

-type minute() :: integer().

-type second() :: integer().

-type time() :: {hour(), minute(), second()}.

-type datetime() :: {date(), time()}.


