-module(useless).
-export([add/2, hello/0, greet_and_add_two/1]).

%% A macro will be replaced before the code is compiled for the VM
%% and is used as ?MACRO inside any function defined in the module. 
-define(sub(X,Y), X-Y).

%% Add two things
add(A,B) ->
  A + B.

%% Shows greetings.
hello() ->
  io:format("Hello, world!~n").

%% Greet and than add 2.
greet_and_add_two(X) ->
  hello(),
  add(X,2).
