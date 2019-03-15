-module(dolphins).
-compile(export_all).

dolphin1() ->
  receive
    do_a_flip ->
      io:format("How about no? ~n");
    fish ->
      io:format("Thanks for the fish~n");
    _ ->
      io:format("Heh, we're smater than you humans")
  end.

dolphin2() ->
  receive
    {From, do_a_flip} ->
      From ! "How about no?";
    {From, fish} ->
      From ! "Thanks buddy!";
    _ ->
      io:format("Heh, we're smater than you humans")
  end.

dolphin3() ->
  receive
    {From, do_a_flip} ->
      From ! "How about no?",
      dolphin3();
    {From, fish} ->
      From ! "so long suckers";
    _ -> 
      io:format("Heh, we're smarter that you humans. ~n"),
      dolphin3()
  end.


