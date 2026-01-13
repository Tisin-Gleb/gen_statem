-module(code_lock).
-behaviour(gen_statem).
-define(NAME, code_lock).
-define(MAX_ATTEMPTS, 3).

-export([start_link/2,stop/0]).
-export([button/1,set_lock_button/1]).
-export([init/1,callback_mode/0,terminate/3]).
-export([handle_event/4]).


start_link(Code, LockButton) ->
    gen_statem:start_link(
        {local,?NAME}, ?MODULE, {Code,LockButton}, []).
stop() ->
    gen_statem:stop(?NAME).

button(Button) ->
    gen_statem:cast(?NAME, {button,Button}).
set_lock_button(LockButton) ->
    gen_statem:call(?NAME, {set_lock_button,LockButton}).

init({Code,LockButton}) ->
    process_flag(trap_exit, true),
    Data = #{code => Code, length => length(Code), buttons => [], attempts => 0},
    {ok, {locked,LockButton}, Data}.

callback_mode() ->
    [handle_event_function,state_enter].

%% State: locked
handle_event(enter, _OldState, {locked,_}, Data) ->
    do_lock(),
    {keep_state, Data#{buttons := []}};
handle_event(state_timeout, button, {locked,_}, Data) ->
    {keep_state, Data#{buttons := []}};
handle_event(
  cast, {button,Button}, {locked,LockButton},
  #{code := Code, length := Length, buttons := Buttons, attempts := Attempts} = Data) ->
    NewButtons =
        if
            length(Buttons) < Length ->
                Buttons;
            true ->
                tl(Buttons)
        end ++ [Button],
    if
        NewButtons =:= Code -> % Correct
            {next_state, {open,LockButton}, Data};
        NewButtons =/= Code, Length == length(NewButtons) ->
            NewAttempts = Attempts + 1,
            case NewAttempts of
                ?MAX_ATTEMPTS ->
                    {next_state, {suspended, LockButton}, Data#{buttons := NewButtons, attempts := 0}};
                _ ->
                    {keep_state, Data#{buttons := [], attempts := NewAttempts},
                    [{state_timeout,10_000,button}]} % Time in milliseconds
            end;
        true ->
             {keep_state, Data#{buttons := NewButtons},
             [{state_timeout,30_000,button}]} % Time in milliseconds
    end;
%%
%% State: open
handle_event(enter, _OldState, {open,_}, _Data) ->
    do_unlock(),
    {keep_state_and_data,
     [{state_timeout,10_000,lock}]}; % Time in milliseconds
handle_event(state_timeout, lock, {open,LockButton}, Data) ->
    {next_state, {locked,LockButton}, Data};
handle_event(cast, {button,LockButton}, {open,LockButton}, Data) ->
    {next_state, {locked,LockButton}, Data};
handle_event(cast, {button,_}, {open,_}, _Data) ->
    {keep_state_and_data,[postpone]};

%%
%% State: suspended
handle_event(enter, _OldState, {suspended,_}, _Data) ->
    io:format("Замок заблокирован после 3 неверных попыток (suspended)~n", []),
    {keep_state_and_data, [{state_timeout, 10_000, lock}]};

handle_event(state_timeout, lock, {suspended, LockButton}, Data) ->
    io:format("Восстановление из suspended → locked~n", []),
    {next_state, {locked, LockButton}, Data};

handle_event(cast, {button, _}, {suspended, _}, _Data) ->
    io:format("Замок в состоянии suspended. Попробуйте позже.~n", []),
    {keep_state_and_data,[postpone]};

handle_event({call, From}, {set_lock_button, NewLockButton},
             {suspended, _Old}, Data) ->
    {next_state, {suspended, NewLockButton}, Data,
     [{reply, From, ok}]};
%%
%% Common events
handle_event(
  {call,From}, {set_lock_button,NewLockButton},
  {StateName,OldLockButton}, Data) ->
    {next_state, {StateName,NewLockButton}, Data,
     [{reply,From,OldLockButton}]}.
do_lock() ->
    io:format("Locked~n", []).
do_unlock() ->
    io:format("Open~n", []).

terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(),
    ok.
