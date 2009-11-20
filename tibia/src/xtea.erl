%%%-------------------------------------------------------------------
%%% File    : xtea.erl
%%% Author  : Olle Mattsson <olle@zubat>
%%% Description : 
%%%
%%% Created : 22 May 2009 by Olle Mattsson <olle@zubat>
%%%-------------------------------------------------------------------
-module(xtea).

-export([encrypt/2,decrypt/2,test/0, fill_padding_bytes/1]).

-include("tibia.hrl").


-define(SUM, 16#C6EF3720).
-define(DELTA, 16#61C88647).

decrypt(Key, Msg) ->
    xtea_decrypt(Key, Msg, []).

%% Take 2*4 bytes and send them to the decrypt function and
%% put the result in an accumulator
xtea_decrypt(Key, <<V0:32/?UINT,V1:32/?UINT,B/binary>>, Acc) ->
    Res = do_decrypt(Key,?SUM, V0,V1, 0),
    xtea_decrypt(Key, B, [Res|Acc]);
xtea_decrypt(_Key, Msg, Acc) when size(Msg) < 8 ->
    make_binary(Acc).


%% Iterate 32 times and then return the result
do_decrypt(_Key,_, V0, V1, 32) ->
    {V0,V1};
do_decrypt(Key,Sum, V0, V1, Rounds) when Rounds < 32 ->
    V11 = fit(V1 - (fit(fit(fit(fit(V0 bsl 4) bxor fit(V0 bsr 5)) + V0) bxor fit(Sum + element(fit(fit(Sum bsr 11) band 3)+#key.k1, Key))))),
    %% This can be changed to subtract instead of add
    %% but in my case it had to be addition
    Sum2 = fit(Sum + ?DELTA),
    V01 = fit(V0 - (fit(fit(fit(fit(V11 bsl 4) bxor fit(V11 bsr 5)) + V11) bxor fit(Sum2 + element(fit(Sum2 band 3)+#key.k1, Key))))),
    do_decrypt(Key, Sum2, V01, V11, Rounds +1).


encrypt(Key, Msg) ->
    xtea_encrypt(Key, fill_padding_bytes(Msg), []).

%% Take 2*4 bytes and send them to the encrypt function and
%% put the result in an accumulator
xtea_encrypt(Key, <<V0:32/?UINT,V1:32/?UINT,B/binary>>, Acc) ->
    Res = do_encrypt(Key, 0, V0, V1, 0),
    xtea_encrypt(Key, B, [Res|Acc]);
%% Return a binary of the result
xtea_encrypt(_Key, <<>>, Acc) ->
    make_binary(Acc).

%% Iterate 32 times and then return the result
do_encrypt(_Key, _, V0,V1, 32) ->
    {V0, V1};
do_encrypt(Key,Sum, V0,V1, Rounds) when Rounds < 32 ->
    V01 = fit(V0 + (fit(fit(fit(fit(V1 bsl 4) bxor fit(V1 bsr 5)) + V1) bxor fit(Sum + element(fit(Sum band 3)+#key.k1, Key))))),
    %% This can be changed to add instead of subtract
    %% but in my case it had to be subtraction
    Sum2 = fit(Sum - ?DELTA),
    V11 = fit(V1 + (fit(fit(fit(fit(V01 bsl 4) bxor fit(V01 bsr 5)) + V01) bxor fit(Sum2 + element(fit(fit(Sum2 bsr 11) band 3)+#key.k1, Key))))),
    do_encrypt(Key, Sum2, V01, V11, Rounds +1).

    
%% This is a function to test the encrypt/decrypt functionality
test() ->
    Msg = <<"This is a test decrypt/encrypt!!">>,
    io:format("The message: ~p\n", [Msg]),
    Key = #key{k1 = 3404669412, k2 = 1292174806,
	       k3 = 1431840963, k4 = 1813482075},
    io:format("The Key:     ~p\n", [Key]),
    Encrypted = encrypt(Key,Msg),
    io:format("The encrypted text: ~p\n", [Encrypted]),
    Decrypted = decrypt(Key,Encrypted),
    io:format("The decrypted text: ~p\n", [Decrypted]).


%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%

%% This function is needed due to Erlang's handeling of bignums
fit(Int) ->
    <<Int2:32/unsigned-integer-little>> =
	<<Int:32/unsigned-integer-little>>,
    Int2.

%% Fill up with padding bytes to be able to encrypt the message properly
fill_padding_bytes(Msg) when size(Msg) rem 8 =/= 0 ->
    NumBytesToAdd = 8 - (size(Msg) rem 8),
    PaddingBytes = list_to_binary(lists:duplicate(NumBytesToAdd, 0)),
    <<Msg/binary,PaddingBytes/binary>>;
fill_padding_bytes(Msg) ->
    Msg.


%% Make a binary of the returned list from decrypt/encrypt
make_binary(List) ->
    make_binary(List, <<>>).

make_binary([], Acc) ->
    Acc;
make_binary([{V0,V1}|T], Acc) ->
    make_binary(T, <<V0:32/?UINT,V1:32/?UINT,Acc/binary>>).

