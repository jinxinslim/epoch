-module(aec_db).

-export([check_db/0,           % called from setup hook
         initialize_db/1,      % assumes mnesia started
         load_database/0,      % called in aecore app start phase
         tables/1,             % for e.g. test database setup
         clear_db/0            % mostly for test purposes
        ]).

-export([transaction/1,
         ensure_transaction/1,
         write/2,
         delete/2,
         read/2]).

%% Mimicking the aec_persistence API used by aec_conductor_chain
-export([get_chain/0,
         write_block/1,
         write_header/1,
         write_block_state/2,
         write_top_block/1,
         write_top_header/1,
         get_block/1,
         get_header/1,
         get_top_block/0,
         get_top_header/0,
         get_block_state/1
        ]).

%% MP trees backend
-export([ find_accounts_node/1
        , find_contracts_node/1
        , find_ns_node/1
        , find_oracles_node/1
        , write_accounts_node/2
        , write_contracts_node/2
        , write_ns_node/2
        , write_oracles_node/2
        ]).

-export([find_block_state/1
        ]).

%% API for maintaining the tx-to-block mapping
-export([write_txs/2,
         write_tx/3,
         read_tx/1,
         delete_tx/2]).

%% API for finding transactions related to account key
-export([transactions_by_account/1,
         transactions_by_account/2]).

%% indexing callbacks
-export([ix_acct2tx/3]).

-include("common.hrl").
-include("blocks.hrl").

%% - transactions
%% - headers
%% - block  [transaction_ids]
%% - oracle_state
%% - account_state
%% - name_service_state
%% - one per state tree

-record(aec_blocks             , {key, value}).
-record(aec_headers            , {key, value}).
-record(aec_contract_state     , {key, value}).
-record(aec_tx                 , {key, tx}).
-record(aec_chain_state        , {key, value}).
-record(aec_block_state        , {key, value}).
-record(aec_oracle_state       , {key, value}).
-record(aec_account_state      , {key, value}).
-record(aec_name_service_state , {key, value}).

-define(TAB(Record), {Record, set(Mode, record_info(fields, Record))}).
-define(TAB(Rec, Extra), {Rec, set(Mode, record_info(fields, Rec), Extra)}).

%% start a transaction if there isn't already one
-define(t(Expr), case get(mnesia_activity_state) of undefined ->
                         transaction(fun() -> Expr end);
                     _ -> Expr
                 end).

tables(Mode) ->
    [?TAB(aec_blocks)
   , ?TAB(aec_headers)
   , ?TAB(aec_tx, [{index, [{acct2tx}]}])
   , ?TAB(aec_chain_state)
   , ?TAB(aec_contract_state)
   , ?TAB(aec_block_state)
   , ?TAB(aec_oracle_state)
   , ?TAB(aec_account_state)
   , ?TAB(aec_name_service_state)
    ].

clear_db() ->
    ?t([clear_table(T) || {T, _} <- tables(ram)]).

clear_table(Tab) ->
    ?t(begin
           Keys = mnesia:all_keys(Tab),
           [delete(Tab, K) || K <- Keys],
           ok
       end).

transaction(Fun) when is_function(Fun, 0) ->
    mnesia:activity(transaction, Fun).

ensure_transaction(Fun) when is_function(Fun, 0) ->
    %% TODO: actually, some non-transactions also have an activity state
    case get(mnesia_activity_state) of undefined ->
            transaction(Fun);
        _ -> Fun()
    end.


read(Tab, Key) ->
    mnesia:read(Tab, Key).

write(Tab, Obj) ->
    mnesia:write(Tab, Obj, write).

delete(Tab, Key) ->
    mnesia:delete(Tab, Key, write).

%% old-style chain_state initialization API

get_chain() ->
    ?t(mnesia:select(
         aec_headers, [{ #aec_headers{value = '$1', _ = '_'},
                         [], ['$1'] }], read)
       ++ mnesia:select(
            aec_blocks, [{ #aec_blocks{value = '$1', _ = '_'},
                           [], ['$1'] }], read)).

write_block(Block) ->
    {ok, Hash} = aec_blocks:hash_internal_representation(Block),
    ?t(mnesia:write(#aec_blocks{key = Hash, value = Block})).

write_header(Header) ->
    {ok, Hash} = aec_headers:hash_header(Header),
    ?t(mnesia:write(#aec_headers{key = Hash, value = Header})).

get_block(Hash) ->
    ?t(begin
           [#aec_blocks{value = Block}] =
               mnesia:read(aec_blocks, Hash),
           Block
       end).

get_header(Hash) ->
    ?t(begin
           [#aec_headers{value = Header}] =
               mnesia:read(aec_headers, Hash),
           Header
       end).

write_block_state(Hash, Trees) ->
    ?t(mnesia:write(#aec_block_state{key = Hash, value = Trees})).

write_accounts_node(Hash, Node) ->
    ?t(mnesia:write(#aec_account_state{key = Hash, value = Node})).

write_contracts_node(Hash, Node) ->
    ?t(mnesia:write(#aec_contract_state{key = Hash, value = Node})).

write_ns_node(Hash, Node) ->
    ?t(mnesia:write(#aec_name_service_state{key = Hash, value = Node})).

write_oracles_node(Hash, Node) ->
    ?t(mnesia:write(#aec_oracle_state{key = Hash, value = Node})).

write_top_block(Hash) ->
    ?t(mnesia:write(#aec_chain_state{key = top_block_hash, value = Hash})).

write_top_header(Hash) ->
    ?t(mnesia:write(#aec_chain_state{key = top_header_hash, value = Hash})).

get_top_block() ->
    get_chain_state_value(top_block_hash).

get_top_header() ->
    get_chain_state_value(top_header_hash).

get_block_state(Hash) ->
    ?t(begin
           [#aec_block_state{value = Trees}] =
               mnesia:read(aec_block_state, Hash),
           Trees
       end).

find_block_state(Hash) ->
    case ?t(mnesia:read(aec_block_state, Hash)) of
        [#aec_block_state{value = Trees}] -> {value, Trees};
        [] -> none
    end.

find_oracles_node(Hash) ->
    case ?t(mnesia:read(aec_oracle_state, Hash)) of
        [#aec_oracle_state{value = Node}] -> {value, Node};
        [] -> none
    end.

find_contracts_node(Hash) ->
    case ?t(mnesia:read(aec_contract_state, Hash)) of
        [#aec_contract_state{value = Node}] -> {value, Node};
        [] -> none
    end.

find_ns_node(Hash) ->
    case ?t(mnesia:read(aec_name_service_state, Hash)) of
        [#aec_name_service_state{value = Node}] -> {value, Node};
        [] -> none
    end.

find_accounts_node(Hash) ->
    case ?t(mnesia:read(aec_account_state, Hash)) of
        [#aec_account_state{value = Node}] -> {value, Node};
        [] -> none
    end.

get_chain_state_value(Key) ->
    ?t(case mnesia:read(aec_chain_state, Key) of
           [#aec_chain_state{value = Value}] ->
               Value;
           _ ->
               undefined
       end).

write_txs(Txs, Hash) ->
    ?t([write_tx(aec_tx:hash_tx(aec_tx_sign:data(Tx)), Hash, Tx)
        || Tx <- Txs]),
    ok.

write_tx(Hash, mempool, Tx) ->
    write_tx_(Hash, [], Tx);
write_tx(Hash, Where, Tx) when is_binary(Where) ->
    write_tx_(Hash, Where, Tx).

write_tx_(Hash, Where, Tx) ->
    ?t(write(aec_tx, #aec_tx{key = {Hash, Where}, tx = Tx})).

read_tx(Hash) ->
    ?t(mnesia:select(
         aec_tx, [{ #aec_tx{key = {Hash,[]}, tx = '$1', _ = '_'},
                    [], [{{mempool, '$1'}}] },
                  { #aec_tx{key = {Hash,'$1'}, tx = '$2', _ = '_'},
                    [{is_binary, '$1'}], [{{'$1', '$2'}}] }])).

delete_tx(Hash, mempool) ->
    delete_tx_(Hash, []);
delete_tx(Hash, Where) when is_binary(Where) ->
    delete_tx_(Hash, Where).

delete_tx_(Hash, Where) ->
    ?t(delete(aec_tx, {Hash, Where})).

transactions_by_account(AcctPubKey) ->
    ?t([T || #aec_tx{tx = T}
                 <- mnesia:index_read(aec_tx, AcctPubKey, {acct2tx})]).

transactions_by_account(AcctPubKey, Filter) ->
    ?t([T || #aec_tx{tx = T}
        <- mnesia:index_read(aec_tx, AcctPubKey, {acct2tx}), Filter(T)]).

%% start phase hook to load the database

load_database() ->
    lager:debug("load_database()", []),
    try
        wait_for_tables()
    catch
        error:E ->
            erlang:error({E, erlang:get_stacktrace()});
        exit:E ->
            exit({E, erlang:get_stacktrace()})
    end.

wait_for_tables() ->
    Tabs = mnesia:system_info(tables) -- [schema],
    lager:debug("wait_for_tables (~p)", [Tabs]),
    case wait_for_tables(Tabs, 0, _TimePeriods = 5, _MaxWait = 60) of
        ok -> ok;
        {timeout, Mins, NotLoaded} ->
            lager:error("Tables not loaded after ~p minutes: ~p",
                        [Mins, NotLoaded]),
            init:stop()
    end.

wait_for_tables(Tabs, Sofar, Period, Max) when Sofar < Max ->
    case mnesia:wait_for_tables(Tabs, timer:minutes(Period)) of
        ok ->
            ok;
        {timeout, NotLoaded} ->
            lager:warning("Tables not loaded after ~p minutes: ~p",
                          [Period, NotLoaded]),
            wait_for_tables(NotLoaded, Sofar + Period, Period, Max)
    end;
wait_for_tables(Tabs, Sofar, _, _) ->
    {timeout, Sofar, Tabs}.

%% Index callbacks

ix_acct2tx(aec_tx, _Ix, #aec_tx{tx = SignedTx}) ->
    try aec_tx_sign:data(SignedTx) of
        Tx ->
            aec_tx:accounts(Tx)
    catch
        error:_ ->
            []
    end;
ix_acct2tx(_, _, _) ->
    [].

%% Initialization routines

check_db() ->
    try
        Mode = case application:get_env(aecore, persist, false) of
                   true  -> disc;
                   false -> ram
               end,
        ensure_schema_storage_mode(Mode),
        ok = application:ensure_started(mnesia),
        initialize_db(Mode)
    catch
        error:Reason ->
            lager:error("CAUGHT error:~p / ~p",
                        [Reason, erlang:get_stacktrace()]),
            error(Reason)
    end.

initialize_db(Mode) ->
    add_backend_plugins(Mode),
    add_index_plugins(),
    ensure_mnesia_tables(Mode),
    ok.


add_backend_plugins(disc) ->
    mnesia_rocksdb:register();
add_backend_plugins(_) ->
    ok.

add_index_plugins() ->
    mnesia_schema:add_index_plugin({acct2tx}, aec_db, ix_acct2tx).

ensure_mnesia_tables(Mode) ->
    Tabs = mnesia:system_info(tables),
    [{atomic,ok} = mnesia:create_table(T, Spec)
     || {T, Spec} <- tables(Mode),
        not lists:member(T, Tabs)],
    ok.


set(Mode, Attrs) ->
    set(Mode, Attrs, []).

-spec set(ram | disc, [atom()], [{atom(), any()}]) -> [{atom(), any()}].
set(Mode, Attrs, Extra) ->
    [copies(Mode), {type, set}, {attributes, Attrs} | Extra].

copies(disc) -> {rocksdb_copies, [node()]};
copies(ram ) -> {ram_copies , [node()]}.

ensure_schema_storage_mode(ram) ->
    case disc_db_exists() of
        {true, Dir} ->
            lager:warning("Will not use existing Mnesia db (~s)", [Dir]),
            set_dummy_mnesia_dir(Dir);
        false ->
            ok
    end;
ensure_schema_storage_mode(disc) ->
    case mnesia:create_schema([node()]) of
        {error, {_, {already_exists, _}}} -> ok;
        ok -> ok
    end.

disc_db_exists() ->
    Dir = default_dir(),
    case f_exists(Dir) of
        true ->
            {true, Dir};
        false ->
            false
    end.

f_exists(F) ->
    case file:read_link_info(F) of
        {ok, _} -> true;
        _ -> false
    end.

set_dummy_mnesia_dir(Dir) ->
    TS = erlang:system_time(millisecond),
    NewDir = find_nonexisting(filename:absname(Dir), TS),
    application:set_env(mnesia, dir, NewDir).

find_nonexisting(Dir, N) ->
    Path = Dir ++ "-" ++ integer_to_list(N),
    case f_exists(Path) of
        true ->
            find_nonexisting(Dir, N+1);
        false ->
            Path
    end.

default_dir() ->
    case application:get_env(mnesia, dir) of
        undefined ->
            %% This is is how mnesia produces the default. The result will
            %% be the same as long as the current working directory doesn't
            %% change between now and when mnesia starts.
            filename:absname(lists:concat(["Mnesia.", node()]));
        {ok, Dir} ->
            Dir
    end.
