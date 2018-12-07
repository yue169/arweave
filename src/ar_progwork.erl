-module(ar_progwork).
-export([calculate/1]).
-export([test_distribution/2]).
-include("ar.hrl").

%%% An experimental ASIC-resistant proof of work based on generalised computation.
%%% The algorithm takes an input binary, uses a pseudo-random number generator,
%%% and generates an output number after performing an amount of complex work.
%%%
%%% Assumptions:
%%% SHA2-256 outputs are randomly distributed.
%%% The PRNG produces reliably 'random' results.
%%%
%%% ProgWork does not necessarily produce outputs that are uniformly distributed
%%% across the search space. It is, however, provable that the algorithm can
%%% produce every output in the 256 bit range (based on the NOP instruction and
%%% the assumed randomness properties of the hashing and PRNG algorithms).

%%% Constants
-define(PROGWORK_HASH_ALG, sha256).
-define(PROGWORK_PRNG_ALG, exrop).

-ifdef(DEBUG).
-define(PROG_LENGTH, 10).
-define(RUN_LENGTH, 50).
-else.
-define(PROG_LENGTH, 1000).
-define(RUN_LENGTH, 50000).
-endif.
-define(NUM_REGISTERS, 16).
-define(REGISTER_SZ, 64).
-define(REGISTER_MAX, (erlang:trunc(math:pow(2, ?REGISTER_SZ)) - 1)).
-define(INSTRUCTION_SET_SZ, 12).

%%% Records
-record(cpu, {
	instruction_ptr, % The index of the current position in the program (>=1).
	register_ptr, % The current register(s) to use for instructions.
	registers, % The current register values.
	program, % The list of program instructions.
	ttl = ?RUN_LENGTH, % The number of instructions left to execute for this run.
	test_flag = 0 % Flag register used in logical operation
}).

%% @doc Perform the work and return the output value.
calculate(Input) ->
	crypto:hash(?PROGWORK_HASH_ALG,
		run_prog(
			generate_prog(
				crypto:hash(?PROGWORK_HASH_ALG, Input)
			)
		)
	).

%%% PROGRAM AND CPU GENERATOR

%% @doc Use the input value to seed a PRNG, then use the PRNG to fill the 
%% registers and generate a series of instructions.
generate_prog(Hash) ->
	%% TODO: Prove this seeding method is secure.
	SeedInt = binary:decode_unsigned(Hash),
	Seed0 =
		rand:seed_s(
			?PROGWORK_PRNG_ALG,
			{SeedInt, SeedInt, SeedInt}
		),
	{Seed1, Registers} =
		generate_random_sequence(
			Seed0,
			?NUM_REGISTERS,
			?REGISTER_MAX
		),
	{Seed2, RawOpCodes} =
		generate_random_sequence(
			Seed1,
			?PROG_LENGTH,
			?INSTRUCTION_SET_SZ
		),
	{StartPos, Seed3} = rand:uniform_s(?PROG_LENGTH, Seed2),
	{StartRegisterPos, _} = rand:uniform_s(?NUM_REGISTERS, Seed3),
	#cpu {
		instruction_ptr = StartPos,
		register_ptr = StartRegisterPos,
		registers = array:from_list(Registers),
		program = lists:map(fun opcode_to_instruction/1, RawOpCodes)
	}.
	

%% @doc Generate a sequence of random integers between 1 and the given max value.
%% Return the next seed and the list of integers.
generate_random_sequence(Seed0, Length, Max) ->
	lists:foldl(
		fun(_, {SeedCurr, Acc}) ->
			{RandomValue, SeedNext} = rand:uniform_s(Max, SeedCurr),
			{SeedNext, [RandomValue|Acc]}
		end,
		{Seed0, []},
		lists:seq(1, Length)
	).

%%% INTERPRETER

%% @doc Turn integer values into their atom equivelants (for simplicity).
opcode_to_instruction(1) -> nop;
opcode_to_instruction(2) -> jmp;
opcode_to_instruction(3) -> add;
opcode_to_instruction(4) -> sub;
opcode_to_instruction(5) -> mul;
%opcode_to_instruction(6) -> idiv;
opcode_to_instruction(6) -> inc_ptr;
opcode_to_instruction(7) -> dec_ptr;
opcode_to_instruction(8) -> test;
opcode_to_instruction(9) -> clr;
opcode_to_instruction(10) -> jgt;
opcode_to_instruction(11) -> jlt;
opcode_to_instruction(12) -> je.


%% @doc Execute the program for the given number of instructions.
run_prog(#cpu { ttl = 0, registers = Registers }) ->
	erlang:iolist_to_binary(
		lists:map(
			fun binary:encode_unsigned/1,
			lists:map(
				fun erlang:abs/1,
				array:to_list(Registers)
			)
		)
	);
run_prog(CPU) ->
	run_prog(apply_instruction(CPU)).

%% @doc Perform instruction application and associated book keeping.
apply_instruction(
		CPU = #cpu {
			ttl = TTL,
			instruction_ptr = IP,
			program = Prog,
			register_ptr = RP,
			registers = Regs }
 	) ->
	CPUNext = apply_instruction(lists:nth(IP, Prog), get_registers(RP, Regs), CPU),
	CPUNext#cpu {
		ttl = TTL - 1,
		instruction_ptr =
			case (NewIP = (CPUNext#cpu.instruction_ptr + 1)) >= ?PROG_LENGTH of
				true -> 1;
				false -> NewIP
			end
	}.

apply_instruction(nop, _, CPU) ->
	CPU;
apply_instruction(jmp, Regs, CPU) ->
	jmp(Regs, CPU);
apply_instruction(add, [A, B], CPU = #cpu { register_ptr = RP, registers = Regs }) ->
	CPU#cpu { registers = set_register(RP, A + B, Regs) };
apply_instruction(sub, [A, B], CPU = #cpu { register_ptr = RP, registers = Regs }) ->
	CPU#cpu { registers = set_register(RP, A - B, Regs) };
apply_instruction(mul, [A, B], CPU = #cpu { register_ptr = RP, registers = Regs }) ->
	CPU#cpu { registers = set_register(RP, A * B, Regs) };
%apply_instruction(idiv, [A, B], CPU = #cpu { register_ptr = RP, registers = Regs }) ->
%	CPU#cpu {
%		registers =
%			array:set(
%				RP - 1,
%				case (A == 0) or (B == 0) of
%					true -> 0;
%					false -> A div B
%				end,
%				Regs
%			)
%	};
apply_instruction(inc_ptr, _, CPU = #cpu { register_ptr = RP }) ->
	CPU#cpu {
		register_ptr =
			case (NewRP = (RP + 1)) >= ?NUM_REGISTERS of
				true -> 1;
				_ -> NewRP
			end
	};
apply_instruction(dec_ptr, _, CPU = #cpu { register_ptr = RP }) ->
	CPU#cpu {
		register_ptr =
			case (NewRP = (RP - 1)) =< 1 of
				true -> ?NUM_REGISTERS;
				_ -> NewRP
			end
	};
apply_instruction(test, [A, B], CPU) ->
	CPU#cpu {
		test_flag =
			case A of
				_ when A > B -> 1;
				_ when A < B -> -1;
				_ when A == B -> 0
			end
	};
apply_instruction(clr, _, CPU) ->
	CPU#cpu { test_flag = 0 };
apply_instruction(jgt, Regs, CPU = #cpu { test_flag = TF }) ->
	case TF of 1 -> jmp(Regs, CPU); _ -> CPU end;
apply_instruction(jlt, Regs, CPU = #cpu { test_flag = TF }) ->
	case TF of -1 -> jmp(Regs, CPU); _ -> CPU end;
apply_instruction(je, Regs, CPU = #cpu { test_flag = TF }) ->
	case TF of 0 -> jmp(Regs, CPU); _ -> CPU end.


%% @doc Set the value of a register and apply range bounding.
set_register(RP, Value, Regs) ->
	array:set(RP - 1, Value rem ?REGISTER_MAX, Regs).

%% @doc Perform a jump to the first operand register.
jmp([Loc, _], CPU) ->
	CPU#cpu { instruction_ptr = erlang:abs(Loc) rem ?PROG_LENGTH }.

%% @doc Get the values of the two operand registers for an instruction.
get_registers(RP, Regs) ->
	R0 = array:get(RP - 1, Regs),
	R1 =
		case RP >= ?NUM_REGISTERS of
			true -> array:get(0, Regs);
			false -> array:get(RP, Regs)
		end,
	[R0, R1].

%%% TESING AND STATISTICAL FUNCTIONS
test_distribution(Fun, TestSz) ->
	Binary =
		iolist_to_binary(
			[
				begin
					io:format("Executing run #~w~n", [RunNum]),
					Fun(crypto:strong_rand_bytes(32))
				end
			||
				RunNum <- lists:seq(1, TestSz)
			]
		),
	sum_bytes(Binary) / byte_size(Binary).

sum_bytes(<<>>) -> 0;
sum_bytes(<< X:8, Rest/binary >>) ->
	X + sum_bytes(Rest).
