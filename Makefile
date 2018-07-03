.DEFAULT_GOAL = all

DIALYZER = dialyzer
PLT_APPS = erts kernel stdlib sasl inets ssl public_key crypto compiler  mnesia sasl eunit asn1 compiler runtime_tools syntax_tools xmerl edoc tools os_mon

ERL_OPTS = -pa ebin/ \
	-pa lib/prometheus/_build/default/lib/prometheus/ebin \
	-pa lib/accept/_build/default/lib/accept/ebin \
	-pa lib/prometheus_process_collector/_build/default/lib/prometheus_process_collector/ebin \
	-s prometheus

test_all: test test_apps

test: deps
	erlc -DTEST +export_all -o ebin/ src/ar.erl
	erl  -DTEST $(ERL_OPTS) -noshell -s ar rebuild -s ar test_coverage -s init stop

test_apps: test
	@erl -DTEST $(ERL_OPTS) -noshell -s ar test_apps -s init stop

test_networks: test
	@erl -DTEST $(ERL_OPTS) -s ar start -s ar test_networks -s init stop

tnt: test

no-vlns: test_networks

test_session: all
	erl -DTEST $(ERL_OPTS) -s ar start

realistic: test
	@erl -DTEST $(ERL_OPTS) -noshell -s ar start -s ar_test_sup start realistic

log:
	tail -f logs/`ls -t logs |  head -n 1`

catlog:
	cat logs/`ls -t logs | head -n 1`

deps: ebin logs blocks
	rm -rf priv
	rm -rf data/mnesia
	cd lib/jiffy && ./rebar compile && cd ../.. && mv lib/jiffy/priv ./
	git submodule init
	git submodule update
	(cd lib/prometheus && ./rebar3 compile)
	(cd lib/accept && ./rebar3 compile)
	(cd lib/prometheus_process_collector && ./rebar3 compile && cp _build/default/lib/prometheus_process_collector/priv/*.so ../../priv)

all: deps
	erlc -DNOTEST +export_all -o ebin/ src/ar.erl
	erl  -DNOTEST $(ERL_OPTS) -noshell -s ar rebuild -s init stop

ebin:
	mkdir -p ebin

logs:
	mkdir -p logs

blocks:
	mkdir -p blocks

docs: all
	mkdir -p docs
	(cd docs && erl -noshell -s ar docs -pa ../ebin -s init stop)

session: all
	erl -DNOTEST $(ERL_OPTS) -s ar start

sim_realistic: test
	erl -DTEST $(ERL_OPTS) -s ar_network spawn_and_mine realistic

sim_hard: test
	erl -DTEST $(ERL_OPTS) -s ar_network spawn_and_mine hard

clean:
	rm -rf data/mnesia
	rm -rf ebin docs logs priv
	rm -f erl_crash.dump

status: clean
	hg status

history:
	hg history | tac

commit:
	EDITOR=vim hg commit
	hg push

update: clean
	hg pull
	hg update

diff:
	hg extdiff -p meld

todo:
	grep --color --line-number --recursive TODO "src"

docker-image:
	docker build -t arweave .

testnet-docker: docker-image
	cat peers.testnet | sed 's/^/peer /' \
		| xargs docker run --name=arweave-testnet arweave

dev-chain-docker: docker-image
	docker run --cpus=0.5 --rm --name arweave-dev-chain --publish 1984:1984 arweave \
		no_auto_join init mine peer 127.0.0.1:9

build-plt:
	$(DIALYZER) --build_plt --output_plt .arweave.plt \
	--apps $(PLT_APPS)

dialyzer:
	$(DIALYZER) --fullpath --src -r ./src -r ./lib/*/src ./lib/pss -r ./lib/prometheus/src/ \
	-I ./lib/prometheus/include/ -I ./lib/elli/include/ -I ./lib/accept/include/ -I ./lib/fusco/include/ \
	--plt .arweave.plt --no_native \
	-Werror_handling -Wrace_conditions
