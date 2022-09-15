#!/bin/bash

set -xe

eval $(opam env)
export LD_LIBRARY_PATH="$(pwd)/src/lib/mina_tree/target/release"
export RUST_BACKTRACE=1
export DUNE_PROFILE=devnet
export MINA_LIBP2P_PASS=

rm -rf /home/sebastien/.mina-config/ /tmp/coda_cache_dir/ /tmp/mina* 

make build_all_sigs

mkdir -p /home/sebastien/.mina-config/p2pkeys; chmod 700 /home/sebastien/.mina-config/p2pkeys

./_build/default/src/app/cli/src/mina.exe libp2p generate-keypair -privkey-path ~/.mina-config/p2pkeys/libp2p_key

./_build/default/src/app/cli/src/mina_testnet_signatures.exe daemon   --peer-list-url https://storage.googleapis.com/seed-lists/berkeley_seeds.txt --config-file genesis_ledgers/berkeley.json --insecure-rest-server --libp2p-keypair ~/.mina-config/p2pkeys/libp2p_key --log-level info 2>&1 | tee BERKLEY5.log
