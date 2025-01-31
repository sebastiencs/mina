#!/bin/bash

set -eo pipefail

export RUST_BACKTRACE=1
export CC=gcc CXX=g++
export LD_LIBRARY_PATH="$(pwd)/src/lib/mina_tree/target/release"
export DUNE_PROFILE=devnet

MINA_EXE_PATH=${MINA_EXE_PATH:-_build/default/src/app/cli/src/mina.exe}
MINA_SANDBOX_DIR=${MINA_SANDBOX_DIR:-/tmp/mina-sandbox}

rm -rf "$MINA_SANDBOX_DIR"
rm -rf /tmp/minadb*

# Demo keys and config file
echo "Running Mina demo..."
MINA_CONFIG_DIR=${MINA_CONFIG_DIR:-${MINA_SANDBOX_DIR}/.mina-config}
MINA_CONFIG_FILE="${MINA_CONFIG_DIR}/daemon.json"

export PK=${PK:-"B62qiZfzW27eavtPrnF6DeDSAKEjXuGFdkouC3T5STRa6rrYLiDUP2p"}
SNARK_PK=${SNARK_PK:-"B62qjnkjj3zDxhEfxbn1qZhUawVeLsUr2GCzEz8m1MDztiBouNsiMUL"}

CONFIG_TEMPLATE=${CONFIG_TEMPLATE:-daemon.json.template}

mkdir -p /tmp/mina-sandbox/.mina-config/root/

mkdir -p ${MINA_SANDBOX_DIR}/keys && chmod go-rwx ${MINA_SANDBOX_DIR}/keys
mkdir -p --mode=700 ${MINA_CONFIG_DIR}/wallets/store/
echo "$PK" >  ${MINA_CONFIG_DIR}/wallets/store/$PK.pub
echo '{"box_primitive":"xsalsa20poly1305","pw_primitive":"argon2i","nonce":"6pcvpWSLkMi393dT5VSLR6ft56AWKkCYRqJoYia","pwsalt":"ASoBkV3NsY7ZRuxztyPJdmJCiz3R","pwdiff":[134217728,6],"ciphertext":"Dmq1Qd8uNbZRT1NT7zVbn3eubpn9Myx9Je9ZQGTKDxUv4BoPNmZAGox18qVfbbEUSuhT4ZGDt"}' > ${MINA_CONFIG_DIR}/wallets/store/${PK}
chmod go-rwx ${MINA_CONFIG_DIR}/wallets/store/${PK}
echo '{"genesis": {"genesis_state_timestamp": "${GENESIS_STATE_TIMESTAMP}"},"ledger":{"name":"mina-demo","accounts":[{"pk":"'${PK}'","balance":"66000","sk":null,"delegate":null}]}}' > ${CONFIG_TEMPLATE}

if [ -z "$GENESIS_STATE_TIMESTAMP" ]; then
    export GENESIS_STATE_TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
fi
echo "Genesis State Timestamp for this run is: ${GENESIS_STATE_TIMESTAMP}"

echo "Rewriting config file from template ${CONFIG_TEMPLATE} to ${MINA_CONFIG_FILE}"
envsubst < ${CONFIG_TEMPLATE} > ${MINA_CONFIG_FILE}

echo "Contents of config file ${MINA_CONFIG_FILE}:"
cat "${MINA_CONFIG_FILE}"

MINA_TIME_OFFSET=${MINA_TIME_OFFSET:-0}

MINA_PRIVKEY_PASS=${MINA_PRIVKEY_PASS:-""}

export MINA_PRIVKEY_PASS
export MINA_TIME_OFFSET

export RUST_BACKTRACE=1

export MINA_LIBP2P_PASS=""

mkdir -p "$MINA_SANDBOX_DIR/keys/"
# mkdir -p --mode=700 "$MINA_SANDBOX_DIR/keys/"
$MINA_EXE_PATH libp2p generate-keypair -privkey-path "$MINA_SANDBOX_DIR/keys/libp2p_key"
# $MINA_EXE_PATH advanced generate-libp2p-keypair -privkey-path "$MINA_SANDBOX_DIR/keys/libp2p_key"
# $MINA_EXE_PATH advanced generate-keypair -privkey-path "$MINA_SANDBOX_DIR/keys/libp2p_key"
chmod 700 "$MINA_SANDBOX_DIR/keys/"
# chmod 600 /tmp/mina-sandbox/keys/libp2p_key.pub

# gdb --args $MINA_EXE_PATH daemon --generate-genesis-proof true --seed --demo-mode --proof-level none --config-dir ${MINA_CONFIG_DIR} --block-producer-pubkey ${PK} --run-snark-worker ${SNARK_PK} -insecure-rest-server $@

# gdb --args \
$MINA_EXE_PATH daemon --libp2p-keypair "$MINA_SANDBOX_DIR/keys/libp2p_key" --generate-genesis-proof true --seed --demo-mode --proof-level none --config-dir ${MINA_CONFIG_DIR} --block-producer-pubkey ${PK} --run-snark-worker ${SNARK_PK} -insecure-rest-server --log-level info $@

# Or --log-level info debug trace

rc=$?
echo "Exiting Mina demo." && exit $rc
