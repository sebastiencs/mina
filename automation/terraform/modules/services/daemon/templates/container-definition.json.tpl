[
  {
    "name": "coda-daemon",
    "image": "codaprotocol/daemon:${mina_container_version}",
    "cpu": 0,
    "memory": 8000,
    "logConfiguration": {
      "logDriver": "awslogs",
      "options": {
        "awslogs-region": "${region}",
        "awslogs-group": "${log_group}",
        "awslogs-stream-prefix": "${log_group}"
      }
    },
    "environment" : [
        { "name" : "MINA_WALLET_KEYS", "value" : "${mina_wallet_keys}" },
        { "name" : "AWS_ACCESS_KEY_ID", "value" : "${aws_access_key}" },
        { "name" : "AWS_SECRET_ACCESS_KEY", "value" : "${aws_secret_key}" },
        { "name" : "AWS_DEFAULT_REGION", "value" : "${aws_default_region}" },
        { "name" : "DAEMON_PEER", "value" : "${daemon_peer}" },
        { "name" : "DAEMON_REST_PORT", "value" : "${daemon_rest_port}" },
        { "name" : "DAEMON_EXTERNAL_PORT", "value" : "${daemon_external_port}" },
        { "name" : "DAEMON_DISCOVERY_PORT", "value" : "${daemon_discovery_port}" },
        { "name" : "DAEMON_METRICS_PORT", "value" : "${daemon_metrics_port}" },
        { "name" : "MINA_PRIVKEY_PASS", "value" : "${mina_privkey_pass}" },
        { "name" : "MINA_SNARK_KEY", "value" : "${mina_snark_key}" },
        { "name" : "MINA_PROPOSE_KEY", "value" : "${mina_propose_key}" }
    ]
  }
]