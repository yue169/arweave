CREATE TABLE block (
    indep_hash TEXT PRIMARY KEY,
    previous_block TEXT,
    height INTEGER,
    timestamp INTEGER
);

CREATE INDEX idx_block_height ON block (height);
CREATE INDEX idx_block_timestamp ON block (timestamp);

CREATE TABLE tx (
    id TEXT PRIMARY KEY,
    block_indep_hash TEXT,
    last_tx TEXT,
    owner TEXT,
    from_address TEXT,
    target TEXT,
    quantity INTEGER,
    signature TEXT,
    reward INTEGER
);

CREATE INDEX idx_tx_block_indep_hash ON tx (block_indep_hash);
CREATE INDEX idx_tx_from ON tx (from_address);

CREATE TABLE tag (
    tx_id TEXT,
    name TEXT,
    value TEXT
);

CREATE INDEX idx_tag_tx_id ON tag (tx_id);
CREATE INDEX idx_tag_name_value ON tag (name, value);
