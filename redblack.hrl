% use when only storing data
-record(node, {color=black, data, left, right}).

% use as a key-value store
-record(kvnode, {color=black, key, value, left, right}).
