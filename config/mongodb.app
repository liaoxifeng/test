%% ex: ts=4 sw=4 noexpandtab syntax=erlang
{application, mongodb, [
  {description, "Client interface to MongoDB, also known as the driver. See www.mongodb.org"},
  {vsn, "3.3.2"},
  {registered, []},
  {applications, [kernel, stdlib, crypto, poolboy]},
  {mod, {mongo_app, []}}
]}.
