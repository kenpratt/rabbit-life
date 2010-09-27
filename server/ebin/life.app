{application, life,
 [{description, "A multiplayer, RabbitMQ-backed server for Conway's game of life"},
  {vsn, "0.1"},
  {modules, [life_app,
             life_sup,
             board,
             rabbit_client,
             reloader,
             json,
             mochijson2]},
  {registered, [life_sup]},
  {applications, [kernel, sasl, stdlib]},
  {mod, {life_app, []}}
 ]}.
