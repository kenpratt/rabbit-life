# rabbit-life

A multiplayer version of [Conway's Game of Life](http://en.wikipedia.org/wiki/Conway's_Game_of_Life), built as a [RabbitMQ](http://www.rabbitmq.com/) demo for the [Vancouver Erlang Meetup](http://www.meetup.com/erlang-vancouver/)

## Requirements

* [Erlang](http://www.erlang.org/) (tested on R14B)
* A web server to host the client files

## Running

Start the Flash policy server

    cd server/
    rake start_policy_server

Start the RabbitMQ server with an embedded Life server

    cd server/
    rake start_rabbit_server

Host the client files

    cd client/
    python -m SimpleHTTPServer 8000

Visit [localhost:8000](http://localhost:8000/).
