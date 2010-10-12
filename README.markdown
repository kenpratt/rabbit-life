# rabbit-life

A multiplayer version of [Conway's Game of Life](http://en.wikipedia.org/wiki/Conway's_Game_of_Life), built as a [RabbitMQ](http://www.rabbitmq.com/) demo for the [Vancouver Erlang Meetup](http://www.meetup.com/erlang-vancouver/).

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

## Software License

rabbit-life is [open-source](http://www.opensource.org/) code, licensed under the [MIT license](http://www.opensource.org/licenses/mit-license.php):

    Copyright (c) 2010 Ken Pratt <ken@kenpratt.net>

    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the "Software"), to deal in the Software without
    restriction, including without limitation the rights to use, copy,
    modify, merge, publish, distribute, sublicense, and/or sell copies
    of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
    HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
    WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    DEALINGS IN THE SOFTWARE.
