Array.prototype.find = function(fn) {
	var count = 0;
	while(count < this.length) {
		if(fn(this[count])) {
			return this[count];
		}
		count++;
	}
	return null;
};

var SnakeOpts = {
	size: 4
};

var SnakeGame = new Class({

	exchange	: null,
	directions	: {
		down	: { x:  1, y:  0 },
		up		: { x: -1, y:  0 },
		right	: { x:  0, y:  1 },
		left	: { x:  0, y: -1 }
	},
	
	score	: 0,
	timer	: null,
	canvas	: null,
	snake	: null,
	name	: null,
	snakes	: [],
	fps		: 24,

	initialize: function(opts) {
		$extend(this, opts);
		
		$(document).addEvent("keydown", this.onKeyPress.bind(this));
		this.canvas.addEvent("click", this.spawn.bind(this));
		
		this.exchange = MQ.exchange("snake", {
			type: "fanout"
		});
		
		MQ.queue("auto").bind("snake").callback(this.onMessage, this);
		

		
		this.start();
	},
	
	start: function() {
		this.timer = setInterval(this.render.bind(this), 1000 / this.fps);
	},
	
	stop: function() {
		
	},
	
	spawn: function() {
		if(this.snake == null) {
			this.snake = new Snake({
				name: this.name,
				game: this
			});
			this.snake.render();
			this.snake.announce();
		}
	},
	
	findSnake: function(name) {
		return this.snakes.find(function(s) {
			return s.name == name;
		});
	},
	
	deleteSnake: function(name) {
		this.snakes.erase(this.snakes.find(function(s) {
			return s.name == name;
		}));
	},
	
	spawnOther: function() {
	},
	
	render: function() {
		
		
		if(this.snake) {
			this.snake.render();
			this.score += this.snake.size() / 10;
			
			var head = this.snake.getHead();
			if(!this.inBounds(head.x,head.y)) {
				this.snake.die(true);
				this.snake = null;
			}
		}
		
		this.snakes.each(function(snake) {
			snake.render();
		});
	},
	
	inBounds: function(x,y) {
		return x < this.canvas.getHeight()
			&& y < this.canvas.getWidth()
			&& x >= 0
			&& y >= 0;
	},
	
	onKeyPress: function(event) {
		if(this.snake != null) {
			if(this.directions[event.key]) {
				this.snake.turn(this.directions[event.key]);
			}
		}
	},
	
	onMessage: function(m) {
		if(this.snake && m.data.src == this.snake.name) {
			return;
		}
		
		var snake = this.findSnake(m.data.src);

		switch(m.data.cmd) {
			case "announce":
				if(!snake) {
					snake = new Snake({
						game: this
					});
					
					this.snakes.push(snake);
					snake.update(m.data.data);
					
					if(this.snake) {
						this.snake.announce();
					}
				}
				break;
			case "turn":
				if(snake) {
					snake.update(m.data.data);
				}
				break;
			case "die":
				if(snake) {
					snake.die();
					this.deleteSnake(snake.name);
				}
				break;
			default:
				break;
		}
	}

});

var Snake = new Class({

	dir: {
		x:  1,
		y:  0
	},
	
	game	: null,
	name	: "Snake",
	parts	: [],
	growth	: 16,

	initialize: function(opts) {
		$extend(this,opts)
	},
	
	getHead: function() {
		return this.parts[0] || { x: 20, y:20, size: SnakeOpts.size };
	},
	
	getTail: function() {
		return this.parts.pop();
	},
	
	grow: function(amount) {
		this.growth += amount;
	},
	
	shrink: function(amount) {
		this.growth -= amount;
	},
	
	turn: function(dir) {
		if((dir.x != 0 && this.dir.x == 0) || (dir.y != 0 && this.dir.y == 0)) {
			this.dir = dir;
			this.notify("turn", {
				dir: this.dir
			});
		}
	},
	
	render: function() {
		
		var head = this.getHead();
		var part = new SnakePart({
			snake: this,
			x: head.x + SnakeOpts.size * this.dir.x,
			y: head.y + SnakeOpts.size * this.dir.y
		});
		
		this.parts.unshift(part);
		
		if(this.growth <= 0) {
			if(this.growth < 0) {
				this.removeTail();
				this.growth++;
			}
			this.removeTail();
		} else {
			this.growth--;
		}
	},
	
	removeTail: function() {
		this.removePart(this.getTail());
	},
	
	removePart: function(p) {
		p.element.destroy();
		delete p;
	},
	
	size: function() {
		return this.parts.length;
	},
	
	die: function(notify) {
		this.growth = 0;
		while(this.size() > 0) {
			this.removePart(this.parts.pop());
		}
		if(notify) {
			this.notify("die", {});
		}
	},
	
	announce: function() {
		this.notify("announce", this.serialize());
	},
	
	serialize: function() {
		return {
			dir		: this.dir,
			name	: this.name,
			growth	: this.growth,
			parts	: this.parts.map(function(item) {
				return {
					x: item.x,
					y: item.y
				};
			})
		};
	},
	
	notify: function(cmd, data) {
		this.game.exchange.publish({src: this.name, cmd: cmd, data: data});
	},
	
	update: function(data) {
		for(var k in data) {
			if(k == "parts") {
				this.parts = data.parts.map(function(p) {
					return new SnakePart({
						x: p.x,
						y: p.x,
						snake: this
					});
				});
			} else {
				this[k] = data[k];
			}
		}
	}

});


var SnakePart = new Class({

	
	element	: null,
	snake	: null,
	x		: 0,
	y		: 0,
	
	initialize: function(opts) {
		$extend(this, opts);
		this.render();
	},
	
	render: function() {
		this.snake.game.canvas.adopt(this.toElement());
	},
	
	toElement: function() {
		return this.element = new Element('div', {
			styles: {
				width 	: SnakeOpts.size,
				height	: SnakeOpts.size,
				top		: this.x,
				left	: this.y
			},
			"class"	: "part"
		});
	}
});