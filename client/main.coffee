root = this
client = (obj) -> new wrapper(obj)
root.client = client

log = (args...) ->
    if console? and console.log?
        console.log(args...)

init = () ->
    app.run("#/")

definition = () ->
    this.use(Sammy.EJS);
    this.debug = true

    this.bind "run", () ->
        log("init")
        context = this
        MQ.configure {
            logger: console,
            host: "0.0.0.0",
            port: 5701
        }
        MQ.on "load", () ->
            log("Loaded")
        MQ.on "connect", () ->
            log("Connected")
            setTimeout(root.tick, 100)
        MQ.on "disconnect", () ->
            log("Disconnected")
        MQ.topic("life")
        MQ.queue("auto").callback (m) ->
            log("Error: no binding matches", m)
        MQ.queue("auto").bind("life", "board.*").callback (m) ->
            context.trigger("update-board", m)

        swfobject.embedSWF(
            "vendor/amqp-js/swfs/amqp.swf?nc=" + Math.random().toString(),
            "AMQPProxy",
            "1",
            "1",
            "9",
            "vendor/amqp-js/swfs/expressInstall.swf",
            {},
            {
                allowScriptAccess: "always",
                wmode: "transparent"
            },
            {},
            () ->
                log("Swfobject loaded")
        )

    this.get "#/", () ->
        log("processing GET #/")
        this.swap("Welcome!<br/><a href=\"#/game\">Play</a>")

    this.get "#/game", () ->
        log("processing GET #/game")
        this.render "board.ejs", { width: 100, height: 100 }, (rendered) ->
            log("board rendered")
            this.event_context.swap(rendered)

    this.bind "update-board", (e, m) ->
        log("board update")
        cells = m.data.board.cells
        for c in cells
            $("#cell_" + c.x + "_" + c.y).css("background", c.c)

app = $.sammy("#main", definition)

client.init = init

root.tick = () ->
    log("tick")
    rand = ((x) -> Math.floor(Math.random() * x))
    randColour = (() -> "#" + rand(256).toString(16) + rand(256).toString(16) + rand(256).toString(16))
    cells = {x:rand(100), y:rand(100), c:randColour()} for i in [0...100]
    MQ.exchange("life").publish({ board: { cells: cells } }, "board.update");
    setTimeout(root.tick, 1000)