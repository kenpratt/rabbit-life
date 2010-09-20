root = this
client = (obj) -> new wrapper(obj)
root.client = client

log = (args...) ->
    if console? and console.log?
        console.log(args...)

init = () ->
    app.run("#/")

snippets = {
    "block": {
        name: "Block",
        width: 2,
        height: 2,
        cells: [{ x: 0, y: 0 }, { x: 1, y: 0 }, { x: 0, y: 1 }, { x: 1, y: 1 }],
        grid: [[true, true], [true, true]]
    },
    "blinker": {
        name: "Blinker",
        width: 3,
        height: 1,
        cells: [{ x: 0, y: 0 }, { x: 1, y: 0 }, { x: 2, y: 0 }],
        grid: [[true, true, true]]
    },
    "se-glider": {
        name: "Glider",
        width: 3,
        height: 3,
        cells: [{ x: 2, y: 0 }, { x: 0, y: 1 }, { x: 2, y: 1 }, { x: 1, y: 2 }, { x: 2, y: 2 }],
        grid: [[false, false, true], [true, false, true], [false, true, true]]
    }
}

definition = () ->
    this.use(Sammy.EJS);
    this.debug = true

    dirty_cells = []

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
        this.render "board.ejs", { width: 100, height: 100, snippets: snippets }, (rendered) ->
            log("board rendered")
            this.event_context.swap(rendered)
            $(".snippet").draggable({ revert: "invalid", opacity: 0.5, snap: ".cell", helper: "clone" })
            $("#board-container").droppable({
                drop: (e, ui) ->
                    id = ui.draggable[0].id
                    boardPos = $(this).offset()
                    dropPos = ui.offset
                    x = Math.round((dropPos.left - boardPos.left) / 5.0)
                    y = Math.round((dropPos.top - boardPos.top) / 5.0)
                    log("dropped", id, x, y)
            })

    this.bind "update-board", (e, m) ->
        log("board update")
        start = (new Date()).getTime()

        # TODO optimize update by only modifying the cells that changed colour?

        # clear board
        for c in dirty_cells
            $("#cell_" + c.x + "_" + c.y).css("background", "#ffffff")

        # set cells that came back from the server
        cells = m.data.board.cells
        for c in cells
            $("#cell_" + c.x + "_" + c.y).css("background", c.c)
        dirty_cells = cells

        diff = (new Date()).getTime() - start
        log("update took: ", diff)

app = $.sammy("#main", definition)

client.init = init

root.tick = () ->
    log("tick")
    rand = ((x) -> Math.floor(Math.random() * x))
    randColour = (() -> "#" + rand(256).toString(16) + rand(256).toString(16) + rand(256).toString(16))
    cells = {x:rand(100), y:rand(100), c:randColour()} for i in [0...500]
    MQ.exchange("life").publish({ board: { cells: cells } }, "board.update")
    #setTimeout(root.tick, 5000)
