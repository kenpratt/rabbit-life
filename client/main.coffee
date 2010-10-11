root = this
client = (obj) -> new wrapper(obj)
root.client = client

log = (args...) ->
    if console? and console.log?
        console.log(args...)

init = () ->
    app.run("#/")

state = {
    uuid: null
    nick: "Anonymouse"
    colour: "#e06d0f"
    connected: false
    registered: false
    dirty_cells: []
}

definition = () ->
    this.use(Sammy.EJS);
    this.debug = true

    # initialize the client (called on first page load)
    this.bind "run", () ->
        log("init")
        state.uuid = uuid()
        embedSwf()
        document.onselectstart = () -> return false # chrome move cursor hack

    # ensure connected & registered, no matter what page is requested
    this.before () ->
        if this.path is "#/connect"
            # do nothing
        else if !state.connected
            log("not connected -- redirecting to /connect")
            this.redirect("#/connect")
            return false
        else if this.path is "#/register"
            # do nothing
        else if !state.registered
            log("not logged in -- redirecting to /register")
            this.redirect("#/register")
            return false

    this.get "#/", () ->
        log("GET #/")
        this.redirect("#/game")

    this.get "#/connect", () ->
        log("GET #/connect")
        status("Connecting...")
        context = this
        MQ.configure {
            # logger: console, # verbose AMQP logging
            host: "0.0.0.0",
            port: 5777
        }
        MQ.on "load", () ->
            log("Loaded")
        MQ.on "connect", () ->
            log("Connected")
            state.connected = true
            context.redirect("#/register")
        MQ.on "disconnect", () ->
            log("Disconnected")
            state.connected = false
            showErrorOverlay("Disconnected from the server. Please refresh the page to try again.")
        MQ.topic("life")
        MQ.queue("auto").callback (m) ->
            log("Error: no binding matches", m)
        MQ.queue("auto").bind("life", "life.board.update").callback (m) ->
            context.trigger("update-board", m)
        MQ.queue("auto").bind("life", "life.players.update").callback (m) ->
            context.trigger("update-players", m)
        MQ.queue("auto").bind("life", "life.player." + state.uuid).callback (m) ->
            context.trigger("direct-message", m)

    this.get "#/register", () ->
        log("GET #/connect")
        this.render "register.ejs", { nick: state.nick, colour: state.colour }, (rendered) ->
            this.event_context.swap(rendered)
            $('#colour').mColorPicker()

    this.post "#/register", () ->
        log("POST #/register", this.params)
        status("Reticulating splines...")
        state.nick = this.params.nick
        state.colour = this.params.colour
        MQ.exchange("life").publish({ uuid: state.uuid, nick: state.nick, colour: state.colour }, "life.client.register")

    this.get "#/game", () ->
        log("GET #/game")
        this.render "game.ejs", { width: 200, height: 200, patterns: patterns, nick: state.nick, colour: state.colour }, (rendered) ->
            log("game rendered")
            this.event_context.swap(rendered)
            $('#colour').mColorPicker()
            $('#colour').bind('colorpicked', () ->
                c = $(this).val()
                $(".cell-on").css("background-color", c)
                state.colour = c
                MQ.exchange("life").publish({ uuid: state.uuid, colour: state.colour }, "life.client.colour_change"))
            $(".pattern").draggable({ revert: "invalid", opacity: 0.5, snap: ".cell", helper: "clone", cursor: "move" })
            $("#board-container").droppable({
                drop: (e, ui) ->
                    id = ui.draggable[0].id
                    boardPos = $(this).offset()
                    dropPos = ui.offset
                    x = Math.round((dropPos.left - boardPos.left) / 5.0)
                    y = Math.round((dropPos.top - boardPos.top) / 5.0)
                    s = pattern_map[id]
                    log("dropped", id, x, y, s)
                    cells = []
                    for dy in [0...s.height]
                        for dx in [0...s.width] when s.grid[dy][dx]
                            cells.push({x: x + dx, y: y + dy, c: $("#colour").attr("value")})
                    MQ.exchange("life").publish({ cells: cells }, "life.board.add")
            })

    this.bind "update-board", (e, m) ->
        # clear board
        for c in state.dirty_cells
            $("#cell_" + c.x + "_" + c.y).css("background", "#ffffff")

        # set cells that came back from the server
        cells = m.data.board.cells
        for c in cells
            $("#cell_" + c.x + "_" + c.y).css("background", c.c)
        state.dirty_cells = cells

    this.bind "update-players", (e, m) ->
        this.render "players.ejs", { players: m.data.players }, (rendered) ->
            $("#players").html(rendered)

    this.bind "direct-message", (e, m) ->
        log("direct-message", e, m)
        if m.data.registered is true
            state.registered = true
            setTimeout(root.heartbeat, 1000)
            this.redirect("#/game")
        else
            log("Error: don't know how to handle message", m)

uuid = () ->
    # http://stackoverflow.com/questions/105034/how-to-create-a-guid-uuid-in-javascript
    # http://www.ietf.org/rfc/rfc4122.txt
    s = []
    hexDigits = "0123456789ABCDEF"
    for i in [1...32]
        s[i] = hexDigits.substr(Math.floor(Math.random() * 0x10), 1)
    s[12] = "4" # bits 12-15 of the time_hi_and_version field to 0010
    s[16] = hexDigits.substr((s[16] & 0x3) | 0x8, 1) # bits 6-7 of the clock_seq_hi_and_reserved to 01
    s.join("")

embedSwf = () ->
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

status = (message) ->
    $("#root").html('<div class="status">' + message + '</div>')

showErrorOverlay = (message) ->
    $("#error-overlay .message").html(message)
    $("#error-overlay").show()

root.heartbeat = () ->
    MQ.exchange("life").publish({ uuid: state.uuid }, "life.client.heartbeat")
    setTimeout(root.heartbeat, 1000)

app = $.sammy("#root", definition)

client.init = init
