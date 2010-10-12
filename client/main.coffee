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
    nick: "anon"
    colour: "#e06d0f"
    connected: false
    registered: false
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
        log("Connecting to AMQP server at", serverIp(), 5777)
        context = this
        MQ.configure {
            # logger: console, # verbose AMQP logging
            host: serverIp(),
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
        MQ.queue("auto").bind("life", "life.player_list.update").callback (m) ->
            context.trigger("update-player-list", m)
        MQ.queue("auto").bind("life", "life.player." + state.uuid + ".registered").callback (m) ->
            context.trigger("registered", m)

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
        MQ.exchange("life").publish({ uuid: state.uuid, nick: state.nick, colour: state.colour }, "life.player." + state.uuid + ".register")

    this.get "#/game", () ->
        log("GET #/game")
        this.render "game.ejs", { width: 200, height: 200, patterns: patterns, nick: state.nick, colour: state.colour }, (rendered) ->
            log("game rendered")
            this.event_context.swap(rendered)
            $('#colour').mColorPicker()
            $('#colour').bind('colorpicked', () ->
                c = $(this).val()
                $(".pattern .cell").css("background-color", c)
                state.colour = c
                MQ.exchange("life").publish({ uuid: state.uuid, colour: state.colour }, "life.player." + state.uuid + ".colour_change"))
            $(".pattern").draggable({ revert: "invalid", opacity: 0.5, helper: "clone", cursor: "move" })
            $("#board-container").droppable({
                drop: (e, ui) ->
                    id = ui.draggable[0].id
                    boardPos = $(this).offset()
                    dropPos = ui.offset
                    x = Math.round((dropPos.left - boardPos.left) / 5.0)
                    y = Math.round((dropPos.top - boardPos.top) / 5.0) - 2
                    s = pattern_map[id]
                    log("dropped", id, x, y, s)
                    cells = _.map(s.cells, (c) -> {x: x + c.x, y: y + c.y, c: state.colour})
                    MQ.exchange("life").publish({ cells: cells }, "life.board.add_cells")
            })

    this.bind "update-board", (e, m) ->
        start = (new Date()).getTime()
        # clear board
        $("#board").html("")

        # set cells that came back from the server
        for c in m.data.board.cells
            $("#board").append('<div class="cell" style="left:' + (c.x * 5 + 1) + 'px;top:' + (c.y * 5 + 1) + 'px;background-color:' + c.c + ';"></div>')
        diff = (new Date()).getTime() - start
        log("update took: ", diff)

    this.bind "update-player-list", (e, m) ->
        this.render "players.ejs", { players: m.data.players }, (rendered) ->
            $("#players").html(rendered)

    this.bind "registered", (e, m) ->
        log("registered", e, m)
        if m.data.registered is true
            state.registered = true
            setTimeout(root.heartbeat, 1000)
            this.redirect("#/game")
        else
            log("Error: registration failed", m)

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

serverIp = () ->
    hostname = window.location.hostname
    if hostname is "localhost"
        "127.0.0.1"
    else
        hostname

root.heartbeat = () ->
    MQ.exchange("life").publish({ uuid: state.uuid }, "life.player." + state.uuid + ".heartbeat")
    setTimeout(root.heartbeat, 1000)

app = $.sammy("#root", definition)

client.init = init
