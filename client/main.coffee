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
        MQ.configure {
            logger: console,
            host: "0.0.0.0",
            port: 5701
        }
        MQ.on "load", () ->
            console.log("Loaded")
        MQ.on "connect", () ->
            console.log("Connected")
        MQ.on "disconnect", () ->
            console.log("Disconnected")
        MQ.topic("life")
        MQ.queue("auto").callback (m) ->
            alert("No Binding Matches", m)
        log("foo")
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
        log("processing GET #/game 2")
        log(this)
        this.render "board.ejs", { width: 100, height: 100 }, (rendered) ->
            log("board rendered")
            this.event_context.swap(rendered)

app = $.sammy("#main", definition)

client.init = init
