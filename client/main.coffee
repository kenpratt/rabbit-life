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
