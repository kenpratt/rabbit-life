root = this

raw_patterns = {
    "Still lives": [
        {
            id: "pattern-block"
            name: "Block"
            grid: "XX
                   XX"
        }
        {
            id: "pattern-beehive"
            name: "Beehive"
            grid: "_XX_
                   X__X
                   _XX_"
        }
        {
            id: "pattern-loaf"
            name: "Loaf"
            grid: "_XX_
                   X__X
                   _X_X
                   __X_"
        }
        {
            id: "pattern-boat"
            name: "Boat"
            grid: "XX_
                   X_X
                   _X_"
        }
    ]
    "Oscillators": [
        {
            id: "pattern-blinker"
            name: "Blinker"
            grid: "___
                   XXX
                   ___"
        }
        {
            id: "pattern-toad"
            name: "Toad"
            grid: "_XXX
                   XXX_"
        }
        {
            id: "pattern-beacon"
            name: "Beacon"
            grid: "XX__
                   XX__
                   __XX
                   __XX"
        }
        {
            id: "pattern-tumbler"
            name: "Tumbler"
            grid: "_X_____X_
                   X_X___X_X
                   X__X_X__X
                   __X___X__
                   __XX_XX__"
        }
        {
            id: "pattern-pulsar"
            name: "Pulsar"
            grid: "__XXX___XXX__
                   _____________
                   X____X_X____X
                   X____X_X____X
                   X____X_X____X
                   __XXX___XXX__
                   _____________
                   __XXX___XXX__
                   X____X_X____X
                   X____X_X____X
                   X____X_X____X
                   _____________
                   __XXX___XXX__"
        }
        {
            id: "pattern-clock"
            name: "Clock"
            grid: "__X_
                   X_X_
                   _X_X
                   _X__"
        }
        {
            id: "pattern-mold"
            name: "Mold"
            grid: "___XX_
                   __X__
                   X__X_X
                   ____X_
                   X_XX__
                   _X____"
        }
    ]
    "Spaceships": [
        {
            id: "pattern-se-glider"
            name: "Glider (SE)"
            grid: "__X
                   X_X
                   _XX"
        }
        {
            id: "pattern-sw-glider"
            name: "Glider (SW)"
            grid: "_X_
                   X__
                   XXX"
        }
        {
            id: "pattern-nw-glider"
            name: "Glider (NW)"
            grid: "XX_
                   X_X
                   X__"
        }
        {
            id: "pattern-ne-glider"
            name: "Glider (NE)"
            grid: "XXX
                   __X
                   _X_"
        }
        {
            id: "pattern-w-lwss"
            name: "Spaceship (W)"
            grid: "_XX__
                   XXXX_
                   XX_XX
                   __XX_"
        }
        {
            id: "pattern-e-lwss"
            name: "Spaceship (E)"
            grid: "__XX_
                   _XXXX
                   XX_XX
                   _XX__"
        }
        {
            id: "pattern-nw-goose"
            name: "Canada goose (NW)"
            grid: "XXX__________
                   X_________XX_
                   _X______XXX_X
                   ___XX__XX____
                   ____X________
                   ________X____
                   ____XX___X___
                   ___X_X_XX____
                   ___X_X__X_XX_
                   __X____XX____
                   __XX_________
                   __XX_________"
        }
    ]
    "Guns": [
        {
            id: "pattern-gosper-glider-gun"
            name: "Gosper glider gun"
            grid: "________________________X_____________
                   ______________________X_X_____________
                   ____________XX______XX______________XX
                   ___________X___X____XX______________XX
                   XX________X_____X___XX________________
                   XX________X___X_XX____X_X_____________
                   __________X_____X_______X_____________
                   ___________X___X______________________
                   ____________XX________________________"
        }
    ]
    "Oddballs": [
        {
            id: "pattern-rabbit"
            name: "Rabbit"
            grid: "X_X___
                   X_X___
                   XXXXXX
                   XXXX_X
                   XXXXXX"
        }
        {
            id: "pattern-butterfly"
            name: "Butterfly"
            grid: "X___
                   XX__
                   X_X_
                   _XXX"
        }
        {
            id: "pattern-exploder"
            name: "Exploder"
            grid: "X_X_X
                   X___X
                   X___X
                   X___X
                   X_X_X"
        }
        {
            id: "pattern-r-pentomino"
            name: "R-pentomino"
            grid: "_XX
                   XX_
                   _X_"
        }
        {
            id: "pattern-diehard"
            name: "Diehard"
            grid: "______X_
                   XX______
                   _X___XXX"
        }
        {
            id: "pattern-acorn"
            name: "Acorn"
            grid: "_X_____
                   ___X___
                   XX__XXX"
        }
    ]
}

root.patterns = {}
_.each(raw_patterns, (list, name) ->
    root.patterns[name] = _.map(list, (pattern) ->
        # construct list of cells from grid
        lines = pattern.grid.split(/[\s\n]+/)
        pattern.cells = []
        _.each(lines, (line, y) -> _.each(line.split(""), (char, x) -> pattern.cells.push({x: x, y: y}) if char is "X" ))

        # calculate width & height of the pattern
        pattern.width = lines[0].length
        pattern.height = lines.length
        pattern
    )
)

# build id -> pattern lookup map
root.pattern_map = {}
_.each(_.flatten(_.toArray(_(patterns).values())), (pattern) -> root.pattern_map[pattern.id] = pattern)
