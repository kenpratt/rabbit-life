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
            id: "pattern-w-lwss"
            name: "Spaceship (E)"
            grid: "__XX_
                   _XXXX
                   XX_XX
                   _XX__"
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
        # turn grids into nested boolean lists
        lines = pattern.grid.split(/[\s\n]+/)
        pattern.grid = _.map(lines, (line) -> _.map(line.split(""), (char) -> char == "X"))

        # calculate width & height of the pattern
        pattern.width = lines[0].length
        pattern.height = lines.length
        pattern
    )
)

# build id -> pattern lookup map
root.pattern_map = {}
_.each(_.flatten(_.toArray(_(patterns).values())), (pattern) -> root.pattern_map[pattern.id] = pattern)
