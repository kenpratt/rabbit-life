root = this

raw_patterns = {
    "Still lives": [
        {
            id: "pattern-block",
            name: "Block",
            grid: "XX
                   XX"
        }
    ],
    "Oscillators": [
        {
            id: "pattern-blinker",
            name: "Blinker",
            grid: "XXX"
        }
    ],
    "Spaceships": [
        {
            id: "pattern-se-glider",
            name: "Glider (SE)",
            grid: "__X
                   X_X
                   _XX"
        },
        {
            id: "pattern-sw-glider",
            name: "Glider (SW)",
            grid: "_X_
                   X__
                   XXX"
        },
        {
            id: "pattern-nw-glider",
            name: "Glider (NW)",
            grid: "XX_
                   X_X
                   X__"
        },
        {
            id: "pattern-ne-glider",
            name: "Glider (NE)",
            grid: "XXX
                   __X
                   _X_"
        },
        {
            id: "pattern-w-lwss",
            name: "Lightweight spaceship (W)",
            grid: "_XX__
                   XXXX_
                   XX_XX
                   __XX_"
        },
        {
            id: "pattern-w-lwss",
            name: "Lightweight spaceship (E)",
            grid: "__XX_
                   _XXXX
                   XX_XX
                   _XX__"
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
