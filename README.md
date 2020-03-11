# D3_choro

<!DOCTYPE html>
<html>
<head>
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>D3: Zoom in to reveal counties</title>
    <style>
        .background {
            fill: none;
            pointer-events: all;
        }

        #states {
            fill: none;
        }

        #states .active {
            display:none;
        }

        #state-borders {
            fill: none;
            stroke: #fff;
            stroke-width: 1px;
            stroke-linejoin: round;
            stroke-linecap: round;
            pointer-events: none;
        }

        .county-boundary {
            fill: 'grey';
            stroke: #fff;
            stroke-width: .5px;
        }

        .county-boundary:hover, .state:hover {
            fill: rgba(121, 121, 121, 0.2);
        }
    </style>
</head>
<body>
    <div class="viz"></div>
    <script type="text/javascript" src="//d3js.org/d3.v5.min.js"></script>
    <script type="text/javascript" src="https://d3js.org/topojson.v3.min.js"></script>
    <script type="text/javascript">

        var x = d3.scaleLinear()
            .domain([1, 10])
            .rangeRound([600, 860]);

        var color = d3.scaleThreshold()
            .domain(d3.range(0, 9))
            .range(d3.schemeBlues[9]);

        var margin = {
            top: 10,
            bottom: 10,
            left: 10,
            right:10
        }, width = parseInt(d3.select('.viz').style('width'))
            , width = width - margin.left - margin.right
            , mapRatio = 0.5
            , height = width * mapRatio
            , active = d3.select(null);

        var svg = d3.select('.viz').append('svg')
            .attr('height', height + margin.top + margin.bottom)
            .attr('width', width + margin.left + margin.right);

        svg.append('rect')
            .attr('class', 'background center-container')
            .attr('height', height + margin.top + margin.bottom)
            .attr('width', width + margin.left + margin.right)
            .on('click', clicked);

        Promise.resolve(d3.json("https://gist.githubusercontent.com/mbostock/4090846/raw/d534aba169207548a8a3d670c9c2cc719ff05c47/us.json")
        ).then(ready)

        var promises = [
        d3.tsv("us-state-names.tsv", function(d) {
            stateNames.set(d.id, d.name)
        }),
        d3.tsv("us-county-names.tsv", function(d) {
            countyNames.set(d.id, d.name)
        }),
        d3.tsv("cases.tsv", function(d) { 
            cases.set(d.name, +d.value); 
        })
        ]
        Promise.all(promises).then(ready)

        var projection = d3.geoAlbersUsa()
            .translate([width /2 , height / 2])
            .scale(width);

        var path = d3.geoPath()
            .projection(projection);

        var g = svg.append("g")
            .attr('class', 'center-container center-items us-state')
            .attr('transform', 'translate('+margin.left+','+margin.top+')')
            .attr('width', width + margin.left + margin.right)
            .attr('height', height + margin.top + margin.bottom)

        function ready(us) {

            g.append("g")
                .attr("id", "counties")
                .selectAll("path")
                .data(topojson.feature(us, us.objects.counties).features)
                .enter().append("path")
                .attr("d", path)
                .attr('fill', color(2))
                .attr("class", "county-boundary")
                .on("click", reset);

            g.append("g")
                .attr("id", "states")
                .selectAll("path")
                .data(topojson.feature(us, us.objects.states).features)
                .enter().append("path")
                .attr("d", path)
                .attr('fill', color(1))
                .attr("class", "state")
                .on("click", clicked);


            g.append("path")
                .datum(topojson.mesh(us, us.objects.states, function(a, b) { return a !== b; }))
                .attr("id", "state-borders")
                .attr("d", path);

        }

        function clicked(d) {
            if (d3.select('.background').node() === this) return reset();

            if (active.node() === this) return reset();

            active.classed("active", false);
            active = d3.select(this).classed("active", true);

            var bounds = path.bounds(d),
                dx = bounds[1][0] - bounds[0][0],
                dy = bounds[1][1] - bounds[0][1],
                x = (bounds[0][0] + bounds[1][0]) / 2,
                y = (bounds[0][1] + bounds[1][1]) / 2,
                scale = .9 / Math.max(dx / width, dy / height),
                translate = [width / 2 - scale * x, height / 2 - scale * y];

            g.transition()
                .duration(750)
                .style("stroke-width", 1.5 / scale + "px")
                .attr("transform", "translate(" + translate + ")scale(" + scale + ")");
        }


        function reset() {
            active.classed("active", false);
            active = d3.select(null);

            g.transition()
                .delay(100)
                .duration(750)
                .style("stroke-width", "1.5px")
                .attr('transform', 'translate('+margin.left+','+margin.top+')');

        }
    </script>
</body>
</html>
