'use strict';

function Button(props) {
  return <rect width={20} height={10}>
    <text>{props.text}</text>
  </rect>;
}

function LeftAxis(props) {

  const ticks = props.scale.ticks()
    .map(value => ({ value, yOffset: props.scale(value) }));

  return <g transform={`translate(${props.spacing.left} , 0)`}>
    <path d={`M0,${props.spacing.height - props.spacing.bottom}L0,${props.spacing.top}`}
      stroke="currentColor" />
    {
      ticks.map((d, i) => {
        return <g transform={"translate(0, " + (d.yOffset) + ")"} key={i}>
          <line x2="-3" stroke="black" />
          <text style={{
            textAnchor: "end",
            fontSize: "10px",
            transform: "translateX(-5px)",
            color: "black"
          }}>
            {d.value}
          </text>
        </g>
      })
    }
    <rect
      width={40} height={props.spacing.height - props.spacing.bottom} x={-40}
      onClick={props.callback}
      style={{ fill: "rgba(0,0,0,0)" }} />
  </g>;
}

function BottomAxis(props) {

  const ticks = props.scale.ticks()
    .map(value => ({ value, xOffset: props.scale(value) }));

  const startScale = props.scale.range()[0];
  const endScale = props.scale.range()[1];

  return <g transform={`translate(0, ${props.spacing.height - props.spacing.bottom})`}>
    <path d={"M " + startScale + " 0.5 H " + endScale}
      stroke="currentColor" />
    {
      ticks.map((d, i) => {
        return <g transform={"translate(" + d.xOffset + ", 0)"} key={i}>
          <line y2="3" stroke="black" />
          <text style={{
            textAnchor: "middle",
            fontSize: "10px",
            transform: "translateY(14px)",
            color: "black"
          }}>
            {d.value}
          </text>
        </g>
      })
    }
    <rect
      height={40} width={props.spacing.width - props.spacing.right} x={props.spacing.left}
      onClick={props.callback}
      style={{ fill: "rgba(0,0,0,0)" }} />
  </g>;
}

const bestDirection = {
  "recall": "max",
  "epsilon": "max",
  "largeepsilon": "max",
  "qps": "max",
  "rel": "min",
  "indexsize": "min",
  "distcomps": "min"
};

const dependentMetrics = {
  "recall": false,
  "epsilon": false,
  "largeepsilon": false,
  "rel": false,
  "qps": true,
  "indexsize": true,
  "distcomps": true
}

const pointComparators = Object.freeze({
  topRight: (a, b) => (b[0] < a[0] ? -1 : (b[0] > a[0] ? 1 : (b[1] < a[1] ? -1 : (b[1] > a[1] ? 1 : 0)))),
  topLeft: (a, b) => (a[0] < b[0] ? -1 : (a[0] > b[0] ? 1 : (a[1] < b[1] ? 1 : (a[1] > b[1] ? -1 : 0)))),
  bottomRight: (a, b) => (b[0] < a[0] ? -1 : (b[0] > a[0] ? 1 : (b[1] < a[1] ? 1 : (b[1] > a[1] ? -1 : 0)))),
  bottomLeft: (a, b) => (a[0] < b[0] ? -1 : (a[0] > b[0] ? 1 : (a[1] < b[1] ? -1 : (a[1] > b[1] ? 1 : 0)))),
});

// Adapted from https://github.com/justinormont/pareto-frontier/blob/e27fe4da2055186dafd3ff7d2ecfa843d8b40ae5/index.js
function paretoFrontier(points, aesX, aesY) {
  const bestX = bestDirection[aesX];
  const bestY = bestDirection[aesY];

  const toPoint = (a) => [a[aesX], a[aesY]];

  var inner = null;
  if (bestX === "max" && bestY === "max") {
    inner = pointComparators.topRight;
  } else if (bestX === "max" && bestY === "min") {
    inner = pointComparators.bottomRight;
  } else if (bestX === "min" && bestY === "min") {
    inner = pointComparators.bottomLeft;
  } else {
    inner = pointComparators.topLeft;
  }
  const pointComparator = (a, b) => inner(toPoint(a), toPoint(b));

  const findMax = !(pointComparator([0, 1], [0, 0]) < 0); // Optimize +y

  const sorted = Array.from(points).sort(pointComparator);

  let last;
  return sorted.filter((p, i) => {
    const y = p[aesY];
    if (i === 0 || findMax && y > last || !findMax && y < last) { last = y; return true; }
    return false;
  });
}

function Circles(props) {
  // console.log(props.data[0]);
  return (
    <g>
      {props.data.map(function (d) {
        const cx = props.scales.x(d[props.aes.x]);
        const cy = props.scales.y(d[props.aes.y]);
        return <circle
          key={d["id"]}
          r={(props.highlighted.indexOf(d.parameters) >= 0) ? 6 : props.radius}
          cx={cx}
          cy={cy}
          fill={props.scales.color(d[props.aes.color])}
          onMouseOver={(e) => props.highlightedCallback(d, true, e.clientX, e.clientY)}
          onMouseOut={(e) => props.highlightedCallback(d, false, e.clientX, e.clientY)}
        />
      })}
    </g>
  );
}

function Lines(props) {
  const line = d3.line()
    .x(d => props.scales.x(d[props.aes.x]))
    .y(d => props.scales.y(d[props.aes.y]));
  const paths = d3.groups(props.data, d => d[props.aes.color]).map(function (pair) {
    const d = line(pair[1]);
    return <path
      key={pair[0]}
      fill={"none"}
      strokeWidth={1.5}
      strokeLinejoin={"round"}
      stroke={props.scales.color(pair[0])}
      d={d}
    />
  });
  return (<g>
    {paths}
  </g>);
}

function ColorLegend(props) {
  return (<div className="flex flex-wrap space-x-4 place-content-center">
    {
      Array.from(props.levels).map((d, i) => {
        return (<div key={i} className="flex flex-row space-x-2">
          <div
            style={{
              width: "20px",
              height: "20px",
              background: props.scale(d)
            }}
          />
          <span>{d}</span>
        </div>)
      })
    }
  </div>);
}

function ParetoPlot(props) {

  const computeFrontier = (dependentMetrics[props.aes.x] && !dependentMetrics[props.aes.y]) || (!dependentMetrics[props.aes.x] && dependentMetrics[props.aes.y]);

  var plotData = undefined;
  if (computeFrontier) {
    plotData = d3.groups(props.data, d => d[props.aes.color]).flatMap(p => paretoFrontier(p[1], props.aes.x, props.aes.y));
  } else {
    plotData = props.data;
  }

  return <div className="w-full">
    <ColorLegend levels={d3.union(props.data.map(d => d[props.aes.color]))} scale={props.scales.color} />
    {/* <svg width={props.spacing.width} height={props.spacing.height}> */}
    <svg viewBox={`0 0 ${props.spacing.width} ${props.spacing.height}`}>
      <Circles
        data={plotData}
        aes={props.aes}
        scales={props.scales}
        radius={2}
        highlighted={props.highlighted}
        highlightedCallback={props.highlightedCallback}
      />
      {computeFrontier && <Lines
        data={plotData}
        aes={props.aes}
        scales={props.scales}
      />}
      <Circles
        data={props.data}
        aes={props.aes}
        scales={props.scales}
        radius={1}
        highlighted={props.highlighted}
        highlightedCallback={props.highlightedCallback}
      />
      <BottomAxis
        scale={props.scales.x}
        spacing={props.spacing}
        callback={props.xAxisCallback}
      />
      <LeftAxis
        scale={props.scales.y}
        spacing={props.spacing}
        callback={props.yAxisCallback}
      />
    </svg>
  </div>
}

class FixedFactorSelector extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <label>
        Select {this.props.label}
        <select className="border-gray-200 border-2 round-lg" value={this.props.level} onChange={this.props.handleChange}>
          {
            this.props.levels.map(v => {
              return <option key={v} value={v}>{v}</option>
            })
          }
        </select>
      </label>
    );
  }
}

class AestheticsSelector extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <label>
        Select {this.props.label}
        <select className="border-gray-200 border-2 round-lg" value={this.props.value} onChange={this.props.handleChange}>
          {
            this.props.aesValues.map(aes => {
              return <option key={aes.name} value={aes.name}>{aes.label}</option>
            })
          }
        </select>
      </label>
    );
  }
}

class InteractivePlot extends React.Component {
  constructor(props) {
    super(props);

    this.factorDefaults = {
      dataset: "GLOVE",
      algorithm: "ONNG",
      difficulty: "hard",
      difficulty_type: "lid"
    }

    this.state = {
      aes: {
        x: "recall",
        y: "qps",
        color: "algorithm"
      },
      factorFiltering: {
        "algorithm": this.factorDefaults["algorithm"],
        "dataset": this.factorDefaults["dataset"],
        "difficulty": this.factorDefaults["difficulty"],
        "difficulty_type": this.factorDefaults["difficulty_type"]
      },
      log: {
        x: false,
        y: true
      },
      spacing: {
        width: 500,
        height: 400,
        top: 10,
        right: 10,
        bottom: 50,
        left: 50
      }
    };

    this.metrics = [
      { "name": "build", "label": "Build time", "trans": "log" },
      { "name": "distcomps", "label": "Number of distance computations", "trans": "lin" },
      { "name": "recall", "label": "Recall", "default": "x", "trans": "lin" },
      { "name": "epsilon", "label": "epsilon recall", "trans": "lin" },
      { "name": "largeepsilon", "label": "large epsilon recall", "trans": "lin" },
      { "name": "indexsize", "label": "Index size", "trans": "lin" },
      { "name": "qps", "label": "Queries per second", "trans": "log" },
      { "name": "rel", "label": "Relative error", "trans": "lin" },
      { "name": "queriessize", "label": "Queries size", "trans": "lin" }
    ];

    this.factors = [
      { "name": "algorithm", label: "Algorithm" },
      { "name": "dataset", label: "Dataset" },
      { "name": "difficulty", label: "Difficulty" },
      { "name": "difficulty_type", label: "Difficulty type" }
    ]

    this.factorLevels = {
      algorithm: Array.from(d3.union(this.props.dataset.map(d => d.algorithm))),
      dataset: Array.from(d3.union(this.props.dataset.map(d => d.dataset))),
      difficulty: Array.from(d3.union(this.props.dataset.map(d => d.difficulty))),
      difficulty_type: Array.from(d3.union(this.props.dataset.map(d => d.difficulty_type)))
    };

    this.handleXaxisClick = this.handleXaxisClick.bind(this);
    this.handleYaxisClick = this.handleYaxisClick.bind(this);
    this.handleChangeX = this.handleChangeX.bind(this);
    this.handleChangeY = this.handleChangeY.bind(this);
    this.handleChangeColor = this.handleChangeColor.bind(this);
    this.handleFilterChange = this.handleFilterChange.bind(this);
  }

  buildDataFilter(active) {
    const factor1 = active[0];
    const factor2 = active[1];
    const factor3 = active[2];
    const level1 = this.state.factorFiltering[active[0]];
    const level2 = this.state.factorFiltering[active[1]];
    const level3 = this.state.factorFiltering[active[2]];
    return (d) => {
      return d[factor1] === level1 && d[factor2] === level2 && d[factor3] === level3;
    }
  }

  handleFilterChange(factor, level) {
    this.setState((state, props) => {
      state.factorFiltering[factor] = level;
      return state;
    });
  }

  handleChangeX(event) {
    this.setState((state, props) => {
      state.aes.x = event.target.value;
      return state;
    });
  }

  handleChangeY(event) {
    this.setState((state, props) => {
      state.aes.y = event.target.value;
      return state;
    });
  }

  handleChangeColor(event) {
    this.setState((state, props) => {
      state.aes.color = event.target.value;
      return state;
    });
  }

  handleXaxisClick(event) {
    this.setState((state, props) => {
      state.log.x = !state.log.x;
      return state
    });
  }

  handleYaxisClick(event) {
    this.setState((state, props) => {
      state.log.y = !state.log.y;
      return state
    });
  }

  render() {
    const fixedFactors = this.factors.filter(f => f.name != this.state.aes.color);
    const flt = this.buildDataFilter(fixedFactors.map(f => f.name));
    const filtered = this.props.dataset.filter(flt);
    const spacing = this.state.spacing;
    const aes = this.state.aes;
    const scales = {
      x: ((this.state.log.x) ? d3.scaleLog() : d3.scaleLinear())
        .domain([
          d3.min(filtered, d => d[aes.x]),
          d3.max(filtered, d => d[aes.x])
        ]).range([spacing.left, spacing.width - spacing.right])
        .nice(),
      y: ((this.state.log.y) ? d3.scaleLog() : d3.scaleLinear())
        .domain([
          d3.min(filtered, d => d[aes.y]),
          d3.max(filtered, d => d[aes.y])
        ]).range([spacing.height - spacing.bottom, spacing.top])
        .nice(),
      color: d3.scaleOrdinal(d3.schemeTableau10).domain([
        d3.min(filtered, (d) => d[aes.color]),
        d3.max(filtered, (d) => d[aes.color])
      ])
    };

    const chart = <ParetoPlot
      spacing={spacing}
      data={filtered}
      aes={aes}
      scales={scales}
      xAxisCallback={this.handleXaxisClick}
      yAxisCallback={this.handleYaxisClick}
      highlighted={this.props.highlighted}
      highlightedCallback={this.props.highlightedCallback}
    />;

    return (
      <div>
        <div className="mx-auto flex flex-col space-y-2 mb-8">
          <AestheticsSelector
            label={"X"}
            value={aes.x}
            aesValues={this.metrics}
            handleChange={this.handleChangeX}
          />
          <AestheticsSelector
            label={"Y"}
            value={aes.y}
            aesValues={this.metrics}
            handleChange={this.handleChangeY}
          />
          <AestheticsSelector
            label={"color"}
            value={aes.color}
            aesValues={this.factors}
            handleChange={this.handleChangeColor}
          />
          {
            fixedFactors.map((f, i) => {
              return <FixedFactorSelector
                key={i}
                handleChange={e => this.handleFilterChange(f.name, e.target.value)}
                label={f.label}
                level={this.state.factorFiltering[f.name]}
                levels={this.factorLevels[f.name]} />;
            })
          }
        </div>
        {chart}
      </div>
    );
  }
}

function Tooltip(props) {
  if (props.visible) {
    const num = d3.format(".2f");
    return (
      <div
        className="bg-gray-800 p-3 rounded-xl text-gray-200 flex flex-col"
        style={{
          left: props.position[0] + 10 + "px",
          top: props.position[1] + 10 + "px",
          position: "absolute"
        }}>
        {
          ["recall", "rel", "qps", "distcomps", "indexsize"].map(m => {
            return <span>
              <span className="text-gray-400">{m}: </span>{num(props.data[m])}
            </span>;
          })
        }
      </div>
    );
  } else {
    return <div></div>;
  }
}

class TwoPane extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      highlighted: [],
      tooltip: {
        position: [0, 0],
        visible: false,
        data: {}
      }
    };

    this.handleHighlighted = this.handleHighlighted.bind(this);
  }

  handleHighlighted(point, over, x, y) {
    if (over) {
      this.setState((state, props) => {
        state.highlighted = [point.parameters];
        state.tooltip.position = [x, y];
        state.tooltip.visible = true;
        state.tooltip.data = point;
        return state;
      });
    } else {
      this.setState((state, props) => {
        state.highlighted = [];
        state.tooltip.visible = false;
        return state;
      });
    }
  }

  render() {
    return <div>
      <Tooltip
        visible={this.state.tooltip.visible}
        position={this.state.tooltip.position}
        data={this.state.tooltip.data}
      />
      <div className="grid grid-cols-2 gap-5 w-11/12 mx-auto">
        <InteractivePlot
          dataset={this.props.data}
          highlightedCallback={this.handleHighlighted}
          highlighted={this.state.highlighted} />
        <InteractivePlot
          dataset={this.props.data}
          highlightedCallback={this.handleHighlighted}
          highlighted={this.state.highlighted} />
      </div>
    </div>;
  }
}

d3.csv("data/summarised.csv", d3.autoType)
  .then(function (d) {
    const data = d.map((d, i) => {
      d["id"] = "_" + i
      return d;
    });
    const container = document.querySelector('#viz-container');

    ReactDOM.render(<TwoPane data={data} />, container);
  });


