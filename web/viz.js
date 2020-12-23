'use strict';

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

function Button(props) {
  return React.createElement(
    "rect",
    { width: 20, height: 10 },
    React.createElement(
      "text",
      null,
      props.text
    )
  );
}

function GridLines(props) {
  var xLines = props.scales.x.ticks().map(function (t, i) {
    var xVal = props.scales.x(t);
    var yStart = props.scales.y.range()[0];
    var yEnd = props.scales.y.range()[1];
    return React.createElement("path", {
      key: i,
      stroke: "lightgray",
      strokeDasharray: "2,2",
      d: "M " + xVal + " " + yStart + " V " + yEnd
    });
  });
  var yLines = props.scales.y.ticks().map(function (t, i) {
    var yVal = props.scales.y(t);
    var xStart = props.scales.x.range()[0];
    var xEnd = props.scales.x.range()[1];
    return React.createElement("path", {
      key: i,
      stroke: "lightgray",
      strokeDasharray: "2,2",
      d: "M " + xStart + " " + yVal + " H " + xEnd
    });
  });
  return React.createElement(
    "g",
    null,
    xLines,
    yLines
  );
}

function LeftAxis(props) {
  var format = props.scale.tickFormat();

  var ticks = props.scale.ticks().map(function (value) {
    return {
      value: format(value),
      yOffset: props.scale(value)
    };
  });

  return React.createElement(
    "g",
    { transform: "translate(" + props.spacing.left + " , 0)" },
    React.createElement("path", { d: "M0," + (props.spacing.height - props.spacing.bottom) + "L0," + props.spacing.top,
      stroke: "currentColor" }),
    ticks.map(function (d, i) {
      return React.createElement(
        "g",
        { transform: "translate(0, " + d.yOffset + ")", key: i },
        React.createElement("line", { x2: "-3", stroke: "black" }),
        React.createElement(
          "text",
          { style: {
              textAnchor: "end",
              fontSize: "10px",
              transform: "translateX(-5px)",
              color: "black"
            } },
          d.value
        )
      );
    }),
    React.createElement("rect", {
      width: 40, height: props.spacing.height - props.spacing.bottom, x: -40,
      onClick: props.callback,
      style: { fill: "rgba(0,0,0,0)" } })
  );
}

function BottomAxis(props) {
  var format = props.scale.tickFormat();

  var ticks = props.scale.ticks().map(function (value) {
    return {
      value: format(value),
      xOffset: props.scale(value)
    };
  });

  var startScale = props.scale.range()[0];
  var endScale = props.scale.range()[1];

  return React.createElement(
    "g",
    { transform: "translate(0, " + (props.spacing.height - props.spacing.bottom) + ")" },
    React.createElement("path", { d: "M " + startScale + " 0.5 H " + endScale,
      stroke: "currentColor" }),
    ticks.map(function (d, i) {
      return React.createElement(
        "g",
        { transform: "translate(" + d.xOffset + ", 0)", key: i },
        React.createElement("line", { y2: "3", stroke: "black" }),
        React.createElement(
          "text",
          { style: {
              textAnchor: "middle",
              fontSize: "10px",
              transform: "translateY(14px)",
              color: "black"
            } },
          d.value
        )
      );
    }),
    React.createElement("rect", {
      height: 40, width: props.spacing.width - props.spacing.right, x: props.spacing.left,
      onClick: props.callback,
      style: { fill: "rgba(0,0,0,0)" } })
  );
}

var bestDirection = {
  "recall": "max",
  "epsilon": "max",
  "largeepsilon": "max",
  "qps": "max",
  "rel": "min",
  "indexsize": "min",
  "distcomps": "min"
};

var dependentMetrics = {
  "recall": false,
  "epsilon": false,
  "largeepsilon": false,
  "rel": false,
  "qps": true,
  "indexsize": true,
  "distcomps": true
};

var pointComparators = Object.freeze({
  topRight: function topRight(a, b) {
    return b[0] < a[0] ? -1 : b[0] > a[0] ? 1 : b[1] < a[1] ? -1 : b[1] > a[1] ? 1 : 0;
  },
  topLeft: function topLeft(a, b) {
    return a[0] < b[0] ? -1 : a[0] > b[0] ? 1 : a[1] < b[1] ? 1 : a[1] > b[1] ? -1 : 0;
  },
  bottomRight: function bottomRight(a, b) {
    return b[0] < a[0] ? -1 : b[0] > a[0] ? 1 : b[1] < a[1] ? 1 : b[1] > a[1] ? -1 : 0;
  },
  bottomLeft: function bottomLeft(a, b) {
    return a[0] < b[0] ? -1 : a[0] > b[0] ? 1 : a[1] < b[1] ? -1 : a[1] > b[1] ? 1 : 0;
  }
});

// Adapted from https://github.com/justinormont/pareto-frontier/blob/e27fe4da2055186dafd3ff7d2ecfa843d8b40ae5/index.js
function paretoFrontier(points, aesX, aesY) {
  var bestX = bestDirection[aesX];
  var bestY = bestDirection[aesY];

  var toPoint = function toPoint(a) {
    return [a[aesX], a[aesY]];
  };

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
  var pointComparator = function pointComparator(a, b) {
    return inner(toPoint(a), toPoint(b));
  };

  var findMax = bestY === "max";

  var sorted = Array.from(points).sort(pointComparator);

  var last = void 0;
  return sorted.filter(function (p, i) {
    var y = p[aesY];
    if (i === 0 || findMax && y > last || !findMax && y < last) {
      last = y;return true;
    }
    return false;
  });
}

function Circles(props) {
  // console.log(props.data[0]);
  return React.createElement(
    "g",
    null,
    props.data.map(function (d) {
      var cx = props.scales.x(d[props.aes.x]);
      var cy = props.scales.y(d[props.aes.y]);
      return React.createElement("circle", {
        key: d["id"],
        r: props.highlighted.indexOf(d.parameters) >= 0 ? 6 : props.radius,
        cx: cx,
        cy: cy,
        fill: props.scales.color(d[props.aes.color]),
        onMouseOver: function onMouseOver(e) {
          return props.highlightedCallback(d, true, e.clientX, e.clientY);
        },
        onMouseOut: function onMouseOut(e) {
          return props.highlightedCallback(d, false, e.clientX, e.clientY);
        }
      });
    })
  );
}

function Lines(props) {
  var line = d3.line().x(function (d) {
    return props.scales.x(d[props.aes.x]);
  }).y(function (d) {
    return props.scales.y(d[props.aes.y]);
  });
  var paths = d3.groups(props.data, function (d) {
    return d[props.aes.color];
  }).map(function (pair) {
    var d = line(pair[1]);
    return React.createElement("path", {
      key: pair[0],
      fill: "none",
      strokeWidth: 1.5,
      strokeLinejoin: "round",
      stroke: props.scales.color(pair[0]),
      d: d
    });
  });
  return React.createElement(
    "g",
    null,
    paths
  );
}

function ColorLegend(props) {
  return React.createElement(
    "div",
    { className: "flex flex-wrap space-x-4 place-content-center" },
    Array.from(props.levels).map(function (d, i) {
      return React.createElement(
        "div",
        { key: i, className: "flex flex-row space-x-2" },
        React.createElement("div", {
          style: {
            width: "20px",
            height: "20px",
            background: props.scale(d)
          }
        }),
        React.createElement(
          "span",
          null,
          d
        )
      );
    })
  );
}

function ParetoPlot(props) {

  var computeFrontier = dependentMetrics[props.aes.x] && !dependentMetrics[props.aes.y] || !dependentMetrics[props.aes.x] && dependentMetrics[props.aes.y];

  var plotData = undefined;
  if (computeFrontier) {
    plotData = d3.groups(props.data, function (d) {
      return d[props.aes.color];
    }).flatMap(function (p) {
      return paretoFrontier(p[1], props.aes.x, props.aes.y);
    });
  } else {
    plotData = props.data;
  }

  return React.createElement(
    "div",
    { className: "w-full" },
    React.createElement(ColorLegend, { levels: d3.union(props.data.map(function (d) {
        return d[props.aes.color];
      })), scale: props.scales.color }),
    React.createElement(
      "svg",
      { viewBox: "0 0 " + props.spacing.width + " " + props.spacing.height },
      React.createElement(GridLines, { scales: props.scales }),
      React.createElement(Circles, {
        data: plotData,
        aes: props.aes,
        scales: props.scales,
        radius: 2,
        highlighted: props.highlighted,
        highlightedCallback: props.highlightedCallback
      }),
      computeFrontier && React.createElement(Lines, {
        data: plotData,
        aes: props.aes,
        scales: props.scales
      }),
      React.createElement(Circles, {
        data: props.data,
        aes: props.aes,
        scales: props.scales,
        radius: 1,
        highlighted: props.highlighted,
        highlightedCallback: props.highlightedCallback
      }),
      React.createElement(BottomAxis, {
        scale: props.scales.x,
        spacing: props.spacing,
        callback: props.xAxisCallback
      }),
      React.createElement(LeftAxis, {
        scale: props.scales.y,
        spacing: props.spacing,
        callback: props.yAxisCallback
      })
    )
  );
}

var FixedFactorSelector = function (_React$Component) {
  _inherits(FixedFactorSelector, _React$Component);

  function FixedFactorSelector(props) {
    _classCallCheck(this, FixedFactorSelector);

    return _possibleConstructorReturn(this, (FixedFactorSelector.__proto__ || Object.getPrototypeOf(FixedFactorSelector)).call(this, props));
  }

  _createClass(FixedFactorSelector, [{
    key: "render",
    value: function render() {
      return React.createElement(
        "label",
        null,
        "Select ",
        this.props.label,
        React.createElement(
          "select",
          { className: "border-gray-200 border-2 round-lg", value: this.props.level, onChange: this.props.handleChange },
          this.props.levels.map(function (v) {
            return React.createElement(
              "option",
              { key: v, value: v },
              v
            );
          })
        )
      );
    }
  }]);

  return FixedFactorSelector;
}(React.Component);

var AestheticsSelector = function (_React$Component2) {
  _inherits(AestheticsSelector, _React$Component2);

  function AestheticsSelector(props) {
    _classCallCheck(this, AestheticsSelector);

    return _possibleConstructorReturn(this, (AestheticsSelector.__proto__ || Object.getPrototypeOf(AestheticsSelector)).call(this, props));
  }

  _createClass(AestheticsSelector, [{
    key: "render",
    value: function render() {
      return React.createElement(
        "label",
        null,
        "Select ",
        this.props.label,
        React.createElement(
          "select",
          { className: "border-gray-200 border-2 round-lg", value: this.props.value, onChange: this.props.handleChange },
          this.props.aesValues.map(function (aes) {
            return React.createElement(
              "option",
              { key: aes.name, value: aes.name },
              aes.label
            );
          })
        )
      );
    }
  }]);

  return AestheticsSelector;
}(React.Component);

var InteractivePlot = function (_React$Component3) {
  _inherits(InteractivePlot, _React$Component3);

  function InteractivePlot(props) {
    _classCallCheck(this, InteractivePlot);

    var _this3 = _possibleConstructorReturn(this, (InteractivePlot.__proto__ || Object.getPrototypeOf(InteractivePlot)).call(this, props));

    _this3.factorDefaults = {
      dataset: "GLOVE",
      algorithm: "ONNG",
      difficulty: "hard",
      difficulty_type: "lid"
    };

    _this3.state = {
      aes: {
        x: "recall",
        y: "qps",
        color: "algorithm"
      },
      factorFiltering: {
        "algorithm": _this3.factorDefaults["algorithm"],
        "dataset": _this3.factorDefaults["dataset"],
        "difficulty": _this3.factorDefaults["difficulty"],
        "difficulty_type": _this3.factorDefaults["difficulty_type"]
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

    _this3.metrics = [{ "name": "build", "label": "Build time", "trans": "log" }, { "name": "distcomps", "label": "Number of distance computations", "trans": "lin" }, { "name": "recall", "label": "Recall", "default": "x", "trans": "lin" }, { "name": "epsilon", "label": "epsilon recall", "trans": "lin" }, { "name": "largeepsilon", "label": "large epsilon recall", "trans": "lin" }, { "name": "indexsize", "label": "Index size", "trans": "lin" }, { "name": "qps", "label": "Queries per second", "trans": "log" }, { "name": "rel", "label": "Relative error", "trans": "lin" }, { "name": "queriessize", "label": "Queries size", "trans": "lin" }];

    _this3.factors = [{ "name": "algorithm", label: "Algorithm" }, { "name": "dataset", label: "Dataset" }, { "name": "difficulty", label: "Difficulty" }, { "name": "difficulty_type", label: "Difficulty type" }];

    _this3.factorLevels = {
      algorithm: Array.from(d3.union(_this3.props.dataset.map(function (d) {
        return d.algorithm;
      }))),
      dataset: Array.from(d3.union(_this3.props.dataset.map(function (d) {
        return d.dataset;
      }))),
      difficulty: Array.from(d3.union(_this3.props.dataset.map(function (d) {
        return d.difficulty;
      }))),
      difficulty_type: Array.from(d3.union(_this3.props.dataset.map(function (d) {
        return d.difficulty_type;
      })))
    };

    _this3.handleXaxisClick = _this3.handleXaxisClick.bind(_this3);
    _this3.handleYaxisClick = _this3.handleYaxisClick.bind(_this3);
    _this3.handleChangeX = _this3.handleChangeX.bind(_this3);
    _this3.handleChangeY = _this3.handleChangeY.bind(_this3);
    _this3.handleChangeColor = _this3.handleChangeColor.bind(_this3);
    _this3.handleFilterChange = _this3.handleFilterChange.bind(_this3);
    return _this3;
  }

  _createClass(InteractivePlot, [{
    key: "buildDataFilter",
    value: function buildDataFilter(active) {
      var factor1 = active[0];
      var factor2 = active[1];
      var factor3 = active[2];
      var level1 = this.state.factorFiltering[active[0]];
      var level2 = this.state.factorFiltering[active[1]];
      var level3 = this.state.factorFiltering[active[2]];
      return function (d) {
        return d[factor1] === level1 && d[factor2] === level2 && d[factor3] === level3;
      };
    }
  }, {
    key: "handleFilterChange",
    value: function handleFilterChange(factor, level) {
      this.setState(function (state, props) {
        state.factorFiltering[factor] = level;
        return state;
      });
    }
  }, {
    key: "handleChangeX",
    value: function handleChangeX(event) {
      this.setState(function (state, props) {
        state.aes.x = event.target.value;
        return state;
      });
    }
  }, {
    key: "handleChangeY",
    value: function handleChangeY(event) {
      this.setState(function (state, props) {
        state.aes.y = event.target.value;
        return state;
      });
    }
  }, {
    key: "handleChangeColor",
    value: function handleChangeColor(event) {
      this.setState(function (state, props) {
        state.aes.color = event.target.value;
        return state;
      });
    }
  }, {
    key: "handleXaxisClick",
    value: function handleXaxisClick(event) {
      this.setState(function (state, props) {
        state.log.x = !state.log.x;
        return state;
      });
    }
  }, {
    key: "handleYaxisClick",
    value: function handleYaxisClick(event) {
      this.setState(function (state, props) {
        state.log.y = !state.log.y;
        return state;
      });
    }
  }, {
    key: "render",
    value: function render() {
      var _this4 = this;

      var fixedFactors = this.factors.filter(function (f) {
        return f.name != _this4.state.aes.color;
      });
      var flt = this.buildDataFilter(fixedFactors.map(function (f) {
        return f.name;
      }));
      var filtered = this.props.dataset.filter(flt);
      var spacing = this.state.spacing;
      var aes = this.state.aes;
      var scales = {
        x: (this.state.log.x ? d3.scaleLog() : d3.scaleLinear()).domain([d3.min(filtered, function (d) {
          return d[aes.x];
        }), d3.max(filtered, function (d) {
          return d[aes.x];
        })]).range([spacing.left, spacing.width - spacing.right]).nice(),
        y: (this.state.log.y ? d3.scaleLog() : d3.scaleLinear()).domain([d3.min(filtered, function (d) {
          return d[aes.y];
        }), d3.max(filtered, function (d) {
          return d[aes.y];
        })]).range([spacing.height - spacing.bottom, spacing.top]).nice(),
        color: d3.scaleOrdinal(d3.schemeTableau10).domain([d3.min(filtered, function (d) {
          return d[aes.color];
        }), d3.max(filtered, function (d) {
          return d[aes.color];
        })])
      };

      var chart = React.createElement(ParetoPlot, {
        spacing: spacing,
        data: filtered,
        aes: aes,
        scales: scales,
        xAxisCallback: this.handleXaxisClick,
        yAxisCallback: this.handleYaxisClick,
        highlighted: this.props.highlighted,
        highlightedCallback: this.props.highlightedCallback
      });

      return React.createElement(
        "div",
        null,
        React.createElement(
          "div",
          { className: "mx-auto flex flex-col space-y-2 mb-8" },
          React.createElement(AestheticsSelector, {
            label: "X",
            value: aes.x,
            aesValues: this.metrics,
            handleChange: this.handleChangeX
          }),
          React.createElement(AestheticsSelector, {
            label: "Y",
            value: aes.y,
            aesValues: this.metrics,
            handleChange: this.handleChangeY
          }),
          React.createElement(AestheticsSelector, {
            label: "color",
            value: aes.color,
            aesValues: this.factors,
            handleChange: this.handleChangeColor
          }),
          fixedFactors.map(function (f, i) {
            return React.createElement(FixedFactorSelector, {
              key: i,
              handleChange: function handleChange(e) {
                return _this4.handleFilterChange(f.name, e.target.value);
              },
              label: f.label,
              level: _this4.state.factorFiltering[f.name],
              levels: _this4.factorLevels[f.name] });
          })
        ),
        chart
      );
    }
  }]);

  return InteractivePlot;
}(React.Component);

function Tooltip(props) {
  if (props.visible) {
    var num = d3.format(".2f");
    return React.createElement(
      "div",
      {
        className: "bg-gray-800 p-3 rounded-xl text-gray-200 flex flex-col",
        style: {
          left: props.position[0] + 10 + "px",
          top: props.position[1] + 10 + "px",
          position: "absolute"
        } },
      ["recall", "rel", "qps", "distcomps", "indexsize"].map(function (m) {
        return React.createElement(
          "span",
          null,
          React.createElement(
            "span",
            { className: "text-gray-400" },
            m,
            ": "
          ),
          num(props.data[m])
        );
      })
    );
  } else {
    return React.createElement("div", null);
  }
}

var TwoPane = function (_React$Component4) {
  _inherits(TwoPane, _React$Component4);

  function TwoPane(props) {
    _classCallCheck(this, TwoPane);

    var _this5 = _possibleConstructorReturn(this, (TwoPane.__proto__ || Object.getPrototypeOf(TwoPane)).call(this, props));

    _this5.state = {
      highlighted: [],
      tooltip: {
        position: [0, 0],
        visible: false,
        data: {}
      }
    };

    _this5.handleHighlighted = _this5.handleHighlighted.bind(_this5);
    return _this5;
  }

  _createClass(TwoPane, [{
    key: "handleHighlighted",
    value: function handleHighlighted(point, over, x, y) {
      if (over) {
        this.setState(function (state, props) {
          state.highlighted = [point.parameters];
          state.tooltip.position = [x, y];
          state.tooltip.visible = true;
          state.tooltip.data = point;
          return state;
        });
      } else {
        this.setState(function (state, props) {
          state.highlighted = [];
          state.tooltip.visible = false;
          return state;
        });
      }
    }
  }, {
    key: "render",
    value: function render() {
      return React.createElement(
        "div",
        null,
        React.createElement(Tooltip, {
          visible: this.state.tooltip.visible,
          position: this.state.tooltip.position,
          data: this.state.tooltip.data
        }),
        React.createElement(
          "div",
          { className: "grid grid-cols-2 gap-5" },
          React.createElement(InteractivePlot, {
            dataset: this.props.data,
            highlightedCallback: this.handleHighlighted,
            highlighted: this.state.highlighted }),
          React.createElement(InteractivePlot, {
            dataset: this.props.data,
            highlightedCallback: this.handleHighlighted,
            highlighted: this.state.highlighted })
        )
      );
    }
  }]);

  return TwoPane;
}(React.Component);

d3.csv("data/summarised.csv", d3.autoType).then(function (d) {
  var data = d.map(function (d, i) {
    d["id"] = "_" + i;
    return d;
  });
  var container = document.querySelector('#viz-container');

  ReactDOM.render(React.createElement(TwoPane, { data: data }), container);
});