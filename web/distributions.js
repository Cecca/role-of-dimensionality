'use strict';

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

var datasets = ["GLOVE", "GLOVE-2M", "GNEWS", "SIFT", "Fashion-MNIST", "MNIST"];

var difficulties = ["hard", "diverse", "middle", "easy"];

var difficultyTypes = ["lid", "lrc", "expansion"];

var algorithms = ["Annoy", "PUFFINN", "ONNG", "HNSW", "IVF"];

var DataForm = function (_React$Component) {
    _inherits(DataForm, _React$Component);

    function DataForm(props) {
        _classCallCheck(this, DataForm);

        var _this = _possibleConstructorReturn(this, (DataForm.__proto__ || Object.getPrototypeOf(DataForm)).call(this, props));

        _this.state = {
            dataset: props.datasets[0],
            difficulty: props.difficulties[0],
            difficultyType: props.difficultyTypes[0],
            algorithm: props.algorithms[0]
        };

        _this.handleDataset = _this.handleDataset.bind(_this);
        _this.handleDifficulty = _this.handleDifficulty.bind(_this);
        _this.handleDifficultyType = _this.handleDifficultyType.bind(_this);
        _this.handleAlgorithm = _this.handleAlgorithm.bind(_this);
        return _this;
    }

    _createClass(DataForm, [{
        key: "componentDidMount",
        value: function componentDidMount() {
            this.callback();
        }
    }, {
        key: "callback",
        value: function callback() {
            var fname = "data/" + [this.state.algorithm, this.state.dataset, this.state.difficulty, this.state.difficultyType, "json"].join(".");
            this.props.callback(fname);
        }
    }, {
        key: "handleDataset",
        value: function handleDataset(event) {
            this.setState({ dataset: event.target.value }, this.callback);
        }
    }, {
        key: "handleDifficulty",
        value: function handleDifficulty(event) {
            this.setState({ difficulty: event.target.value }, this.callback);
        }
    }, {
        key: "handleDifficultyType",
        value: function handleDifficultyType(event) {
            this.setState({ difficultyType: event.target.value }, this.callback);
        }
    }, {
        key: "handleAlgorithm",
        value: function handleAlgorithm(event) {
            this.setState({ algorithm: event.target.value }, this.callback);
        }
    }, {
        key: "render",
        value: function render() {
            return React.createElement(
                "div",
                { className: "flex flex-col space-y-3" },
                React.createElement(
                    "label",
                    null,
                    "Dataset:",
                    React.createElement(
                        "select",
                        { value: this.state.dataset, onChange: this.handleDataset },
                        this.props.datasets.map(function (d) {
                            return React.createElement(
                                "option",
                                { key: d, value: d },
                                d
                            );
                        })
                    )
                ),
                React.createElement(
                    "label",
                    null,
                    "Difficulty:",
                    React.createElement(
                        "select",
                        { value: this.state.difficulty, onChange: this.handleDifficulty },
                        this.props.difficulties.map(function (d) {
                            return React.createElement(
                                "option",
                                { key: d, value: d },
                                d
                            );
                        })
                    )
                ),
                React.createElement(
                    "label",
                    null,
                    "Difficulty type:",
                    React.createElement(
                        "select",
                        { value: this.state.difficultyType, onChange: this.handleDifficultyType },
                        this.props.difficultyTypes.map(function (d) {
                            return React.createElement(
                                "option",
                                { key: d, value: d },
                                d
                            );
                        })
                    )
                ),
                React.createElement(
                    "label",
                    null,
                    "Algorithm",
                    React.createElement(
                        "select",
                        { value: this.state.handleAlgorithm, onChange: this.handleAlgorithm },
                        this.props.algorithms.map(function (d) {
                            return React.createElement(
                                "option",
                                { key: d, value: d },
                                d
                            );
                        })
                    )
                )
            );
        }
    }]);

    return DataForm;
}(React.Component);

function GridLines(props) {
    var xLines = props.scales.xMajor.ticks().map(function (t, i) {
        var xVal = props.scales.xMajor(t);
        var yStart = props.scales.yMajor.range()[0];
        var yEnd = props.scales.yMajor.range()[1];
        return React.createElement("path", {
            key: i,
            stroke: "lightgray",
            strokeDasharray: "2,2",
            d: "M " + xVal + " " + yStart + " V " + yEnd
        });
    });
    var yLines = props.scales.yMajor.ticks().map(function (t, i) {
        var yVal = props.scales.yMajor(t);
        var xStart = props.scales.xMajor.range()[0];
        var xEnd = props.scales.xMajor.range()[1];
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

    var ticks = props.scale.ticks().filter(function (value) {
        return [1, 10, 100, 1000, 10000, 100000].indexOf(value) >= 0;
    }).map(function (value) {
        return { value: value, yOffset: props.scale(value) };
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
        })
    );
}

function BottomAxis(props) {

    var ticks = props.scale.ticks().map(function (value) {
        return { value: value, xOffset: props.scale(value) };
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
        })
    );
}

function Plot(props) {
    var topDistributionLine = d3.line().x(function (d) {
        return props.scales.xMajor(d.x);
    }).y(function (d) {
        return props.scales.yMinor(d.y);
    });
    var rightDistributionLine = d3.line().x(function (d) {
        return props.scales.xMinor(d.y);
    }).y(function (d) {
        return props.scales.yMajor(d.x);
    });

    return React.createElement(
        "div",
        { className: "w-full" },
        React.createElement(
            "svg",
            { viewBox: "0 0 " + props.spacing.width + " " + props.spacing.height },
            React.createElement(GridLines, { scales: props.scales }),

            // Highlighted circle
            props.highlighted.map(function (d) {
                return React.createElement("circle", {
                    key: d.id,
                    cx: props.scales.xMajor(d.recall),
                    cy: props.scales.yMajor(d.qps),
                    r: 4,
                    fill: "hotpink"
                });
            }),

            // Circles
            props.data.map(function (d) {
                return React.createElement("circle", {
                    key: d.id,
                    cx: props.scales.xMajor(d.recall),
                    cy: props.scales.yMajor(d.qps),
                    r: 2,
                    onMouseOver: function onMouseOver(e) {
                        return props.highlightedCallback(d);
                    }
                });
            }),

            // Top distribution
            props.highlighted.map(function (d) {
                return React.createElement("path", {
                    key: d.id,
                    stroke: "black",
                    fill: "none",
                    d: topDistributionLine(d.recall_distribution)
                });
            }),

            // Right distribution
            props.highlighted.map(function (d) {
                return React.createElement("path", {
                    key: d.id,
                    stroke: "black",
                    fill: "none",
                    d: rightDistributionLine(d.qps_distribution)
                });
            }),
            React.createElement(BottomAxis, {
                scale: props.scales.xMajor,
                spacing: props.spacing
            }),
            React.createElement(LeftAxis, {
                scale: props.scales.yMajor,
                spacing: props.spacing
            })
        )
    );
}

var App = function (_React$Component2) {
    _inherits(App, _React$Component2);

    function App(props) {
        _classCallCheck(this, App);

        var _this2 = _possibleConstructorReturn(this, (App.__proto__ || Object.getPrototypeOf(App)).call(this, props));

        _this2.state = {
            data: [],
            highlighted: []
        };

        _this2.timerID = -1;

        _this2.handleDataChange = _this2.handleDataChange.bind(_this2);
        _this2.handleHighlighted = _this2.handleHighlighted.bind(_this2);
        return _this2;
    }

    _createClass(App, [{
        key: "handleDataChange",
        value: function handleDataChange(fname) {
            var _this3 = this;

            console.log("Loading data from " + fname);
            d3.json(fname).then(function (data) {
                _this3.setState({
                    data: data,
                    highlighted: [data[0]]
                }, _this3.startTicking);
            });
        }
    }, {
        key: "handleHighlighted",
        value: function handleHighlighted(point) {
            this.stopTicking();
            this.setState({ highlighted: [point] });
        }
    }, {
        key: "tick",
        value: function tick() {
            this.setState(function (state, _props) {
                if (state.highlighted.length > 0) {
                    var curIdx = state.highlighted[0].id - 1;
                    var nextIdx = (curIdx + 1) % state.data.length;
                    return {
                        highlighted: [state.data[nextIdx]]
                    };
                }
            });
        }
    }, {
        key: "startTicking",
        value: function startTicking() {
            var _this4 = this;

            if (this.timerID < 0) {
                this.timerID = setInterval(function () {
                    return _this4.tick();
                }, 1000);
            }
        }
    }, {
        key: "stopTicking",
        value: function stopTicking() {
            clearInterval(this.timerID);
            this.timerID = -1;
        }
    }, {
        key: "componentDidMount",
        value: function componentDidMount() {
            this.startTicking();
        }
    }, {
        key: "render",
        value: function render() {
            var spacing = {
                width: 500,
                height: 400,
                top: 100,
                right: 100,
                bottom: 50,
                left: 50,
                margin: 5
            };

            var scales = {
                xMajor: d3.scaleLinear().domain([0, 1]).range([spacing.left, spacing.width - spacing.right - spacing.margin]).nice(),
                xMinor: d3.scaleLinear().domain([d3.min(this.state.data, function (d) {
                    return d3.min(d.qps_distribution, function (l) {
                        return l.y;
                    });
                }), d3.max(this.state.data, function (d) {
                    return d3.max(d.qps_distribution, function (l) {
                        return l.y;
                    });
                })]).range([spacing.width - spacing.right, spacing.width - spacing.margin]).nice(),
                yMajor: d3.scaleLog().domain([d3.min(this.state.data, function (d) {
                    return d3.min(d.qps_distribution, function (l) {
                        return l.x;
                    });
                }), d3.max(this.state.data, function (d) {
                    return d3.max(d.qps_distribution, function (l) {
                        return l.x;
                    });
                })]).range([spacing.height - spacing.bottom, spacing.top + spacing.margin]).nice(),
                yMinor: d3.scaleLinear().domain([d3.min(this.state.data, function (d) {
                    return d3.min(d.recall_distribution, function (l) {
                        return l.y;
                    });
                }), d3.max(this.state.data, function (d) {
                    return d3.max(d.recall_distribution, function (l) {
                        return l.y;
                    });
                })]).range([spacing.top, spacing.margin]).nice()
            };

            var form = React.createElement(DataForm, {
                datasets: datasets,
                difficulties: difficulties,
                difficultyTypes: difficultyTypes,
                algorithms: algorithms,
                callback: this.handleDataChange
            });

            var plot = React.createElement(Plot, {
                data: this.state.data,
                highlighted: this.state.highlighted,
                spacing: spacing,
                scales: scales,
                highlightedCallback: this.handleHighlighted
            });

            return React.createElement(
                "div",
                null,
                form,
                plot
            );
        }
    }]);

    return App;
}(React.Component);

/////////////////////////////////////////////////////////////////////
// App setup

var container = document.querySelector('#viz-container');

ReactDOM.render(React.createElement(App, null), container);