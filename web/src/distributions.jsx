'use strict';

const datasets = [
    "GLOVE",
    "GLOVE-2M",
    "GNEWS",
    "SIFT",
    "Fashion-MNIST",
    "MNIST"
];

const difficulties = [
    "hard",
    "diverse",
    "middle",
    "easy"
];

const difficultyTypes = [
    "lid", "lrc", "expansion"
];

const algorithms = [
    "Annoy",
    "PUFFINN",
    "ONNG",
    "HNSW",
    "IVF"
];

const kValues = [10];

class DataForm extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            dataset: props.datasets[0],
            difficulty: props.difficulties[0],
            difficultyType: props.difficultyTypes[0],
            algorithm: props.algorithms[0],
            k: props.kValues[0]
        };

        this.handleDataset = this.handleDataset.bind(this);
        this.handleDifficulty = this.handleDifficulty.bind(this);
        this.handleDifficultyType = this.handleDifficultyType.bind(this);
        this.handleAlgorithm = this.handleAlgorithm.bind(this);
        this.handleK = this.handleK.bind(this);
    }

    componentDidMount() {
        this.callback();
    }

    callback() {
        const fname = "data/" + [
            this.state.algorithm,
            this.state.dataset,
            this.state.difficulty,
            this.state.difficultyType,
            this.state.k,
            "json"
        ].join(".");
        this.props.callback(fname);
    }

    handleDataset(event) {
        this.setState({ dataset: event.target.value }, this.callback);
    }

    handleDifficulty(event) {
        this.setState({ difficulty: event.target.value }, this.callback);
    }

    handleDifficultyType(event) {
        this.setState({ difficultyType: event.target.value }, this.callback);
    }

    handleAlgorithm(event) {
        this.setState({ algorithm: event.target.value }, this.callback);
    }

    handleK(event) {
        this.setState({ k: event.target.value }, this.callback);
    }

    render() {
        return (
            <div className="flex flex-col space-y-3">
                <label>
                    Dataset:
                    <select value={this.state.dataset} onChange={this.handleDataset}>
                        {
                            this.props.datasets.map(d => <option key={d} value={d}>
                                {d}
                            </option>)
                        }
                    </select>
                </label>
                <label>
                    Difficulty:
                    <select value={this.state.difficulty} onChange={this.handleDifficulty}>
                        {
                            this.props.difficulties.map(d => <option key={d} value={d}>
                                {d}
                            </option>)
                        }
                    </select>
                </label>
                <label>
                    Difficulty type:
                    <select value={this.state.difficultyType} onChange={this.handleDifficultyType}>
                        {
                            this.props.difficultyTypes.map(d => <option key={d} value={d}>
                                {d}
                            </option>)
                        }
                    </select>
                </label>
                <label>
                    Algorithm
                    <select value={this.state.algorithm} onChange={this.handleAlgorithm}>
                        {
                            this.props.algorithms.map(d => <option key={d} value={d}>
                                {d}
                            </option>)
                        }
                    </select>
                </label>
                <label>
                    K
                    <select value={this.state.handleK} onChange={this.handleK}>
                        {
                            this.props.kValues.map(d => <option key={d} value={d}>
                                {d}
                            </option>)
                        }
                    </select>
                </label>
            </div>
        );
    }
}

function GridLines(props) {
    const xLines = props.scales.xMajor
        .ticks()
        .map((t, i) => {
            const xVal = props.scales.xMajor(t);
            const yStart = props.scales.yMajor.range()[0];
            const yEnd = props.scales.yMajor.range()[1];
            return <path
                key={i}
                stroke={"lightgray"}
                strokeDasharray={"2,2"}
                d={`M ${xVal} ${yStart} V ${yEnd}`}
            />;
        });
    const yLines = props.scales.yMajor
        .ticks()
        .map((t, i) => {
            const yVal = props.scales.yMajor(t);
            const xStart = props.scales.xMajor.range()[0];
            const xEnd = props.scales.xMajor.range()[1];
            return <path
                key={i}
                stroke={"lightgray"}
                strokeDasharray={"2,2"}
                d={`M ${xStart} ${yVal} H ${xEnd}`}
            />;
        });
    return <g>
        {xLines}
        {yLines}
    </g>;
}

function LeftAxis(props) {

    const ticks = props.scale.ticks()
        .filter(value => [1, 10, 100, 1000, 10000, 100000].indexOf(value) >= 0)
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
    </g>;
}


function Plot(props) {
    // Scale the distributions curves so that the peak has always the same height.
    // Therefore they no longer have area 1, but are more readable.
    const yTopScale = (props.highlighted.length > 0) ? props.scales.yMinor.copy()
        .domain(d3.extent(props.highlighted[0].recall_distribution.map(d => d.y))) : props.scales.yMinor;
    const xRightScale = (props.highlighted.length > 0) ? props.scales.xMinor.copy()
        .domain(d3.extent(props.highlighted[0].qps_distribution.map(d => d.y))) : props.scales.xMinor;

    const topDistributionLine = d3.line()
        .x(d => props.scales.xMajor(d.x))
        .y(d => yTopScale(d.y));
    const rightDistributionLine = d3.line()
        .x(d => xRightScale(d.y))
        .y(d => props.scales.yMajor(d.x));

    return <div className="w-full">
        <svg viewBox={`0 0 ${props.spacing.width} ${props.spacing.height}`}>
            <GridLines scales={props.scales} />
            {
                // Highlighted circle
                props.highlighted.map(d => {
                    return <circle
                        key={d.id}
                        cx={props.scales.xMajor(d.recall)}
                        cy={props.scales.yMajor(d.qps)}
                        r={4}
                        fill={"hotpink"}
                    />;
                })
            }
            {
                // Circles
                props.data.map(d => {
                    return <circle
                        key={d.id}
                        cx={props.scales.xMajor(d.recall)}
                        cy={props.scales.yMajor(d.qps)}
                        r={2}
                        onMouseOver={(e) => props.highlightedCallback(d)}
                    />;
                })
            }
            {
                // Top distribution
                props.highlighted.map(d => {
                    return <path
                        key={d.id}
                        stroke={"black"}
                        fill={"none"}
                        d={topDistributionLine(d.recall_distribution)}
                    />
                })
            }
            {
                // Right distribution
                props.highlighted.map(d => {
                    return <path
                        key={d.id}
                        stroke={"black"}
                        fill={"none"}
                        d={rightDistributionLine(d.qps_distribution)}
                    />
                })
            }
            <BottomAxis
                scale={props.scales.xMajor}
                spacing={props.spacing}
            />
            <LeftAxis
                scale={props.scales.yMajor}
                spacing={props.spacing}
            />
        </svg>
    </div>
}

class App extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            data: [],
            highlighted: []
        };

        this.timerID = -1;

        this.handleDataChange = this.handleDataChange.bind(this);
        this.handleHighlighted = this.handleHighlighted.bind(this);
    }

    handleDataChange(fname) {
        console.log("Loading data from " + fname);
        d3.json(fname).then((data) => {
            this.setState({
                data: data,
                highlighted: [data[0]]
            }, this.startTicking);
        });
    }

    handleHighlighted(point) {
        this.stopTicking();
        this.setState({ highlighted: [point] });
    }

    tick() {
        this.setState((state, _props) => {
            if (state.highlighted.length > 0) {
                const curIdx = state.highlighted[0].id - 1;
                const nextIdx = (curIdx + 1) % state.data.length;
                return {
                    highlighted: [state.data[nextIdx]]
                };
            }
        });
    }

    startTicking() {
        if (this.timerID < 0) {
            this.timerID = setInterval(
                () => this.tick(),
                1000
            );
        }
    }

    stopTicking() {
        clearInterval(this.timerID);
        this.timerID = -1;
    }

    componentDidMount() {
        this.startTicking();
    }

    render() {
        const spacing = {
            width: 500,
            height: 400,
            top: 100,
            right: 100,
            bottom: 50,
            left: 50,
            margin: 5
        };

        const scales = {
            xMajor: d3.scaleLinear()
                .domain([0, 1])
                .range([spacing.left, spacing.width - spacing.right - spacing.margin])
                .nice(),
            xMinor: d3.scaleLinear()
                .domain([
                    d3.min(this.state.data, d => d3.min(d.qps_distribution, l => l.y)),
                    d3.max(this.state.data, d => d3.max(d.qps_distribution, l => l.y)),
                ])
                .range([spacing.width - spacing.right, spacing.width - spacing.margin])
                .nice(),
            yMajor: d3.scaleLog()
                .domain([
                    d3.min(this.state.data, d => d3.min(d.qps_distribution, l => l.x)),
                    d3.max(this.state.data, d => d3.max(d.qps_distribution, l => l.x)),
                ]).range([spacing.height - spacing.bottom, spacing.top + spacing.margin])
                .nice(),
            yMinor: d3.scaleLinear()
                .domain([
                    d3.min(this.state.data, d => d3.min(d.recall_distribution, l => l.y)),
                    d3.max(this.state.data, d => d3.max(d.recall_distribution, l => l.y)),
                ])
                .range([spacing.top, spacing.margin])
                .nice(),
        };

        const form = <DataForm
            datasets={datasets}
            difficulties={difficulties}
            difficultyTypes={difficultyTypes}
            algorithms={algorithms}
            kValues={kValues}
            callback={this.handleDataChange}
        />;

        const plot = <Plot
            data={this.state.data}
            highlighted={this.state.highlighted}
            spacing={spacing}
            scales={scales}
            highlightedCallback={this.handleHighlighted}
        />

        return <div>
            {form}
            {plot}
        </div>;
    }
}

/////////////////////////////////////////////////////////////////////
// App setup

const container = document.querySelector('#viz-container');

ReactDOM.render(<App />, container);

